//! Thread-locals in JIT code. Codegen gives each thread-local a key and uses [`get`] for every
//! access.

use std::alloc::{Layout, alloc, dealloc, handle_alloc_error};
use std::ptr::{self, NonNull};
use std::sync::{Arc, RwLock};

use rustc_data_structures::fx::FxHashSet;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;

use super::jit::SymbolLookup;
use crate::prelude::*;

pub(super) enum ThreadLocalSource {
    /// Defined in JIT code: each thread's copy starts as a copy of `bytes`, aligned to `align`.
    Image { bytes: &'static [u8], align: usize },
    /// Defined in a dylib: returns the calling thread's copy, the one the dylib's own code uses.
    Dylib(Box<dyn Fn() -> *mut u8 + Send + Sync>),
}

pub(super) fn is_thread_local(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    tcx.codegen_fn_attrs(def_id).flags.contains(CodegenFnAttrFlags::THREAD_LOCAL)
}

pub(super) struct NewThreadLocal {
    key: usize,
    symbol: String,
    /// The initial bytes' layout, for one JIT code defines. `None` for a dylib's.
    image: Option<Layout>,
}

/// Sorts newly keyed thread-locals, as `(key, symbol, def id)`, into those whose `images` this
/// build defines and those in dylibs, refusing any that no dylib exports.
pub(super) fn classify_new_thread_locals<'a>(
    tcx: TyCtxt<'_>,
    keys: impl IntoIterator<Item = (usize, &'a str, DefId)>,
    images: &FxHashSet<DefId>,
    dylibs: &SymbolLookup,
) -> Vec<NewThreadLocal> {
    let mut new = Vec::new();
    for (key, symbol, def_id) in keys {
        let image = if images.contains(&def_id) {
            let alloc = tcx.eval_static_initializer(def_id).unwrap();
            let align = alloc.inner().align.bytes() as usize;
            Some(Layout::from_size_align(alloc.inner().len(), align).unwrap())
        } else {
            if dylibs(symbol).is_none() {
                tcx.dcx().span_err(
                    tcx.def_span(def_id),
                    format!(
                        "thread-local `{}` is not exported by any dylib",
                        tcx.def_path_str(def_id)
                    ),
                );
            }
            None
        };
        new.push(NewThreadLocal { key, symbol: symbol.to_owned(), image });
    }
    new
}

/// A classified thread-local's key and source, once its build is finalised. `image_address` gives
/// the finalised address of a thread-local's initial bytes by symbol.
pub(super) fn source(
    new: NewThreadLocal,
    image_address: impl Fn(&str) -> usize,
    dylibs: &Arc<SymbolLookup>,
) -> (usize, ThreadLocalSource) {
    let NewThreadLocal { key, symbol, image } = new;
    let source = match image {
        Some(layout) => {
            let image = image_address(&symbol) as *const u8;
            // SAFETY: the image is JIT data of `layout.size()` initialised bytes, never written or
            // freed once its build is finalised.
            let bytes = unsafe { std::slice::from_raw_parts(image, layout.size()) };
            ThreadLocalSource::Image { bytes, align: layout.align() }
        }
        None => {
            let dylibs = dylibs.clone();
            ThreadLocalSource::Dylib(Box::new(move || {
                // Found when the build was classified, and dylibs are never unloaded.
                let found = (*dylibs)(&symbol).unwrap();
                thread_address(found)
            }))
        }
    };
    (key, source)
}

/// glibc's `dlsym` resolves a TLS symbol to the calling thread's copy.
#[cfg(not(target_vendor = "apple"))]
fn thread_address(found: *const u8) -> *mut u8 {
    found.cast_mut()
}

/// dyld's `dlsym` returns the thread-local's descriptor, whose thunk gives the calling thread's
/// copy.
#[cfg(target_vendor = "apple")]
fn thread_address(found: *const u8) -> *mut u8 {
    #[repr(C)]
    struct TlvDescriptor {
        thunk: unsafe extern "C" fn(*const TlvDescriptor) -> *mut u8,
        key: usize,
        offset: usize,
    }
    let descriptor = found.cast::<TlvDescriptor>();
    // SAFETY: `dlsym` returned a TLS symbol, which dyld represents by a live descriptor of this
    // layout; its thunk takes the descriptor itself.
    unsafe { ((*descriptor).thunk)(descriptor) }
}

/// Indexed by key.
static SOURCES: RwLock<Vec<ThreadLocalSource>> = RwLock::new(Vec::new());

/// Keys are defined once each, in increasing order from 0, before any code using them can run.
pub(super) fn define(key: usize, source: ThreadLocalSource) {
    let mut sources = SOURCES.write().unwrap();
    assert_eq!(sources.len(), key, "thread-local keys are defined in order");
    sources.push(source);
}

struct Table {
    /// Indexed by key; `None` until the thread first reaches that thread-local.
    copies: Vec<Option<NonNull<u8>>>,
    /// The copies this thread allocated, as opposed to a dylib's own.
    owned: Vec<OwnedCopy>,
    /// Set by the first round of pthread key destructors; the table is freed on the next.
    #[cfg(unix)]
    deferred: bool,
}

struct OwnedCopy {
    ptr: NonNull<u8>,
    layout: Layout,
}

impl Drop for OwnedCopy {
    fn drop(&mut self) {
        // SAFETY: `first_access` allocated `ptr` with exactly this layout.
        unsafe { dealloc(self.ptr.as_ptr(), self.layout) };
    }
}

/// Each thread's table hangs off a pthread key and is freed on the second round of key
/// destructors. Both copies of std, the compiler's and the program's, normally run their
/// thread-local destructors by the first: glibc before any key, FreeBSD from a key, macOS from
/// dyld's `_terminatorsKey` destructor.
#[cfg(unix)]
mod slot {
    use std::ffi::c_void;
    use std::sync::OnceLock;

    use super::Table;

    pub(super) fn current() -> *mut Table {
        // SAFETY: `key` is a valid key.
        unsafe { libc::pthread_getspecific(key()) }.cast()
    }

    pub(super) fn install(table: *mut Table) {
        // SAFETY: `key` is a valid key, and `free_table` takes back ownership of `table`.
        assert_eq!(unsafe { libc::pthread_setspecific(key(), table.cast()) }, 0);
    }

    fn key() -> libc::pthread_key_t {
        static KEY: OnceLock<libc::pthread_key_t> = OnceLock::new();
        *KEY.get_or_init(|| {
            let mut key = 0;
            // SAFETY: `key` is a valid place for the new key.
            assert_eq!(unsafe { libc::pthread_key_create(&mut key, Some(free_table)) }, 0);
            key
        })
    }

    unsafe extern "C" fn free_table(table: *mut c_void) {
        let table = table.cast::<Table>();
        // SAFETY: the key only ever holds this thread's live table, set by `install`.
        let live = unsafe { &mut *table };
        if !live.deferred {
            live.deferred = true;
            // SAFETY: as above. A failure leaks the table rather than freeing it early.
            unsafe { libc::pthread_setspecific(key(), table.cast()) };
            return;
        }
        // The key is already null, so a later access starts a fresh table.
        // SAFETY: `install` took the table from `Box`, and nothing else points at it.
        drop(unsafe { Box::from_raw(table) });
    }
}

/// Off Unix each thread's table is never freed.
#[cfg(not(unix))]
mod slot {
    use std::cell::Cell;
    use std::ptr;

    use super::Table;

    thread_local! {
        static TABLE: Cell<*mut Table> = const { Cell::new(ptr::null_mut()) };
    }

    pub(super) fn current() -> *mut Table {
        TABLE.get()
    }

    pub(super) fn install(table: *mut Table) {
        TABLE.set(table);
    }
}

/// The calling thread's copy of the thread-local behind `key`. Its address is fixed for the
/// thread's life.
pub(super) extern "C" fn get(key: usize) -> *mut u8 {
    // SAFETY: `slot::current` is null or this thread's live table, and nothing else holds a
    // reference to it during the call.
    if let Some(table) = unsafe { slot::current().as_ref() }
        && let Some(&Some(copy)) = table.copies.get(key)
    {
        return copy.as_ptr();
    }
    first_access(key)
}

#[cold]
fn first_access(key: usize) -> *mut u8 {
    let mut table = table();
    // SAFETY: the table lives at least until thread exit, and only `get` and `first_access` reach
    // it, neither while another reference to it is live. Neither calls back into JIT code:
    // `resolve` only calls `dlsym`.
    let table = unsafe { table.as_mut() };
    let sources = SOURCES.read().unwrap();

    let copy = match sources[key] {
        ThreadLocalSource::Image { bytes, align } => {
            // `alloc` rejects zero sizes.
            let layout = Layout::from_size_align(bytes.len().max(1), align).unwrap();
            // SAFETY: `layout` has a non-zero size.
            let copy = NonNull::new(unsafe { alloc(layout) })
                .unwrap_or_else(|| handle_alloc_error(layout));
            // SAFETY: `copy` is a fresh allocation of at least `bytes.len()` bytes.
            unsafe { ptr::copy_nonoverlapping(bytes.as_ptr(), copy.as_ptr(), bytes.len()) };
            table.owned.push(OwnedCopy { ptr: copy, layout });
            copy.as_ptr()
        }
        ThreadLocalSource::Dylib(ref resolve) => resolve(),
    };

    if table.copies.len() <= key {
        table.copies.resize(key + 1, None);
    }
    table.copies[key] = NonNull::new(copy);

    copy
}

fn table() -> NonNull<Table> {
    if let Some(table) = NonNull::new(slot::current()) {
        return table;
    }
    let new = Table {
        copies: Vec::new(),
        owned: Vec::new(),
        #[cfg(unix)]
        deferred: false,
    };
    let table = NonNull::from(Box::leak(Box::new(new)));
    slot::install(table.as_ptr());
    table
}
