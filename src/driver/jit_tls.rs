//! Thread-locals in JIT code. Codegen gives each thread-local a key and uses [`get`] for every
//! access.

use std::alloc::{Layout, alloc, dealloc, handle_alloc_error};
use std::cell::Cell;
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

/// Frees the thread's table when dropped. Registered as the table is created, which is before the
/// program can register a destructor for any copy in it, and glibc runs thread-local destructors
/// newest first, so the program's run while the table is still there.
struct FreeTable;

impl Drop for FreeTable {
    fn drop(&mut self) {
        let table = TABLE.replace(ptr::null_mut());
        // SAFETY: `FreeTable` is registered only when `table` creates a table from `Box`, and
        // nulling `TABLE` first means nothing reaches it again.
        drop(unsafe { Box::from_raw(table) });
    }
}

thread_local! {
    static TABLE: Cell<*mut Table> = const { Cell::new(ptr::null_mut()) };
    static FREE_TABLE: FreeTable = const { FreeTable };
}

/// The calling thread's copy of the thread-local behind `key`. Its address is fixed for the
/// thread's life.
pub(super) extern "C" fn get(key: usize) -> *mut u8 {
    // SAFETY: `TABLE` is null or this thread's live table, and nothing else holds a reference to
    // it during the call.
    if let Some(table) = unsafe { TABLE.get().as_ref() }
        && let Some(&Some(copy)) = table.copies.get(key)
    {
        return copy.as_ptr();
    }
    first_access(key)
}

#[cold]
fn first_access(key: usize) -> *mut u8 {
    let table = table();
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

fn table() -> &'static mut Table {
    let mut table = TABLE.get();
    if table.is_null() {
        table = Box::into_raw(Box::new(Table { copies: Vec::new(), owned: Vec::new() }));
        TABLE.set(table);
        // Fails only when reached from a destructor that ran after `FreeTable`'s. The new table
        // then outlives the thread.
        let _ = FREE_TABLE.try_with(|_| ());
    }
    // SAFETY: the table lives until this thread's `FreeTable` runs, and only `get` and
    // `first_access` reach it, neither while another reference to it is live. Neither calls back
    // into JIT code: `resolve` only calls `dlsym`.
    unsafe { &mut *table }
}
