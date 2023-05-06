#![feature(
    no_core,
    lang_items,
    never_type,
    linkage,
    extern_types,
    naked_functions,
    thread_local,
    repr_simd,
    intrinsics,
    rustc_attrs,
    transparent_unions,
    freeze_impls,
    auto_traits
)]
#![no_core]
#![allow(dead_code, non_camel_case_types, internal_features)]
#![no_main]

use self::libc::*;

struct NoisyDrop;

impl Drop for NoisyDrop {
    fn drop(&mut self) {
        unsafe {
            puts("Inner got dropped!\0" as *const str as *const i8);
        }
    }
}

/// Invoke a closure, capturing the cause of an unwinding panic if one occurs.
pub unsafe fn catch_unwind<R>(f: fn() -> R) {
    let data_ptr = f as *mut u8;
    unsafe {
        intrinsics::catch_unwind(do_call::<R>, data_ptr, do_catch);
    }

    fn do_call<R>(data: *mut u8) {
        unsafe {
            intrinsics::transmute::<_, fn() -> R>(data)();
        }
    }

    fn do_catch(_data: *mut u8, _payload: *mut u8) {
        unsafe {
            puts("Caught exception!\0" as *const str as *const i8);
        }
    }
}

type _Unwind_Exception_Class = u64;
type _Unwind_Word = usize;
type _Unwind_Ptr = usize;

#[link(name = "gcc_s")]
extern "C-unwind" {
    fn _Unwind_RaiseException(exception: *mut _Unwind_Exception) -> u8;
    fn _Unwind_Resume(exception: *mut _Unwind_Exception) -> !;
}

extern "C" {
    fn _Unwind_DeleteException(exception: *mut _Unwind_Exception);
    fn _Unwind_GetLanguageSpecificData(ctx: *mut _Unwind_Context) -> *mut ();
    fn _Unwind_GetRegionStart(ctx: *mut _Unwind_Context) -> _Unwind_Ptr;
    fn _Unwind_GetTextRelBase(ctx: *mut _Unwind_Context) -> _Unwind_Ptr;
    fn _Unwind_GetDataRelBase(ctx: *mut _Unwind_Context) -> _Unwind_Ptr;

    fn _Unwind_GetGR(ctx: *mut _Unwind_Context, reg_index: i32) -> _Unwind_Word;
    fn _Unwind_SetGR(ctx: *mut _Unwind_Context, reg_index: i32, value: _Unwind_Word);
    fn _Unwind_GetIP(ctx: *mut _Unwind_Context) -> _Unwind_Word;
    fn _Unwind_SetIP(ctx: *mut _Unwind_Context, value: _Unwind_Word);
    fn _Unwind_GetIPInfo(ctx: *mut _Unwind_Context, ip_before_insn: *mut i32) -> _Unwind_Word;
    fn _Unwind_FindEnclosingFunction(pc: *mut ()) -> *mut ();
}

#[repr(C)]
struct _Unwind_Exception {
    _exception_class: u64,
    _exception_cleanup: extern "C" fn(unwind_code: u64, exception: *mut _Unwind_Exception),
    _private: [usize; 2],
}

extern "C" fn cleanup(_: u64, _: *mut _Unwind_Exception) {}

static mut EXC: _Unwind_Exception =
    _Unwind_Exception { _exception_class: 0u64, _exception_cleanup: cleanup, _private: [0; 2] };

#[no_mangle]
fn main(_argc: isize, _argv: *const *const u8) {
    unsafe {
        fn f() {
            unsafe {
                let _noise = NoisyDrop;

                printf(
                    "failed to raise exception: %d\n\0" as *const str as *const i8,
                    _Unwind_RaiseException(&mut EXC) as i32,
                );
                panic("foo");
            }
        }
        catch_unwind(f);
    }
}

#[repr(C)]
pub enum _Unwind_Reason_Code {
    _URC_NO_REASON = 0,
    _URC_FOREIGN_EXCEPTION_CAUGHT = 1,
    _URC_FATAL_PHASE2_ERROR = 2,
    _URC_FATAL_PHASE1_ERROR = 3,
    _URC_NORMAL_STOP = 4,
    _URC_END_OF_STACK = 5,
    _URC_HANDLER_FOUND = 6,
    _URC_INSTALL_CONTEXT = 7,
    _URC_CONTINUE_UNWIND = 8,
    _URC_FAILURE = 9, // used only by ARM EHABI
}

pub enum _Unwind_Context {}

#[repr(C)]
pub enum _Unwind_Action {
    _UA_SEARCH_PHASE = 1,
    _UA_CLEANUP_PHASE = 2,
    _UA_HANDLER_FRAME = 4,
    _UA_FORCE_UNWIND = 8,
    _UA_END_OF_STACK = 16,
}

impl Copy for _Unwind_Action {}

#[lang = "eh_personality"]
unsafe extern "C" fn rust_eh_personality(
    version: i32,
    actions: _Unwind_Action,
    exception_class: _Unwind_Exception_Class,
    exception_object: *mut _Unwind_Exception,
    context: *mut _Unwind_Context,
) -> _Unwind_Reason_Code {
    // FIXME implement an actual personality function

    let ip = _Unwind_GetIP(context);
    let lsda = _Unwind_GetLanguageSpecificData(context);

    if actions as i32 & _Unwind_Action::_UA_SEARCH_PHASE as i32 != 0 {
        libc::printf(
            "personality for %p; lsda=%s; search\n\0" as *const str as *const i8,
            ip,
            lsda,
        );

        if strcmp(
            lsda as *const i8,
            "_ZN21mini_core_hello_world12catch_unwind17h349b550af0b8971cE\0" as *const str
                as *const i8,
        ) == 0
        {
            libc::puts("Catch function found!\0" as *const str as *const i8);
            _Unwind_Reason_Code::_URC_HANDLER_FOUND
        } else {
            _Unwind_Reason_Code::_URC_CONTINUE_UNWIND
        }
    } else if actions as i32 & _Unwind_Action::_UA_CLEANUP_PHASE as i32 != 0 {
        libc::printf(
            "personality for %p; lsda=%s; cleanup\n\0" as *const str as *const i8,
            ip,
            lsda,
        );

        if strcmp(
            lsda as *const i8,
            "_ZN21mini_core_hello_world12catch_unwind17h349b550af0b8971cE\0" as *const str
                as *const i8,
        ) == 0
        {
            libc::puts("Catch!\0" as *const str as *const i8);
            _Unwind_Reason_Code::_URC_FATAL_PHASE2_ERROR
        } else {
            _Unwind_Reason_Code::_URC_CONTINUE_UNWIND
        }
    } else {
        intrinsics::abort();
    }
}

#[lang = "sized"]
pub trait Sized {}

#[lang = "receiver"]
pub trait Receiver {}

impl<T: ?Sized> Receiver for &T {}
impl<T: ?Sized> Receiver for &mut T {}

#[lang = "copy"]
pub trait Copy {}

impl Copy for bool {}
impl Copy for u8 {}
impl Copy for u16 {}
impl Copy for u32 {}
impl Copy for u64 {}
impl Copy for u128 {}
impl Copy for usize {}
impl Copy for i8 {}
impl Copy for i16 {}
impl Copy for i32 {}
impl Copy for isize {}
impl Copy for f32 {}
impl Copy for f64 {}
impl Copy for char {}
impl<'a, T: ?Sized> Copy for &'a T {}
impl<T: ?Sized> Copy for *const T {}
impl<T: ?Sized> Copy for *mut T {}

#[lang = "sync"]
pub unsafe trait Sync {}

unsafe impl Sync for bool {}
unsafe impl Sync for u8 {}
unsafe impl Sync for u16 {}
unsafe impl Sync for u32 {}
unsafe impl Sync for u64 {}
unsafe impl Sync for usize {}
unsafe impl Sync for i8 {}
unsafe impl Sync for i16 {}
unsafe impl Sync for i32 {}
unsafe impl Sync for isize {}
unsafe impl Sync for char {}
unsafe impl<'a, T: ?Sized> Sync for &'a T {}
unsafe impl Sync for [u8; 16] {}

#[lang = "freeze"]
unsafe auto trait Freeze {}

unsafe impl<T: ?Sized> Freeze for PhantomData<T> {}
unsafe impl<T: ?Sized> Freeze for *const T {}
unsafe impl<T: ?Sized> Freeze for *mut T {}
unsafe impl<T: ?Sized> Freeze for &T {}
unsafe impl<T: ?Sized> Freeze for &mut T {}

#[lang = "structural_peq"]
pub trait StructuralPartialEq {}

#[lang = "destruct"]
pub trait Destruct {}

#[lang = "phantom_data"]
pub struct PhantomData<T: ?Sized>;

#[lang = "legacy_receiver"]
pub trait LegacyReceiver {}

impl<T: ?Sized> LegacyReceiver for &T {}
impl<T: ?Sized> LegacyReceiver for &mut T {}

#[lang = "panic"]
#[track_caller]
pub fn panic(_msg: &'static str) -> ! {
    unsafe {
        libc::puts("Panicking\n\0" as *const str as *const i8);
        intrinsics::abort();
    }
}

#[lang = "panic_cannot_unwind"]
fn panic_cannot_unwind() -> ! {
    unsafe {
        libc::puts("panic in a function that cannot unwind\n\0" as *const str as *const i8);
        intrinsics::abort();
    }
}

#[lang = "drop_in_place"]
#[allow(unconditional_recursion)]
pub unsafe fn drop_in_place<T: ?Sized>(to_drop: *mut T) {
    // Code here does not matter - this is replaced by the
    // real drop glue by the compiler.
    drop_in_place(to_drop);
}

#[lang = "deref"]
pub trait Deref {
    type Target: ?Sized;

    fn deref(&self) -> &Self::Target;
}

#[lang = "drop"]
pub trait Drop {
    fn drop(&mut self);
}

#[lang = "manually_drop"]
#[repr(transparent)]
pub struct ManuallyDrop<T: ?Sized> {
    pub value: T,
}

#[lang = "maybe_uninit"]
#[repr(transparent)]
pub union MaybeUninit<T> {
    pub uninit: (),
    pub value: ManuallyDrop<T>,
}

pub mod intrinsics {
    #[rustc_intrinsic]
    pub fn abort() -> !;
    #[rustc_intrinsic]
    pub fn size_of<T>() -> usize;
    #[rustc_intrinsic]
    pub unsafe fn size_of_val<T: ?::Sized>(val: *const T) -> usize;
    #[rustc_intrinsic]
    pub fn min_align_of<T>() -> usize;
    #[rustc_intrinsic]
    pub unsafe fn min_align_of_val<T: ?::Sized>(val: *const T) -> usize;
    #[rustc_intrinsic]
    pub unsafe fn copy<T>(src: *const T, dst: *mut T, count: usize);
    #[rustc_intrinsic]
    pub unsafe fn transmute<T, U>(e: T) -> U;
    #[rustc_intrinsic]
    pub unsafe fn ctlz_nonzero<T>(x: T) -> u32;
    #[rustc_intrinsic]
    pub fn needs_drop<T: ?::Sized>() -> bool;
    #[rustc_intrinsic]
    pub fn bitreverse<T>(x: T) -> T;
    #[rustc_intrinsic]
    pub fn bswap<T>(x: T) -> T;
    #[rustc_intrinsic]
    pub unsafe fn write_bytes<T>(dst: *mut T, val: u8, count: usize);
    #[rustc_intrinsic]
    pub unsafe fn unreachable() -> !;
    #[rustc_intrinsic]
    pub unsafe fn catch_unwind(
        try_fn: fn(_: *mut u8),
        data: *mut u8,
        catch_fn: fn(_: *mut u8, _: *mut u8),
    ) -> i32;
}

pub mod libc {
    // With the new Universal CRT, msvc has switched to all the printf functions being inline wrapper
    // functions. legacy_stdio_definitions.lib which provides the printf wrapper functions as normal
    // symbols to link against.
    #[cfg_attr(unix, link(name = "c"))]
    #[cfg_attr(target_env = "msvc", link(name = "legacy_stdio_definitions"))]
    extern "C" {
        pub fn printf(format: *const i8, ...) -> i32;
    }

    #[cfg_attr(unix, link(name = "c"))]
    #[cfg_attr(target_env = "msvc", link(name = "msvcrt"))]
    extern "C" {
        pub fn puts(s: *const i8) -> i32;
        pub fn malloc(size: usize) -> *mut u8;
        pub fn free(ptr: *mut u8);
        pub fn memcpy(dst: *mut u8, src: *const u8, size: usize);
        pub fn memmove(dst: *mut u8, src: *const u8, size: usize);
        pub fn strncpy(dst: *mut u8, src: *const u8, size: usize);
        pub fn strcmp(a: *const i8, b: *const i8) -> i32;
    }
}

extern "C" {
    type VaListImpl;
}

#[lang = "va_list"]
#[repr(transparent)]
pub struct VaList<'a>(&'a mut VaListImpl);

#[lang = "panic_location"]
struct PanicLocation {
    _file: &'static str,
    _line: u32,
    _column: u32,
}

#[lang = "eq"]
pub trait PartialEq<Rhs: ?Sized = Self> {
    fn eq(&self, other: &Rhs) -> bool;
    fn ne(&self, other: &Rhs) -> bool;
}

impl PartialEq for i32 {
    fn eq(&self, other: &i32) -> bool {
        (*self) == (*other)
    }
    fn ne(&self, other: &i32) -> bool {
        (*self) != (*other)
    }
}

#[lang = "bitand"]
pub trait BitAnd<RHS = Self> {
    type Output;

    #[must_use]
    fn bitand(self, rhs: RHS) -> Self::Output;
}

impl BitAnd for i32 {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        self & rhs
    }
}
