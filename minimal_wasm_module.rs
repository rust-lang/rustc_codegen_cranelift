#![feature(
    no_core,
    lang_items,
    intrinsics,
    unboxed_closures,
    extern_types,
    decl_macro,
    rustc_attrs,
    transparent_unions,
    auto_traits,
    thread_local
)]
#![no_core]
#![no_main]
#![allow(dead_code, internal_features, ambiguous_wide_pointer_comparisons)]

#[lang = "sized"]
pub trait Sized {}

#[lang = "destruct"]
pub trait Destruct {}

#[lang = "tuple_trait"]
pub trait Tuple {}

#[lang = "copy"]
pub unsafe trait Copy {}

unsafe impl Copy for bool {}
unsafe impl Copy for u8 {}
unsafe impl Copy for u16 {}
unsafe impl Copy for u32 {}
unsafe impl Copy for u64 {}
unsafe impl Copy for u128 {}
unsafe impl Copy for usize {}
unsafe impl Copy for i8 {}
unsafe impl Copy for i16 {}
unsafe impl Copy for i32 {}
unsafe impl Copy for isize {}
unsafe impl Copy for f32 {}
unsafe impl Copy for f64 {}
unsafe impl Copy for char {}
unsafe impl<'a, T: ?Sized> Copy for &'a T {}
unsafe impl<T: ?Sized> Copy for *const T {}
unsafe impl<T: ?Sized> Copy for *mut T {}

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

#[lang = "structural_peq"]
pub trait StructuralPartialEq {}

#[lang = "not"]
pub trait Not {
    type Output;

    fn not(self) -> Self::Output;
}

impl Not for bool {
    type Output = bool;

    fn not(self) -> bool {
        !self
    }
}

#[lang = "mul"]
pub trait Mul<RHS = Self> {
    type Output;

    #[must_use]
    fn mul(self, rhs: RHS) -> Self::Output;
}

impl Mul for u8 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        self * rhs
    }
}

impl Mul for usize {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        self * rhs
    }
}

#[lang = "add"]
pub trait Add<RHS = Self> {
    type Output;

    fn add(self, rhs: RHS) -> Self::Output;
}

impl Add for u8 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        self + rhs
    }
}

impl Add for i8 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        self + rhs
    }
}

impl Add for u32 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        self + rhs
    }
}

#[lang = "fn_once"]
#[rustc_paren_sugar]
pub trait FnOnce<Args: Tuple> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

#[lang = "fn_mut"]
#[rustc_paren_sugar]
pub trait FnMut<Args: Tuple>: FnOnce<Args> {
    extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output;
}

#[lang = "drop_in_place"]
#[allow(unconditional_recursion)]
pub unsafe fn drop_in_place<T: ?Sized>(to_drop: *mut T) {
    // Code here does not matter - this is replaced by the
    // real drop glue by the compiler.
    drop_in_place(to_drop);
}

#[lang = "drop"]
pub trait Drop {
    fn drop(&mut self);
}

pub mod intrinsics {
    extern "rust-intrinsic" {
        #[rustc_safe_intrinsic]
        pub fn abort() -> !;
        #[rustc_safe_intrinsic]
        pub fn size_of<T>() -> usize;
        pub fn size_of_val<T: ?::Sized>(val: *const T) -> usize;
        #[rustc_safe_intrinsic]
        pub fn min_align_of<T>() -> usize;
        pub fn min_align_of_val<T: ?::Sized>(val: *const T) -> usize;
        pub fn copy<T>(src: *const T, dst: *mut T, count: usize);
        pub fn transmute<T, U>(e: T) -> U;
        pub fn ctlz_nonzero<T>(x: T) -> u32;
        #[rustc_safe_intrinsic]
        pub fn needs_drop<T: ?::Sized>() -> bool;
        #[rustc_safe_intrinsic]
        pub fn bitreverse<T>(x: T) -> T;
        #[rustc_safe_intrinsic]
        pub fn bswap<T>(x: T) -> T;
        pub fn write_bytes<T>(dst: *mut T, val: u8, count: usize);
        #[rustc_safe_intrinsic]
        pub fn caller_location() -> &'static crate::Location<'static>;
    }
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
    }
}

#[lang = "receiver"]
pub trait Receiver {}

impl<T: ?Sized> Receiver for &T {}
impl<T: ?Sized> Receiver for &mut T {}

#[lang = "eq"]
pub trait PartialEq<Rhs: ?Sized = Self> {
    fn eq(&self, other: &Rhs) -> bool;
    fn ne(&self, other: &Rhs) -> bool;
}

impl PartialEq for u8 {
    fn eq(&self, other: &u8) -> bool {
        (*self) == (*other)
    }
    fn ne(&self, other: &u8) -> bool {
        (*self) != (*other)
    }
}

impl PartialEq for u16 {
    fn eq(&self, other: &u16) -> bool {
        (*self) == (*other)
    }
    fn ne(&self, other: &u16) -> bool {
        (*self) != (*other)
    }
}

impl PartialEq for u32 {
    fn eq(&self, other: &u32) -> bool {
        (*self) == (*other)
    }
    fn ne(&self, other: &u32) -> bool {
        (*self) != (*other)
    }
}

#[repr(C)]
struct Ciovec {
    buf: *const u8,
    buf_len: u32,
}

#[link(wasm_import_module = "wasi_snapshot_preview1")]
extern "C" {
    fn args_get(argv: *mut *mut u8, argv_buf: *mut u8) -> i32;
    fn args_sizes_get(argc: *mut u32, argv_size: *mut u32) -> i32;
    fn fd_write(fd: i32, iovs_ptr: *const Ciovec, iovs_len: i32, rp0: *mut u32) -> i32;
}

#[lang = "panic"]
#[track_caller]
pub fn panic(msg: &'static str) -> ! {
    let ciovec = Ciovec { buf: "Panicking at " as *const str as *const u8, buf_len: 13 };
    unsafe {
        fd_write(2, &ciovec, 1, &mut 0);
    }
    let caller = intrinsics::caller_location();
    let ciovec =
        Ciovec { buf: caller.file as *const str as *const u8, buf_len: 24 /* FIXME */ };
    unsafe {
        fd_write(2, &ciovec, 1, &mut 0);
    }
    let ciovec = Ciovec { buf: ": \n" as *const str as *const u8, buf_len: 3 };
    unsafe {
        fd_write(2, &ciovec, 1, &mut 0);
    }
    let ciovec = Ciovec { buf: msg as *const str as *const u8, buf_len: 4 /* FIXME */ };
    unsafe {
        fd_write(2, &ciovec, 1, &mut 0);
    }
    let ciovec = Ciovec { buf: "\n" as *const str as *const u8, buf_len: 1 };
    unsafe {
        fd_write(2, &ciovec, 1, &mut 0);
    }
    intrinsics::abort();
}

macro_rules! panic_const {
    ($($lang:ident = $message:expr,)+) => {
        pub mod panic_const {
            use super::*;

            $(
                #[track_caller]
                #[lang = stringify!($lang)]
                pub fn $lang() -> ! {
                    panic($message);
                }
            )+
        }
    }
}

panic_const! {
    panic_const_add_overflow = "attempt to add with overflow",
    panic_const_sub_overflow = "attempt to subtract with overflow",
    panic_const_mul_overflow = "attempt to multiply with overflow",
    panic_const_div_overflow = "attempt to divide with overflow",
    panic_const_rem_overflow = "attempt to calculate the remainder with overflow",
    panic_const_neg_overflow = "attempt to negate with overflow",
    panic_const_shr_overflow = "attempt to shift right with overflow",
    panic_const_shl_overflow = "attempt to shift left with overflow",
    panic_const_div_by_zero = "attempt to divide by zero",
    panic_const_rem_by_zero = "attempt to calculate the remainder with a divisor of zero",
}

#[rustc_builtin_macro]
#[rustc_macro_transparency = "semitransparent"]
pub macro stringify($($t:tt)*) {
    /* compiler built-in */
}

#[lang = "panic_location"]
struct Location<'a> {
    file: &'a str,
    line: u32,
    column: u32,
}

#[no_mangle]
fn _start() {
    let ciovec = Ciovec { buf: "foo\n" as *const str as *const u8, buf_len: 4 };
    unsafe {
        fd_write(2, &ciovec, 1, &mut 0);
    }

    let mut argc = 0;
    let mut argv_size = 0;
    unsafe {
        args_sizes_get(&mut argc, &mut argv_size);
    }

    #[allow(arithmetic_overflow)]
    {
        0xffffffffu32 + 1u32;
    }

    if argc != 2 {
        //intrinsics::abort();
    }

    if add_1(argc) + add_1_and_2(argc).1 + argc != 9 {
        panic("oops");
    }
}

fn add_1(foo: u32) -> u32 {
    foo + 1
}

fn add_1_and_2(foo: u32) -> (u32, u32) {
    (foo + 1, foo + 2)
}
