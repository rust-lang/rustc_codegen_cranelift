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

unsafe impl<T: ?Sized> Freeze for *const T {}
unsafe impl<T: ?Sized> Freeze for *mut T {}
unsafe impl<T: ?Sized> Freeze for &T {}
unsafe impl<T: ?Sized> Freeze for &mut T {}

#[lang = "structural_peq"]
pub trait StructuralPartialEq {}

#[lang = "structural_teq"]
pub trait StructuralEq {}

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
        pub fn ctlz_nonzero<T>(x: T) -> T;
        #[rustc_safe_intrinsic]
        pub fn needs_drop<T: ?::Sized>() -> bool;
        #[rustc_safe_intrinsic]
        pub fn bitreverse<T>(x: T) -> T;
        #[rustc_safe_intrinsic]
        pub fn bswap<T>(x: T) -> T;
        pub fn write_bytes<T>(dst: *mut T, val: u8, count: usize);
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

#[no_mangle]
fn main(foo: u32) -> u32 {
    add_1(foo) + add_1_and_2(foo).1 + "foo" as *const str as *const u8 as u32
}

fn add_1(foo: u32) -> u32 {
    foo + 1
}

fn add_1_and_2(foo: u32) -> (u32, u32) {
    (foo + 1, foo + 2)
}
