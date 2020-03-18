// Adapted from https://github.com/sunfishcode/mir2cranelift/blob/master/rust-examples/nocore-hello-world.rs

#![feature(
    no_core, unboxed_closures, start, lang_items, box_syntax, never_type, linkage,
    extern_types, thread_local
)]
#![no_core]
#![allow(dead_code, non_camel_case_types)]

extern crate mini_core;

use mini_core::*;
use mini_core::libc::*;

unsafe extern "C" fn my_puts(s: *const u8) {
    puts(s);
}

#[lang = "termination"]
trait Termination {
    fn report(self) -> i32;
}

impl Termination for () {
    fn report(self) -> i32 {
        unsafe {
            NUM = 6 * 7 + 1 + (1u8 == 1u8) as u8; // 44
            *NUM_REF as i32
        }
    }
}

trait SomeTrait {
    fn object_safe(&self);
}

impl SomeTrait for &'static str {
    fn object_safe(&self) {
        unsafe {
            puts(*self as *const str as *const u8);
        }
    }
}

struct NoisyDrop {
    text: &'static str,
    inner: NoisyDropInner,
}

struct NoisyDropInner;

impl Drop for NoisyDrop {
    fn drop(&mut self) {
        unsafe {
            puts(self.text as *const str as *const u8);
        }
    }
}

impl Drop for NoisyDropInner {
    fn drop(&mut self) {
        unsafe {
            puts("Inner got dropped!\0" as *const str as *const u8);
        }
    }
}

impl SomeTrait for NoisyDrop {
    fn object_safe(&self) {}
}

enum Ordering {
    Less = -1,
    Equal = 0,
    Greater = 1,
}

#[lang = "start"]
fn start<T: Termination + 'static>(
    main: fn() -> T,
    argc: isize,
    argv: *const *const u8,
) -> isize {
    //if argc == 3 {
        //unsafe { puts(*argv); }
        let v = argv; //unsafe { puts(*((argv as usize + intrinsics::size_of::<*const u8>()) as *const *const u8)); }
    //}

    //main().report();
    0
}

static mut NUM: u8 = 6 * 7;
static NUM_REF: &'static u8 = unsafe { &NUM };

macro_rules! assert {
    ($e:expr) => {
        if !$e {
            panic(&(stringify!(! $e), file!(), line!(), 0));
        }
    };
}

macro_rules! assert_eq {
    ($l:expr, $r: expr) => {
        if $l != $r {
            panic(&(stringify!($l != $r), file!(), line!(), 0));
        }
    }
}

struct Unique<T: ?Sized> {
    pointer: *const T,
    _marker: PhantomData<T>,
}

impl<T: ?Sized, U: ?Sized> CoerceUnsized<Unique<U>> for Unique<T> where T: Unsize<U> {}

unsafe fn zeroed<T>() -> T {
    let mut uninit = MaybeUninit { uninit: () };
    intrinsics::write_bytes(&mut uninit.value.value as *mut T, 0, 1);
    uninit.value.value
}

fn take_f32(_f: f32) {}
fn take_unique(_u: Unique<()>) {}

fn return_u128_pair() -> (u128, u128) {
    (0, 0)
}

fn call_return_u128_pair() {
    return_u128_pair();
}

fn main() {
    /*take_unique(Unique {
        pointer: 0 as *const (),
        _marker: PhantomData,
    });
    take_f32(0.1);

    call_return_u128_pair();

    let slice = &[0, 1] as &[i32];
    let slice_ptr = slice as *const [i32] as *const i32;

    assert_eq!(slice_ptr as usize % 4, 0);*/
}
