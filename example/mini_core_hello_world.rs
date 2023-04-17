#![feature(
    no_core,
    lang_items,
    never_type,
    linkage,
    extern_types,
    thread_local,
    repr_simd,
    c_unwind
)]
#![no_core]
#![allow(dead_code, non_camel_case_types)]

extern crate mini_core;

use mini_core::libc::*;
use mini_core::*;

macro_rules! assert {
    ($e:expr) => {
        if !$e {
            panic(stringify!(!$e));
        }
    };
}

macro_rules! assert_eq {
    ($l:expr, $r: expr) => {
        if $l != $r {
            panic(stringify!($l != $r));
        }
    };
}

#[lang = "termination"]
trait Termination {
    fn report(self) -> i32;
}

impl Termination for () {
    fn report(self) -> i32 {
        unsafe {
            NUM = 6 * 7 + 1 + (1u8 == 1u8) as u8; // 44
            assert_eq!(*NUM_REF as i32, 44);
        }
        0
    }
}

trait SomeTrait {
    fn object_safe(&self);
}

impl SomeTrait for &'static str {
    fn object_safe(&self) {
        unsafe {
            puts(*self as *const str as *const i8);
        }
    }
}

struct NoisyDrop {
    text: &'static str,
    inner: NoisyDropInner,
}

struct NoisyDropUnsized {
    inner: NoisyDropInner,
    text: str,
}

struct NoisyDropInner;

impl Drop for NoisyDrop {
    fn drop(&mut self) {
        unsafe {
            puts(self.text as *const str as *const i8);
        }
    }
}

impl Drop for NoisyDropInner {
    fn drop(&mut self) {
        unsafe {
            puts("Inner got dropped!\0" as *const str as *const i8);
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

/// Invoke a closure, capturing the cause of an unwinding panic if one occurs.
pub unsafe fn catch_unwind<R>(f: fn() -> R) {
    let data_ptr = f as *mut u8;
    unsafe {
        intrinsics::r#try(do_call::<R>, data_ptr, do_catch);
    }

    fn do_call<R>(data: *mut u8) {
        unsafe {
            intrinsics::transmute::<_, fn() -> R>(data)();
        }
    }

    fn do_catch(data: *mut u8, payload: *mut u8) {
        unsafe {
            puts("Caught exception!\0" as *const str as *const i8);
        }
    }
}

#[lang = "start"]
fn start<T: Termination + 'static>(
    main: fn() -> T,
    argc: isize,
    argv: *const *const u8,
    _sigpipe: u8,
) -> isize {
    unsafe {
        catch_unwind(main);
    }

    0
}

static mut NUM: u8 = 6 * 7;
static NUM_REF: &'static u8 = unsafe { &NUM };

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

#[repr(C)]
pub struct bool_11 {
    field0: bool,
    field1: bool,
    field2: bool,
    field3: bool,
    field4: bool,
    field5: bool,
    field6: bool,
    field7: bool,
    field8: bool,
    field9: bool,
    field10: bool,
}

extern "C" fn bool_struct_in_11(_arg0: bool_11) {}

#[link(name = "gcc_s")]
extern "C-unwind" {
    fn _Unwind_RaiseException(exception: *mut _Unwind_Exception) -> u8;
    fn _Unwind_Resume(exception: *mut _Unwind_Exception) -> !;
}

struct _Unwind_Exception {
    exception_class: u64,
    exception_cleanup: extern "C" fn(unwind_code: u64, exception: *mut _Unwind_Exception),
    private: [usize; 2],
}

extern "C" fn cleanup(_: u64, _: *mut _Unwind_Exception) {}

#[allow(unreachable_code)] // FIXME false positive
fn main() {
    unsafe {
        catch_unwind(|| {
            //let _a = Box::new(0);
            //take_unique(Unique { pointer: unsafe { NonNull(1 as *mut ()) }, _marker: PhantomData });
            //take_f32(0.1);
            let _noise = NoisyDropInner;

            unsafe {
                printf(
                    "failed to raise exception: %d\n\0" as *const str as *const i8,
                    _Unwind_RaiseException(
                        Box::new(_Unwind_Exception {
                            exception_class: 0u64,
                            exception_cleanup: cleanup,
                            private: [0; 2],
                        })
                        .0
                        .pointer
                        .0 as *mut _,
                    ) as i32,
                );
                panic("foo");
            }
        });
    }
}
