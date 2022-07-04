#![feature(
    core_panic,
    fmt_internals,
    lang_items,
    core_intrinsics,
    panic_internals,
    std_internals,
    panic_info_message,
    start
)]

extern crate core;

use core::any::Any;
use core::fmt;
use core::intrinsics;
use core::mem::{self, MaybeUninit};
use core::panic::{BoxMeUp, Location, PanicInfo};

#[start]
fn main(argc: isize, argv: *const *const u8) -> isize {
    r#try(|| {
        struct StrPanicPayload(&'static str);

        unsafe impl BoxMeUp for StrPanicPayload {
            fn take_box(&mut self) -> *mut (dyn Any + Send) {
                Box::into_raw(Box::new(self.0))
            }

            fn get(&mut self) -> &(dyn Any + Send) {
                &self.0
            }
        }

        let _panic_guard = PanicGuard;

        let mut msg = &mut StrPanicPayload("explicit panic") as &mut dyn BoxMeUp;
        let code = unsafe {
            let obj = &mut msg as *mut &mut dyn BoxMeUp;
            __rust_start_panic(obj)
        };
        unsafe { std::intrinsics::abort(); }
    })
    .unwrap();
    0
}

struct PanicGuard;
impl Drop for PanicGuard {
    fn drop(&mut self) {

    }
}

#[allow(improper_ctypes)]
extern "Rust" {
    /// `payload` is passed through another layer of raw pointers as `&mut dyn Trait` is not
    /// FFI-safe. `BoxMeUp` lazily performs allocation only when needed (this avoids allocations
    /// when using the "abort" panic runtime).
    fn __rust_start_panic(payload: *mut &mut dyn BoxMeUp) -> u32;
}

#[allow(improper_ctypes)]
extern "C" {
    fn __rust_panic_cleanup(payload: *mut u8) -> *mut (dyn Any + Send + 'static);
}

fn r#try(f: fn()) -> Result<(), ()> {
    struct Data {
        f: fn(),
    }

    let mut data = Data { f };

    let data_ptr = &mut data as *mut _ as *mut u8;
    unsafe {
        return if intrinsics::r#try(do_call, data_ptr, do_catch) == 0 {
            Ok(())
        } else {
            Err(())
        };
    }

    fn do_call(data: *mut u8) {
        unsafe {
            let data = data as *mut Data;
            let data = &mut *data;
            (data.f)();
        }
    }

    fn do_catch(data: *mut u8, payload: *mut u8) {

    }
}
