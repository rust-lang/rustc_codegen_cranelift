#![feature(core_intrinsics, custom_mir)]
#![allow(internal_features)]

use std::hint::black_box;

const INPUT: [u8; 16] = [
    0x81, 0x92, 0xa3, 0xb4, 0xc5, 0xd6, 0xe7, 0xf8, 0x89, 0x9a, 0xab, 0xbc, 0xcd, 0xde, 0xef, 0xf0,
];
const OUTPUT: [u8; 16] = [
    0xf0, 0xef, 0xde, 0xcd, 0xbc, 0xab, 0x9a, 0x89, 0xf8, 0xe7, 0xd6, 0xc5, 0xb4, 0xa3, 0x92, 0x81,
];

#[derive(Clone, Copy)]
#[repr(C)]
struct Bytes<const N: usize>([u8; N]);

#[inline(never)]
extern "C" fn receive<const N: usize>(value: Bytes<N>) -> Bytes<N> {
    // Observe the argument before returning a different value, so two incorrect
    // conversions cannot cancel each other out in a round trip.
    assert_eq!(value.0, INPUT[..N]);
    Bytes(OUTPUT[..N].try_into().unwrap())
}

fn check<const N: usize>() {
    let receive = black_box(receive::<N> as extern "C" fn(Bytes<N>) -> Bytes<N>);
    let value = black_box(Bytes(INPUT[..N].try_into().unwrap()));
    assert_eq!(receive(value).0, OUTPUT[..N]);

    #[cfg(target_os = "linux")]
    unsafe {
        guarded::check(receive);
    }
}

#[cfg(target_os = "linux")]
mod guarded {
    use std::ffi::c_void;
    use std::intrinsics::mir::*;

    use super::{Bytes, INPUT, OUTPUT, black_box};

    unsafe extern "C" {
        fn getpagesize() -> i32;
        fn mmap(
            addr: *mut c_void,
            len: usize,
            prot: i32,
            flags: i32,
            fd: i32,
            offset: isize,
        ) -> *mut c_void;
        fn mprotect(addr: *mut c_void, len: usize, prot: i32) -> i32;
        fn munmap(addr: *mut c_void, len: usize) -> i32;
    }

    // Pass directly from the guarded allocation. Ordinary Rust MIR introduces a
    // temporary copy, which would hide an out-of-bounds ABI load on the stack.
    #[custom_mir(dialect = "runtime", phase = "optimized")]
    unsafe fn call<const N: usize>(
        receive: extern "C" fn(Bytes<N>) -> Bytes<N>,
        value: *const Bytes<N>,
    ) -> Bytes<N> {
        mir! {
            {
                Call(RET = receive(*value), ReturnTo(done), UnwindUnreachable())
            }
            done = {
                Return()
            }
        }
    }

    pub(super) unsafe fn check<const N: usize>(receive: extern "C" fn(Bytes<N>) -> Bytes<N>) {
        unsafe {
            let page_size = usize::try_from(getpagesize()).unwrap();
            // PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS.
            let mapping = mmap(std::ptr::null_mut(), page_size * 2, 3, 0x22, -1, 0);
            assert_ne!(mapping as isize, -1);
            let boundary = mapping.cast::<u8>().add(page_size);
            assert_eq!(mprotect(boundary.cast(), page_size, 0), 0);

            // A full-register load past this argument would touch the protected page.
            let value = boundary.sub(N).cast::<Bytes<N>>();
            value.write(Bytes(INPUT[..N].try_into().unwrap()));
            assert_eq!(call(receive, black_box(value)).0, OUTPUT[..N]);

            assert_eq!(munmap(mapping, page_size * 2), 0);
        }
    }
}

fn main() {
    check::<1>();
    check::<2>();
    check::<3>();
    check::<4>();
    check::<5>();
    check::<6>();
    check::<7>();
    check::<8>();
    check::<9>();
    check::<10>();
    check::<11>();
    check::<12>();
    check::<13>();
    check::<14>();
    check::<15>();
    check::<16>();
}
