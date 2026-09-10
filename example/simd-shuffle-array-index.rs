// Copied from pathfinder_simd-style usage.
// run-pass

#![feature(repr_simd, core_intrinsics)]
#![allow(internal_features, non_camel_case_types)]

use std::intrinsics::simd::simd_shuffle;

#[repr(simd)]
#[derive(Copy, Clone)]
struct f32x4([f32; 4]);

impl f32x4 {
    fn into_array(self) -> [f32; 4] {
        unsafe { std::mem::transmute(self) }
    }
}

fn main() {
    let x = f32x4([1.0, 2.0, 3.0, 4.0]);
    const IDX: [u32; 4] = [3, 3, 3, 3];
    let r: f32x4 = unsafe { simd_shuffle(x, x, IDX) };
    assert_eq!(r.into_array(), [4.0, 4.0, 4.0, 4.0]);
}
