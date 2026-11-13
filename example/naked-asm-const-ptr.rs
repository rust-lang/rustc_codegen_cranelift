// Regression test for an ICE while compiling `const` asm operands that point
// at anonymous constant allocations (`const &0`). Those allocations have no
// symbol name of their own, so the const pointer handling for global asm has
// to materialize them first, just like the inline asm path already does.
#![feature(asm_const_ptr)]

use std::arch::{global_asm, naked_asm};

global_asm!("/* {} */", const &0);

#[unsafe(naked)]
#[allow(dead_code)]
extern "C" fn naked() {
    naked_asm!("/* {} */", const &0)
}

fn main() {
    println!("ok");
}
