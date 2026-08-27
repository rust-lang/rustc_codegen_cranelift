// Regression test for https://github.com/rust-lang/rustc_codegen_cranelift/issues/1690
// run-pass

#![feature(asm_const_ptr)]

use std::arch::naked_asm;

#[unsafe(naked)]
extern "C" fn naked() {
    naked_asm!("ret /* {} */", const &0);
}

fn main() {
    naked();
}
