// Regression test for https://github.com/rust-lang/rustc_codegen_cranelift/issues/1690
// run-pass

#![feature(asm_const_ptr)]

use std::arch::naked_asm;

#[unsafe(naked)]
extern "C" fn naked() {
    // `ret` is x86-specific; s390x returns via `br %r14` (see `has_mnemonic` in src/lib.rs).
    #[cfg(not(target_arch = "s390x"))]
    naked_asm!("ret /* {} */", const &0);
    #[cfg(target_arch = "s390x")]
    naked_asm!("br %r14 /* {} */", const &0);
}

fn main() {
    naked();
}
