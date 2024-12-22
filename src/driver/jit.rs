//! The JIT driver uses [`cranelift_jit`] to JIT execute programs without writing any object
//! files.

use crate::prelude::*;
use crate::{CodegenMode, jit};

// Alright so run_jit executes the program in a new thread, and then listens for messages from the
// program that notify it that a function needs to be JIT compiled.

// In our case, we actually want to define our own shims that request new code to be generated that
// then needs to be JIT compiled.

// So we need to modify the JIT system to allow for a custom shim protocol.
pub(crate) fn run_jit(tcx: TyCtxt<'_>, codegen_mode: CodegenMode, jit_args: Vec<String>) -> ! {
    println!(
        "Rustc codegen cranelift will JIT run the executable, because -Cllvm-args=mode=jit was passed"
    );

    jit::run_jit(tcx, codegen_mode, jit_args)
}
