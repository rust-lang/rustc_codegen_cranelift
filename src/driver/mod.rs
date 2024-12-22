//! Drivers are responsible for calling [`codegen_fn`] or [`codegen_static`] for each mono item and
//! performing any further actions like JIT executing or writing object files.
//!
//! [`codegen_fn`]: crate::base::codegen_fn
//! [`codegen_static`]: crate::constant::codegen_static

pub(crate) mod aot;
#[cfg(feature = "jit")]
pub(crate) mod jit;
