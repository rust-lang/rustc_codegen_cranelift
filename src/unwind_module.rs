use cranelift_codegen::Context;
use cranelift_codegen::control::ControlPlane;
use cranelift_codegen::ir::Signature;
use cranelift_codegen::isa::{TargetFrontendConfig, TargetIsa};
use cranelift_module::{
    DataDescription, DataId, FuncId, FuncOrDataId, Linkage, Module, ModuleDeclarations,
    ModuleReloc, ModuleResult,
};
use cranelift_object::{ObjectModule, ObjectProduct};
use target_lexicon::{Architecture, BinaryFormat};

use crate::UnwindContext;

/// A wrapper around a [Module] which adds any defined function to the [UnwindContext].
pub(crate) struct UnwindModule<T> {
    pub(crate) module: T,
    unwind_context: UnwindContext,
}

impl<T: Module> UnwindModule<T> {
    pub(crate) fn new(mut module: T, pic_eh_frame: bool) -> Self {
        let unwind_context = UnwindContext::new(&mut module, pic_eh_frame);
        UnwindModule { module, unwind_context }
    }

    /// Handles LLVM's `\x01` symbol name prefix, which means "use the rest of the name verbatim,
    /// without the target's global prefix". bindgen emits it in `#[link_name]` on Apple targets.
    ///
    /// Both the object writer and `dlsym` add the global prefix themselves, so for `\x01_foo` on a
    /// target with a `_` prefix we have to pass `foo`. A verbatim name without the leading `_`
    /// can't be expressed this way and just gets the `\x01` stripped.
    fn symbol_name<'a>(&self, name: &'a str) -> &'a str {
        let Some(verbatim) = name.strip_prefix('\x01') else {
            return name;
        };
        let triple = self.module.isa().triple();
        let has_underscore_prefix = match triple.binary_format {
            BinaryFormat::Macho => true,
            BinaryFormat::Coff => matches!(triple.architecture, Architecture::X86_32(_)),
            _ => false,
        };
        if has_underscore_prefix {
            verbatim.strip_prefix('_').unwrap_or(verbatim)
        } else {
            verbatim
        }
    }
}

impl UnwindModule<ObjectModule> {
    pub(crate) fn finish(self) -> ObjectProduct {
        let mut product = self.module.finish();
        self.unwind_context.emit(&mut product);
        product
    }
}

#[cfg(feature = "jit")]
impl UnwindModule<cranelift_jit::JITModule> {
    pub(crate) fn finalize_definitions(mut self) -> cranelift_jit::JITModule {
        self.module.finalize_definitions().unwrap();
        unsafe { self.unwind_context.register_jit(&self.module) };
        self.module
    }
}

impl<T: Module> Module for UnwindModule<T> {
    fn isa(&self) -> &dyn TargetIsa {
        self.module.isa()
    }

    fn declarations(&self) -> &ModuleDeclarations {
        self.module.declarations()
    }

    fn get_name(&self, name: &str) -> Option<FuncOrDataId> {
        self.module.get_name(self.symbol_name(name))
    }

    fn target_config(&self) -> TargetFrontendConfig {
        self.module.target_config()
    }

    fn declare_function(
        &mut self,
        name: &str,
        linkage: Linkage,
        signature: &Signature,
    ) -> ModuleResult<FuncId> {
        self.module.declare_function(self.symbol_name(name), linkage, signature)
    }

    fn declare_anonymous_function(&mut self, signature: &Signature) -> ModuleResult<FuncId> {
        self.module.declare_anonymous_function(signature)
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: Linkage,
        writable: bool,
        tls: bool,
    ) -> ModuleResult<DataId> {
        self.module.declare_data(self.symbol_name(name), linkage, writable, tls)
    }

    fn declare_anonymous_data(&mut self, writable: bool, tls: bool) -> ModuleResult<DataId> {
        self.module.declare_anonymous_data(writable, tls)
    }

    fn define_function_with_control_plane(
        &mut self,
        func: FuncId,
        ctx: &mut Context,
        ctrl_plane: &mut ControlPlane,
    ) -> ModuleResult<()> {
        self.module.define_function_with_control_plane(func, ctx, ctrl_plane)?;
        self.unwind_context.add_function(&mut self.module, func, ctx);
        Ok(())
    }

    fn define_function_bytes(
        &mut self,
        _func_id: FuncId,
        _alignment: u64,
        _bytes: &[u8],
        _relocs: &[ModuleReloc],
    ) -> ModuleResult<()> {
        unimplemented!()
    }

    fn define_data(&mut self, data_id: DataId, data: &DataDescription) -> ModuleResult<()> {
        self.module.define_data(data_id, data)
    }
}
