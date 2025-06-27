use cranelift_codegen::Context;
use cranelift_codegen::control::ControlPlane;
use cranelift_codegen::incremental_cache::CacheKvStore;
use cranelift_codegen::ir::Signature;
use cranelift_codegen::isa::{TargetFrontendConfig, TargetIsa};
use cranelift_module::{
    DataDescription, DataId, FuncId, FuncOrDataId, Linkage, Module, ModuleDeclarations,
    ModuleReloc, ModuleResult,
};
use cranelift_object::{ObjectModule, ObjectProduct};

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
        self.module.get_name(name)
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
        self.module.declare_function(name, linkage, signature)
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
        self.module.declare_data(name, linkage, writable, tls)
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
        if std::env::var("CG_CLIF_FUNCTION_CACHE").as_deref() == Ok("naive") {
            if ctx.func.layout.blocks().nth(1).is_none()
                || ctx.func.layout.blocks().nth(2).is_none()
            {
                ctx.compile(self.module.isa(), ctrl_plane)?;
            } else {
                ctx.compile_with_cache(self.module.isa(), &mut Cache, ctrl_plane)?;
            }
        } else {
            ctx.compile(self.module.isa(), ctrl_plane)?;
        }
        let res = ctx.compiled_code().unwrap();

        let alignment = res.buffer.alignment as u64;
        let relocs = res
            .buffer
            .relocs()
            .iter()
            .map(|reloc| ModuleReloc::from_mach_reloc(&reloc, &ctx.func, func))
            .collect::<Vec<_>>();
        self.module.define_function_bytes(func, alignment, res.buffer.data(), &relocs)?;

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

struct Cache;

impl Cache {
    fn file_for_key(&self, key: &[u8]) -> String {
        let mut path = key.iter().map(|b| format!("{:02x}", b)).collect::<String>();
        path.push_str(".clif_cache");
        "/home/bjorn/Projects/cg_clif/cache/".to_owned() + &path
    }
}

impl CacheKvStore for Cache {
    fn get(&self, key: &[u8]) -> Option<std::borrow::Cow<'_, [u8]>> {
        let path = self.file_for_key(key);
        if std::fs::exists(&path).unwrap() {
            Some(std::fs::read(path).unwrap().into())
        } else {
            None
        }
    }

    fn insert(&mut self, key: &[u8], val: Vec<u8>) {
        let path = self.file_for_key(key);
        std::fs::write(path, val).unwrap();
    }
}
