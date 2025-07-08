use std::cell::RefCell;
use std::collections::BTreeMap;
use std::mem;
use std::sync::{Arc, OnceLock};

use cranelift_codegen::control::ControlPlane;
use cranelift_codegen::entity::SecondaryMap;
use cranelift_codegen::ir::function::FunctionParameters;
use cranelift_codegen::ir::{ExternalName, Signature, UserExternalName};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::{Final, FinalizedMachReloc, FinalizedRelocTarget, MachBufferFinalized};
use cranelift_module::{
    DataId, ModuleDeclarations, ModuleError, ModuleReloc, ModuleRelocTarget, ModuleResult,
};
use rustc_data_structures::stable_hasher::HashStable;

use crate::prelude::*;

pub(super) struct SerializableModule {
    isa: Arc<dyn TargetIsa>,
    inner: SerializableModuleInner,
    serialized: OnceLock<Vec<u8>>,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct SerializableModuleInner {
    declarations: ModuleDeclarations,
    functions: BTreeMap<FuncId, RefCell<FunctionMaybeCompiled>>,
    data_objects: BTreeMap<DataId, DataDescription>,
    global_asm: String,
}

#[derive(serde::Serialize, serde::Deserialize)]
enum FunctionMaybeCompiled {
    Ir(Function),
    Compiled(MachBufferFinalized<Final>, FunctionParameters),
}

impl<CTX> HashStable<CTX> for SerializableModule {
    fn hash_stable(
        &self,
        hcx: &mut CTX,
        hasher: &mut rustc_data_structures::stable_hasher::StableHasher,
    ) {
        let ser = self.serialized.get_or_init(|| self.serialize());
        ser.hash_stable(hcx, hasher);
    }
}

impl SerializableModule {
    pub(crate) fn new(isa: Arc<dyn TargetIsa>) -> Self {
        SerializableModule {
            isa,
            inner: SerializableModuleInner {
                declarations: ModuleDeclarations::default(),
                functions: BTreeMap::new(),
                data_objects: BTreeMap::new(),
                global_asm: String::new(),
            },
            serialized: OnceLock::new(),
        }
    }

    pub(crate) fn serialize(&self) -> Vec<u8> {
        self.compile_funcs();
        postcard::to_stdvec(&self.inner).unwrap()
    }

    pub(crate) fn deserialize(blob: &[u8], isa: Arc<dyn TargetIsa>) -> SerializableModule {
        // FIXME check isa compatibility
        SerializableModule {
            isa,
            inner: postcard::from_bytes(blob).unwrap(),
            serialized: OnceLock::new(),
        }
    }

    pub(crate) fn add_global_asm(&mut self, asm: &str) {
        self.inner.global_asm.push_str(asm);
    }

    fn compile_funcs(&self) {
        let mut ctx = Context::new();
        for func in self.inner.functions.values() {
            let mut func = func.borrow_mut();
            match &mut *func {
                FunctionMaybeCompiled::Ir(ir_func) => {
                    // FIXME lazily do this during serialize/apply_to
                    ctx.func = mem::replace(ir_func, Function::new());
                    let res = ctx.compile(&*self.isa, &mut ControlPlane::default()).unwrap();

                    let buffer = res.buffer.clone();
                    *func = FunctionMaybeCompiled::Compiled(buffer, ctx.func.params);
                }
                FunctionMaybeCompiled::Compiled(_, _) => {}
            }
        }
    }

    pub(crate) fn apply_to(self, module: &mut dyn Module) -> String {
        self.compile_funcs();
        let mut function_map: SecondaryMap<FuncId, Option<FuncId>> = SecondaryMap::new();
        let mut data_object_map: SecondaryMap<DataId, Option<DataId>> = SecondaryMap::new();

        let mut remap_func_id =
            |module: &mut dyn Module, declarations: &ModuleDeclarations, func_id: FuncId| {
                if function_map[func_id].is_none() {
                    let decl = declarations.get_function_decl(func_id);
                    function_map[func_id] = Some(if let Some(name) = &decl.name {
                        module.declare_function(name, decl.linkage, &decl.signature).unwrap()
                    } else {
                        module.declare_anonymous_function(&decl.signature).unwrap()
                    });
                }
                function_map[func_id].unwrap()
            };

        let mut remap_data_id =
            |module: &mut dyn Module, declarations: &ModuleDeclarations, data_id: DataId| {
                if data_object_map[data_id].is_none() {
                    let decl = declarations.get_data_decl(data_id);
                    data_object_map[data_id] = Some(if let Some(name) = &decl.name {
                        module.declare_data(name, decl.linkage, decl.writable, decl.tls).unwrap()
                    } else {
                        module.declare_anonymous_data(decl.writable, decl.tls).unwrap()
                    });
                }
                data_object_map[data_id].unwrap()
            };

        for (func_id, func) in self.inner.functions {
            let func_id = remap_func_id(module, &self.inner.declarations, func_id);

            let FunctionMaybeCompiled::Compiled(buffer, params) = &*func.borrow() else {
                unreachable!()
            };

            let remap_reloc = |reloc: &FinalizedMachReloc| {
                let name = match reloc.target {
                    FinalizedRelocTarget::ExternalName(ExternalName::User(reff)) => {
                        let ext_name = &params.user_named_funcs()[reff];
                        let ext_name = if ext_name.namespace == 0 {
                            UserExternalName::new(
                                0,
                                remap_func_id(
                                    module,
                                    &self.inner.declarations,
                                    FuncId::from_u32(ext_name.index),
                                )
                                .as_u32(),
                            )
                        } else if ext_name.namespace == 1 {
                            UserExternalName::new(
                                1,
                                remap_data_id(
                                    module,
                                    &self.inner.declarations,
                                    DataId::from_u32(ext_name.index),
                                )
                                .as_u32(),
                            )
                        } else {
                            unreachable!();
                        };
                        ModuleRelocTarget::user(ext_name.namespace, ext_name.index)
                    }
                    FinalizedRelocTarget::ExternalName(ExternalName::TestCase(_)) => {
                        unimplemented!()
                    }
                    FinalizedRelocTarget::ExternalName(ExternalName::LibCall(libcall)) => {
                        ModuleRelocTarget::LibCall(libcall)
                    }
                    FinalizedRelocTarget::ExternalName(ExternalName::KnownSymbol(ks)) => {
                        ModuleRelocTarget::KnownSymbol(ks)
                    }
                    FinalizedRelocTarget::Func(offset) => {
                        ModuleRelocTarget::FunctionOffset(func_id, offset)
                    }
                };
                ModuleReloc { offset: reloc.offset, kind: reloc.kind, name, addend: reloc.addend }
            };

            let relocs = buffer.relocs().iter().map(remap_reloc).collect::<Vec<_>>();
            module
                .define_function_bytes(func_id, buffer.alignment as u64, buffer.data(), &relocs)
                .unwrap();
        }

        for (data_id, mut data) in self.inner.data_objects {
            let data_id = remap_data_id(module, &self.inner.declarations, data_id);
            for ext_name in data
                .function_decls
                .iter_mut()
                .map(|(_, ext_name)| ext_name)
                .chain(data.data_decls.iter_mut().map(|(_, ext_name)| ext_name))
            {
                match *ext_name {
                    ModuleRelocTarget::User { namespace, ref mut index } => {
                        if namespace == 0 {
                            *index = remap_func_id(
                                module,
                                &self.inner.declarations,
                                FuncId::from_u32(*index),
                            )
                            .as_u32();
                        } else if namespace == 1 {
                            *index = remap_data_id(
                                module,
                                &self.inner.declarations,
                                DataId::from_u32(*index),
                            )
                            .as_u32();
                        } else {
                            unreachable!();
                        }
                    }
                    ModuleRelocTarget::KnownSymbol(_)
                    | ModuleRelocTarget::LibCall(_)
                    | ModuleRelocTarget::FunctionOffset(_, _) => {}
                }
            }
            module.define_data(data_id, &data).unwrap();
        }

        self.inner.global_asm
    }
}

impl Module for SerializableModule {
    fn isa(&self) -> &dyn TargetIsa {
        &*self.isa
    }

    fn declarations(&self) -> &ModuleDeclarations {
        &self.inner.declarations
    }

    fn declare_function(
        &mut self,
        name: &str,
        linkage: Linkage,
        signature: &Signature,
    ) -> ModuleResult<FuncId> {
        let (id, _linkage) = self.inner.declarations.declare_function(name, linkage, signature)?;

        Ok(id)
    }

    fn declare_anonymous_function(&mut self, signature: &Signature) -> ModuleResult<FuncId> {
        Ok(self.inner.declarations.declare_anonymous_function(signature)?)
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: Linkage,
        writable: bool,
        tls: bool,
    ) -> ModuleResult<DataId> {
        let (id, _linkage) = self.inner.declarations.declare_data(name, linkage, writable, tls)?;

        Ok(id)
    }

    fn declare_anonymous_data(&mut self, writable: bool, tls: bool) -> ModuleResult<DataId> {
        Ok(self.inner.declarations.declare_anonymous_data(writable, tls)?)
    }

    fn define_function_with_control_plane(
        &mut self,
        func_id: FuncId,
        ctx: &mut Context,
        _ctrl_plane: &mut ControlPlane,
    ) -> ModuleResult<()> {
        let decl = self.inner.declarations.get_function_decl(func_id);
        if !decl.linkage.is_definable() {
            return Err(ModuleError::InvalidImportDefinition(
                decl.name.as_deref().unwrap_or("<anonymous>").to_owned(),
            ));
        }

        if self.inner.functions.get(&func_id).is_some() {
            return Err(ModuleError::DuplicateDefinition(
                decl.name.as_deref().unwrap_or("<anonymous>").to_owned(),
            ));
        }

        self.inner
            .functions
            .insert(func_id, RefCell::new(FunctionMaybeCompiled::Ir(ctx.func.clone())));

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
        let decl = self.inner.declarations.get_data_decl(data_id);
        if !decl.linkage.is_definable() {
            return Err(ModuleError::InvalidImportDefinition(
                decl.name.as_deref().unwrap_or("<anonymous>").to_owned(),
            ));
        }

        if self.inner.data_objects.get(&data_id).is_some() {
            return Err(ModuleError::DuplicateDefinition(
                decl.name.as_deref().unwrap_or("<anonymous>").to_owned(),
            ));
        }

        self.inner.data_objects.insert(data_id, data.clone());

        Ok(())
    }
}
