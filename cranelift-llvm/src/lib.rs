use std::collections::{HashMap, HashSet};

use cranelift_codegen::ir::{Function, Signature};
use cranelift_codegen::isa::TargetIsa;
use cranelift_module::{
    DataContext, DataDescription, DataId, FuncId, ModuleCompiledFunction, ModuleDeclarations,
};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::types::{BasicTypeEnum, FunctionType};
use inkwell::values::{FunctionValue, GlobalValue};
use inkwell::OptimizationLevel;

mod function;

pub struct LlvmModule<'ctx> {
    isa: &'ctx dyn TargetIsa,
    declarations: ModuleDeclarations,

    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    intrinsic_refs: HashMap<&'static str, FunctionValue<'ctx>>,
    function_refs: HashMap<FuncId, FunctionValue<'ctx>>,
    data_object_refs: HashMap<DataId, GlobalValue<'ctx>>,

    function_defs: HashMap<FuncId, Function>,
    data_object_defs: HashMap<DataId, DataDescription>,
}

impl Drop for LlvmModule<'_> {
    fn drop(&mut self) {
        if std::thread::panicking() {
            self.module.print_to_stderr();
        }
    }
}

impl<'ctx> LlvmModule<'ctx> {
    pub fn with_module<T>(
        name: &str,
        isa: &dyn TargetIsa,
        f: impl for<'a> FnOnce(&mut LlvmModule<'a>) -> T,
    ) -> T {
        let context = Context::create();
        let x = f(&mut LlvmModule {
            isa,
            declarations: ModuleDeclarations::default(),
            context: &context,
            module: context.create_module(name),
            builder: context.create_builder(),

            intrinsic_refs: HashMap::new(),
            function_refs: HashMap::new(),
            data_object_refs: HashMap::new(),

            function_defs: HashMap::new(),
            data_object_defs: HashMap::new(),
        });
        x
    }

    pub fn print_to_stderr(&self) {
        self.module.print_to_stderr();
    }

    pub fn finalize(&mut self) {
        for (func_id, func) in self.declarations.get_functions() {
            if !self.function_refs.contains_key(&func_id) {
                let func_val = self.module.add_function(
                    &func.name,
                    function::translate_sig(self.context, &func.signature),
                    Some(translate_linkage(func.linkage)),
                );
                // FIXME apply param attributes
                self.function_refs.insert(func_id, func_val);
            }
        }

        for (data_id, data) in self.declarations.get_data_objects() {
            if !self.data_object_refs.contains_key(&data_id) {
                let data_val = self.module.add_global(
                    self.context.i8_type(), // dummy
                    None,
                    &data.name,
                );
                data_val.set_externally_initialized(true);
                data_val.set_constant(!self.declarations.get_data_decl(data_id).writable);
                self.data_object_refs.insert(data_id, data_val);
            }
        }

        let func_defs = std::mem::take(&mut self.function_defs);
        let data_defs = std::mem::take(&mut self.data_object_defs);

        for (func_id, func) in func_defs {
            function::define_function(self, func_id, func);
        }

        for (data_id, data) in data_defs {
            assert!(data.function_relocs.is_empty() && data.data_relocs.is_empty());
            let data_val = self.data_object_refs[&data_id];
            data_val.set_constant(!self.declarations.get_data_decl(data_id).writable);
            data_val.set_initializer(&match data.init {
                cranelift_module::Init::Uninitialized => self
                    .context
                    .i8_type()
                    .array_type(data.init.size().try_into().unwrap())
                    .get_undef(),
                cranelift_module::Init::Zeros { size } => {
                    self.context.i8_type().array_type(size.try_into().unwrap()).const_zero()
                }
                cranelift_module::Init::Bytes { contents } => self.context.i8_type().const_array(
                    &contents
                        .iter()
                        .map(|&byte| self.context.i8_type().const_int(byte.into(), false))
                        .collect::<Vec<_>>(),
                ),
            });
        }
    }

    pub fn compile(&mut self) -> MemoryBuffer {
        self.finalize();

        self.print_to_stderr();

        let pass_manager: PassManager<Module> = PassManager::create(());
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_reassociate_pass();
        pass_manager.add_gvn_pass();
        pass_manager.add_cfg_simplification_pass();
        pass_manager.add_basic_alias_analysis_pass();
        pass_manager.add_promote_memory_to_register_pass();
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_reassociate_pass();
        pass_manager.run_on(&self.module);
        self.print_to_stderr();

        Target::initialize_x86(&InitializationConfig::default());
        let opt = OptimizationLevel::Default;
        let target = Target::from_name("x86-64").unwrap();
        let target_machine = target
            .create_target_machine(
                &TargetTriple::create("x86_64-pc-linux-gnu"),
                "x86-64",
                "+avx2",
                opt,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        target_machine.write_to_memory_buffer(&self.module, FileType::Object).unwrap()
    }

    fn get_intrinsic(&mut self, name: &'static str, ty: FunctionType<'ctx>) -> FunctionValue<'ctx> {
        *self.intrinsic_refs.entry(name).or_insert_with(|| self.module.add_function(name, ty, None))
    }
}

fn translate_linkage(linkage: cranelift_module::Linkage) -> inkwell::module::Linkage {
    match linkage {
        cranelift_module::Linkage::Import => inkwell::module::Linkage::External,
        cranelift_module::Linkage::Local => inkwell::module::Linkage::Internal,
        cranelift_module::Linkage::Preemptible => inkwell::module::Linkage::ExternalWeak,
        cranelift_module::Linkage::Hidden => todo!(),
        cranelift_module::Linkage::Export => inkwell::module::Linkage::External,
    }
}

fn data_object_type<'ctx>(context: &'ctx Context, data_ctx: &DataContext) -> BasicTypeEnum<'ctx> {
    // FIXME use an opaque struct type for data objects when declaring and .set_body() when defining
    // to allow eager data object definition
    let ptr_size = 8; // FIXME
    let ptr_ty = context.i64_type(); // FIXME

    if data_ctx.description().function_relocs.is_empty()
        && data_ctx.description().data_relocs.is_empty()
    {
        return context
            .i8_type()
            .array_type(data_ctx.description().init.size().try_into().unwrap())
            .into();
    }

    let mut relocs = HashSet::new();
    for &(offset, _) in &data_ctx.description().function_relocs {
        relocs.insert(offset);
    }
    for &(offset, _, _) in &data_ctx.description().data_relocs {
        relocs.insert(offset);
    }

    let mut parts: Vec<BasicTypeEnum> = vec![];
    let mut current_run_len = 0;

    let mut i = 0;
    let size = data_ctx.description().init.size().try_into().unwrap();
    while i < size {
        if relocs.contains(&i) {
            if current_run_len > 0 {
                parts.push(context.i8_type().array_type(current_run_len).into());
                current_run_len = 0;
            }
            parts.push(ptr_ty.into());
            i += ptr_size;
        } else {
            current_run_len += 1;
            i += 1;
        }
    }
    if current_run_len > 0 {
        parts.push(context.i8_type().array_type(current_run_len).into());
    }

    context.struct_type(&parts, true).into()
}

impl<'ctx> cranelift_module::Module for LlvmModule<'ctx> {
    fn isa(&self) -> &dyn cranelift_codegen::isa::TargetIsa {
        self.isa
    }

    fn declarations(&self) -> &cranelift_module::ModuleDeclarations {
        &self.declarations
    }

    fn declare_function(
        &mut self,
        name: &str,
        linkage: cranelift_module::Linkage,
        signature: &Signature,
    ) -> cranelift_module::ModuleResult<FuncId> {
        let (func_id, _) = self.declarations.declare_function(name, linkage, signature)?;

        Ok(func_id)
    }

    fn declare_anonymous_function(
        &mut self,
        signature: &Signature,
    ) -> cranelift_module::ModuleResult<FuncId> {
        self.declarations.declare_anonymous_function(signature)
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: cranelift_module::Linkage,
        writable: bool,
        tls: bool,
    ) -> cranelift_module::ModuleResult<DataId> {
        let (data_id, _) = self.declarations.declare_data(name, linkage, writable, tls)?;

        Ok(data_id)
    }

    fn declare_anonymous_data(
        &mut self,
        writable: bool,
        tls: bool,
    ) -> cranelift_module::ModuleResult<DataId> {
        self.declarations.declare_anonymous_data(writable, tls)
    }

    fn define_function(
        &mut self,
        func_id: FuncId,
        ctx: &mut cranelift_codegen::Context,
        _trap_sink: &mut dyn cranelift_codegen::binemit::TrapSink,
        _stack_map_sink: &mut dyn cranelift_codegen::binemit::StackMapSink,
    ) -> cranelift_module::ModuleResult<cranelift_module::ModuleCompiledFunction> {
        let func = self.declarations.get_function_decl(func_id);

        let func_val = self.module.add_function(
            &func.name,
            function::translate_sig(self.context, &func.signature),
            Some(translate_linkage(func.linkage)),
        );
        // FIXME apply param attributes
        self.function_refs.insert(func_id, func_val);
        self.function_defs.insert(func_id, ctx.func.clone());

        Ok(ModuleCompiledFunction { size: 0 })
    }

    fn define_function_bytes(
        &mut self,
        _func: FuncId,
        _bytes: &[u8],
        _relocs: &[cranelift_module::RelocRecord],
    ) -> cranelift_module::ModuleResult<cranelift_module::ModuleCompiledFunction> {
        unimplemented!("define_function_bytes can't be implemented for LLVM");
    }

    fn define_data(
        &mut self,
        data_id: DataId,
        data_ctx: &DataContext,
    ) -> cranelift_module::ModuleResult<()> {
        let data = self.declarations.get_data_decl(data_id);

        let data_val =
            self.module.add_global(data_object_type(&self.context, data_ctx), None, &data.name);
        self.data_object_refs.insert(data_id, data_val);
        self.data_object_defs.insert(data_id, data_ctx.description().clone());

        Ok(())
    }
}
