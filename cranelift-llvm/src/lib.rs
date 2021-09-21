use std::collections::HashMap;

use cranelift_codegen::ir::Signature;
use cranelift_codegen::isa::TargetIsa;
use cranelift_module::{DataId, FuncId, ModuleCompiledFunction, ModuleDeclarations};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::types::{FunctionType, IntType};
use inkwell::values::{FunctionValue, GlobalValue};
use inkwell::OptimizationLevel;

mod function;

pub struct LlvmModule<'ctx> {
    isa: &'ctx dyn TargetIsa,
    declarations: ModuleDeclarations,

    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    intrinsics: HashMap<&'static str, FunctionValue<'ctx>>,
    functions: HashMap<FuncId, FunctionValue<'ctx>>,
    data_objects: HashMap<DataId, GlobalValue<'ctx>>,
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
            functions: HashMap::new(),
            data_objects: HashMap::new(),
            intrinsics: HashMap::new(),
        });
        x
    }

    pub fn print_to_stderr(&self) {
        self.module.print_to_stderr();
    }

    pub fn compile(&mut self) -> MemoryBuffer {
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
        *self.intrinsics.entry(name).or_insert_with(|| self.module.add_function(name, ty, None))
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
    ) -> cranelift_module::ModuleResult<cranelift_module::FuncId> {
        let (func_id, linkage) = self.declarations.declare_function(name, linkage, signature)?;

        let func_val = self.module.add_function(
            name,
            function::translate_sig(self.context, signature),
            Some(translate_linkage(linkage)),
        );
        // FIXME apply param attributes
        self.functions.insert(func_id, func_val);

        Ok(func_id)
    }

    fn declare_anonymous_function(
        &mut self,
        signature: &Signature,
    ) -> cranelift_module::ModuleResult<cranelift_module::FuncId> {
        let func_id = self.declarations.declare_anonymous_function(signature)?;

        let func_val = self.module.add_function(
            "__anon",
            function::translate_sig(self.context, signature),
            Some(inkwell::module::Linkage::Internal),
        );
        // FIXME apply param attributes
        self.functions.insert(func_id, func_val);

        Ok(func_id)
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: cranelift_module::Linkage,
        writable: bool,
        tls: bool,
    ) -> cranelift_module::ModuleResult<cranelift_module::DataId> {
        let (data_id, linkage) = self.declarations.declare_data(name, linkage, writable, tls)?;

        let data_val = self.module.add_global(todo!() as IntType, None, name);
        self.data_objects.insert(data_id, data_val);

        Ok(data_id)
    }

    fn declare_anonymous_data(
        &mut self,
        writable: bool,
        tls: bool,
    ) -> cranelift_module::ModuleResult<cranelift_module::DataId> {
        let data_id = self.declarations.declare_anonymous_data(writable, tls)?;

        let data_val = self.module.add_global(todo!() as IntType, None, "__anon");
        self.data_objects.insert(data_id, data_val);

        Ok(data_id)
    }

    fn define_function(
        &mut self,
        func: cranelift_module::FuncId,
        ctx: &mut cranelift_codegen::Context,
        _trap_sink: &mut dyn cranelift_codegen::binemit::TrapSink,
        _stack_map_sink: &mut dyn cranelift_codegen::binemit::StackMapSink,
    ) -> cranelift_module::ModuleResult<cranelift_module::ModuleCompiledFunction> {
        function::define_function(self, func, ctx);

        Ok(ModuleCompiledFunction { size: 0 })
    }

    fn define_function_bytes(
        &mut self,
        func: cranelift_module::FuncId,
        bytes: &[u8],
        relocs: &[cranelift_module::RelocRecord],
    ) -> cranelift_module::ModuleResult<cranelift_module::ModuleCompiledFunction> {
        todo!()
    }

    fn define_data(
        &mut self,
        data: cranelift_module::DataId,
        data_ctx: &cranelift_module::DataContext,
    ) -> cranelift_module::ModuleResult<()> {
        todo!()
    }
}
