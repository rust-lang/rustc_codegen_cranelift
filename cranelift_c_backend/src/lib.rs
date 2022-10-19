use std::borrow::Cow;
use std::fmt::Write;
use std::io;
use std::path::Path;

use cranelift_codegen::isa::{CallConv, TargetFrontendConfig};
use cranelift_codegen::{ir, Context};
use cranelift_module::{
    DataContext, DataId, FuncId, Init, Linkage, Module, ModuleCompiledFunction, ModuleDeclarations,
    ModuleResult,
};

pub struct CModule {
    declarations: ModuleDeclarations,
    libcall_names: Box<dyn Fn(ir::LibCall) -> String + Send + Sync>,

    source: String,
}

impl CModule {
    pub fn new(libcall_names: Box<dyn Fn(ir::LibCall) -> String + Send + Sync>) -> Self {
        CModule {
            declarations: ModuleDeclarations::default(),
            libcall_names,
            source: String::new(),
        }
    }

    pub fn finish(self, path: &Path) -> io::Result<()> {
        let tmp_file = path.with_extension("c");
        std::fs::write(&tmp_file, self.source.as_bytes())?;
        assert!(
            std::process::Command::new("gcc")
                .arg("-fno-strict-aliasing")
                .arg("-c")
                .arg(&tmp_file)
                .arg("-o")
                .arg(path)
                .status()?
                .success()
        );
        // FIXME
        //std::fs::remove_file(tmp_file)?;
        Ok(())
    }
}

impl Module for CModule {
    fn isa(&self) -> &dyn cranelift_codegen::isa::TargetIsa {
        todo!()
    }

    fn target_config(&self) -> TargetFrontendConfig {
        // FIXME make configurable
        TargetFrontendConfig {
            default_call_conv: CallConv::SystemV,
            pointer_width: target_lexicon::PointerWidth::U64,
        }
    }

    fn declarations(&self) -> &cranelift_module::ModuleDeclarations {
        &self.declarations
    }

    fn declare_function(
        &mut self,
        name: &str,
        linkage: cranelift_module::Linkage,
        signature: &cranelift_codegen::ir::Signature,
    ) -> ModuleResult<FuncId> {
        let (func_id, linkage) = self.declarations.declare_function(name, linkage, signature)?;
        // FIXME add forward declaration
        Ok(func_id)
    }

    fn declare_anonymous_function(
        &mut self,
        signature: &cranelift_codegen::ir::Signature,
    ) -> ModuleResult<FuncId> {
        let func_id = self.declarations.declare_anonymous_function(signature)?;
        // FIXME add forward declaration
        Ok(func_id)
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: cranelift_module::Linkage,
        writable: bool,
        tls: bool,
    ) -> ModuleResult<DataId> {
        let (data_id, linkage) = self.declarations.declare_data(name, linkage, writable, tls)?;
        // FIXME add forward declaration
        Ok(data_id)
    }

    fn declare_anonymous_data(
        &mut self,
        writable: bool,
        tls: bool,
    ) -> ModuleResult<DataId> {
        let data_id = self.declarations.declare_anonymous_data(writable, tls)?;
        // FIXME add forward declaration
        Ok(data_id)
    }

    fn define_function(
        &mut self,
        func: FuncId,
        ctx: &mut Context,
    ) -> ModuleResult<ModuleCompiledFunction> {
        //todo!()

        Ok(ModuleCompiledFunction { size: 0 })
    }

    fn define_function_bytes(
        &mut self,
        func_id: FuncId,
        func: &cranelift_codegen::ir::Function,
        alignment: u64,
        bytes: &[u8],
        relocs: &[cranelift_codegen::MachReloc],
    ) -> ModuleResult<ModuleCompiledFunction> {
        todo!()
    }

    fn define_data(&mut self, data_id: DataId, data_ctx: &DataContext) -> ModuleResult<()> {
        let data_decl = self.declarations.get_data_decl(data_id);

        let mut data = data_decl_to_c(data_decl);
        let align =
            data_ctx.description().align.unwrap_or(1 /* FIXME correct default alignment */);
        let alignment = if align == 1 {
            String::new()
        } else {
            format!(" __attribute__ ((aligned ({align})))")
        };
        write!(data, "{alignment} = {{").unwrap();

        // FIXME handle relocations
        match &data_ctx.description().init {
            Init::Uninitialized => todo!(),
            Init::Zeros { size } => todo!(),
            Init::Bytes { contents } => {
                for (i, &byte) in contents.iter().enumerate() {
                    write!(data, "0x{:02x}", byte).unwrap();
                    if i != contents.len() - 1 {
                        write!(data, ", ").unwrap();
                    }
                }
            }
        }

        write!(data, "}};\n").unwrap();

        self.source.push_str(&data);

        Ok(())
    }
}

impl Drop for CModule {
    fn drop(&mut self) {
        if std::thread::panicking() {
            eprintln!("module:\n{}", self.source);
        }
    }
}

fn linkage_to_c(linkage: Linkage) -> &'static str {
    match linkage {
        cranelift_module::Linkage::Import => "extern ",
        cranelift_module::Linkage::Local => "static ",
        cranelift_module::Linkage::Preemptible => "__attribute__ ((weak)) ",
        cranelift_module::Linkage::Hidden => "__attribute__ ((visibility (\"hidden\"))) ",
        cranelift_module::Linkage::Export => "",
    }
}

fn string_to_c(string: &str) -> String {
    format!("{string:?}") // FIXME use proper C escape rules
}

fn name_decl_to_c<'a>(name: &'a str) -> (Cow<'a, str>, Cow<'a, str>) {
    if let Some(mangled_name) = mangle_name(name) {
        let name_string = string_to_c(name);
        (Cow::Owned(mangled_name), Cow::Owned(format!(" asm({name_string})")))
    } else {
        (Cow::Borrowed(name), Cow::Borrowed(""))
    }
}

fn name_use_to_c<'a>(name: &'a str) -> Cow<'a, str> {
    if let Some(mangled_name) = mangle_name(name) {
        Cow::Owned(mangled_name)
    } else {
        Cow::Borrowed(name)
    }
}

fn mangle_name(name: &str) -> Option<String> {
    if name.contains('.') || name.contains('$') {
        Some(name.replace('_', "__").replace('.', "_dot_").replace('$', "_dollar_"))
    } else {
        None
    }
}

fn data_decl_to_c(data_decl: &cranelift_module::DataDeclaration) -> String {
    let linkage = linkage_to_c(data_decl.linkage);
    let thread_local = if data_decl.tls { "__thread " } else { "" };
    let (name_prefix, name_postfix) = name_decl_to_c(&data_decl.name);

    // FIXME handle alignment
    format!("{linkage}{thread_local}char {name_prefix}[]{name_postfix}")
}
