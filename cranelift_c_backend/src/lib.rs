use std::borrow::Cow;
use std::collections::HashMap;
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
    libcall_names: Box<dyn Fn(ir::LibCall) -> String + Send + Sync>,

    declarations: ModuleDeclarations,
    data_object_types: HashMap<DataId, String>,

    source: String,
}

impl CModule {
    pub fn new(libcall_names: Box<dyn Fn(ir::LibCall) -> String + Send + Sync>) -> Self {
        CModule {
            libcall_names,

            declarations: ModuleDeclarations::default(),
            data_object_types: HashMap::new(),

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
        let (data_id, _linkage) = self.declarations.declare_data(name, linkage, writable, tls)?;

        let type_name = self.declare_data_object_type(data_id);
        let data_decl = self.declarations.get_data_decl(data_id);
        write!(self.source, "{};\n", data_decl_to_c(data_decl, &type_name)).unwrap();

        Ok(data_id)
    }

    fn declare_anonymous_data(&mut self, writable: bool, tls: bool) -> ModuleResult<DataId> {
        let data_id = self.declarations.declare_anonymous_data(writable, tls)?;

        let type_name = self.declare_data_object_type(data_id);
        let data_decl = self.declarations.get_data_decl(data_id);
        write!(self.source, "{};\n", data_decl_to_c(data_decl, &type_name)).unwrap();

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
        let type_ = DataObjectType::new(match data_ctx.description().init {
            Init::Uninitialized => unreachable!(),
            Init::Zeros { size } => size,
            Init::Bytes { ref contents } => contents.len(),
        });

        let type_name = self.define_data_object_type(data_id, &type_);
        let data_decl = self.declarations.get_data_decl(data_id);

        let mut data = data_decl_to_c(data_decl, &type_name);
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

impl CModule {
    fn declare_data_object_type(&mut self, data_id: DataId) -> String {
        self.data_object_types
            .entry(data_id)
            .or_insert_with(|| {
                let name = "__type_".to_owned()
                    + name_use_to_c(&self.declarations.get_data_decl(data_id).name).as_ref();
                writeln!(self.source, "struct {name};").unwrap();
                name
            })
            .clone()
    }

    fn define_data_object_type(&mut self, data_id: DataId, type_: &DataObjectType) -> String {
        let name = self
            .data_object_types
            .entry(data_id)
            .or_insert_with(|| {
                "__type_".to_owned()
                    + name_use_to_c(&self.declarations.get_data_decl(data_id).name).as_ref()
            })
            .clone();
        writeln!(self.source, "struct {name} {{").unwrap();
        for (i, &element) in type_.0.iter().enumerate() {
            match element {
                DataObjectTypeElement::Bytes(count) => {
                    writeln!(self.source, "    char field{i}[{count}];").unwrap()
                }
                DataObjectTypeElement::Pointer => {
                    writeln!(self.source, "    void *field{i};").unwrap()
                }
            }
        }
        writeln!(self.source, "}};").unwrap();
        name
    }
}

struct DataObjectType(Vec<DataObjectTypeElement>);

#[derive(Copy, Clone)]
enum DataObjectTypeElement {
    Bytes(usize),
    Pointer,
}

impl DataObjectType {
    fn new(size: usize) -> Self {
        if size == 0 {
            DataObjectType(vec![])
        } else {
            DataObjectType(vec![DataObjectTypeElement::Bytes(size)])
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

fn data_decl_to_c(data_decl: &cranelift_module::DataDeclaration, type_name: &str) -> String {
    let linkage = linkage_to_c(data_decl.linkage);
    let thread_local = if data_decl.tls { "__thread " } else { "" };
    let (name_prefix, name_postfix) = name_decl_to_c(&data_decl.name);

    // FIXME handle alignment
    format!("{linkage}{thread_local}struct {type_name} {name_prefix}{name_postfix}")
}
