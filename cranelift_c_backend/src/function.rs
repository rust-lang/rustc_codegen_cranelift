use std::fmt::Write;

use cranelift_codegen::binemit::Reloc;
use cranelift_codegen::ir::{types, AbiParam, ArgumentExtension, ArgumentPurpose};
use cranelift_codegen::Context;
use cranelift_module::{
    DataContext, DataId, FuncId, Module, ModuleDeclarations, ModuleReloc, ModuleResult,
};

use crate::{linkage_to_c, name_decl_to_c, name_use_to_c, CModule};

impl CModule {
    fn normal_abi_param_to_c(param: &AbiParam) -> &'static str {
        assert_eq!(param.purpose, ArgumentPurpose::Normal);

        match param.extension {
            ArgumentExtension::None | ArgumentExtension::Uext => match param.value_type {
                types::I8 => "uint8_t",
                types::I16 => "uint16_t",
                types::I32 => "uint32_t",
                types::I64 => "uint64_t",
                types::I128 => "unsigned __int128",
                types::F32 => "float",
                types::F64 => "double",
                ty => todo!("{ty:?}"),
            },
            ArgumentExtension::Sext => match param.value_type {
                types::I8 => "int8_t",
                types::I16 => "int16_t",
                types::I32 => "int32_t",
                types::I64 => "int64_t",
                types::I128 => "__int128",
                types::F32 => "float",
                types::F64 => "double",
                ty => todo!("{ty:?}"),
            },
        }
    }

    fn function_signature_to_c(
        func_decl: &cranelift_module::FunctionDeclaration,
    ) -> (String, String) {
        let linkage = linkage_to_c(func_decl.linkage);
        let (name_prefix, name_suffix) = name_decl_to_c(&func_decl.name);
        let return_type = if let Some(struct_return_index) =
            func_decl.signature.special_param_index(ArgumentPurpose::StructReturn)
        {
            let struct_return_size = func_decl.signature.params[struct_return_index];
            todo!()
        } else {
            match &*func_decl.signature.returns {
                [] => "void".to_owned(),
                [ret] => Self::normal_abi_param_to_c(ret).to_owned(),
                rets => {
                    // FIXME
                    /*let mut ret_type = "struct __ret_".to_owned()
                        + name_use_to_c(&func_decl.name).as_ref()
                        + " {\n";
                    for (i, ret) in rets.iter().enumerate() {
                        writeln!(ret_type, "    {} field{i};", Self::normal_abi_param_to_c(ret))
                            .unwrap();
                    }
                    write!(ret_type, "}}").unwrap();
                    ret_type*/
                    "void".to_owned()
                }
            }
        };
        let mut arguments = String::new();
        for (i, param) in func_decl.signature.params.iter().enumerate() {
            assert_eq!(param.purpose, ArgumentPurpose::Normal);

            if i != 0 {
                write!(arguments, ", ").unwrap();
            }

            write!(arguments, "{} arg{i}", Self::normal_abi_param_to_c(param)).unwrap();
        }
        (format!("{linkage}{return_type} {name_prefix}({arguments})"), name_suffix.into_owned())
    }

    pub(crate) fn declare_function_inner(&mut self, func_id: FuncId) {
        let func_decl = self.declarations.get_function_decl(func_id);
        let (name_prefix, name_suffix) = Self::function_signature_to_c(func_decl);
        writeln!(self.source, "{name_prefix}{name_suffix};\n").unwrap();
    }

    pub(crate) fn define_function_inner(
        &mut self,
        func_id: FuncId,
        ctx: &Context,
    ) -> ModuleResult<()> {
        let func_decl = self.declarations.get_function_decl(func_id);

        // NOTE: name_suffix is ignored as gcc doesn't allow asm("...") on function definitions and
        // there is a forward declaration to apply it anyway.
        // https://gcc.gnu.org/onlinedocs/gcc-8.1.0/gcc/Asm-Labels.html
        let (mut func, _name_suffix) = Self::function_signature_to_c(func_decl);

        // FIXME implement
        writeln!(func, " {{}}").unwrap();

        self.source.push_str(&func);

        Ok(())
    }
}
