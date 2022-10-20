use std::fmt::Write;

use cranelift_codegen::binemit::Reloc;
use cranelift_module::{
    DataContext, DataId, FuncId, Init, Module, ModuleDeclarations, ModuleReloc, ModuleResult,
};

use crate::{linkage_to_c, name_decl_to_c, name_use_to_c, CModule};

impl CModule {
    fn data_object_type_name(&mut self, data_id: DataId) -> String {
        "struct __type_".to_owned()
            + name_use_to_c(&self.declarations.get_data_decl(data_id).name).as_ref()
    }

    pub(crate) fn define_data_object_type(
        &mut self,
        data_id: DataId,
        data_object_parts: &DataObjectParts,
    ) -> String {
        let name = self.data_object_type_name(data_id);
        writeln!(self.source, "{name} {{").unwrap();
        for (i, element) in data_object_parts.0.iter().enumerate() {
            match *element {
                DataObjectPart::Zeros(len) => {
                    writeln!(self.source, "    char field{i}[{len}];").unwrap();
                }
                DataObjectPart::Bytes(ref data) => {
                    let len = data.len();
                    writeln!(self.source, "    char field{i}[{len}];").unwrap();
                }
                DataObjectPart::Pointer(_) => {
                    writeln!(self.source, "    void *field{i};").unwrap();
                }
            }
        }
        writeln!(self.source, "}};").unwrap();
        name
    }

    fn data_decl_to_c(data_decl: &cranelift_module::DataDeclaration, type_name: &str) -> String {
        // https://gcc.gnu.org/onlinedocs/gcc/Common-Variable-Attributes.html
        let linkage = linkage_to_c(data_decl.linkage);
        let thread_local = if data_decl.tls { "__thread " } else { "" };
        let (name_prefix, name_suffix) = name_decl_to_c(&data_decl.name);
        format!("{linkage}{thread_local}{type_name} {name_prefix}{name_suffix}")
    }

    pub(crate) fn declare_data_object(&mut self, data_id: DataId) {
        let type_name = self.data_object_type_name(data_id);
        let data_decl = self.declarations.get_data_decl(data_id);
        writeln!(self.source, "{};\n", Self::data_decl_to_c(data_decl, &type_name)).unwrap();
    }

    pub(crate) fn define_data_object(
        &mut self,
        data_id: DataId,
        data_ctx: &DataContext,
    ) -> ModuleResult<()> {
        let reloc_size = self.target_config().pointer_bytes().into();
        let mut relocs = data_ctx
            .description()
            .all_relocs(Reloc::S390xTlsGdCall /* dummy */)
            .map(|ModuleReloc { offset, name, addend, kind: _ }| {
                let target_name = if ModuleDeclarations::is_function(&name) {
                    name_use_to_c(
                        &self.declarations.get_function_decl(FuncId::from_name(&name)).name,
                    )
                } else {
                    name_use_to_c(&self.declarations.get_data_decl(DataId::from_name(&name)).name)
                };
                let reloc_target = if addend == 0 {
                    format!("(void*)&{target_name}")
                } else {
                    format!("(void*)((char*)&{target_name} + {addend})")
                };
                (offset as usize, reloc_target)
            })
            .collect::<Vec<_>>();
        relocs.sort();
        let data_object_parts =
            DataObjectParts::new(&data_ctx.description().init, reloc_size, relocs);

        let type_name = self.define_data_object_type(data_id, &data_object_parts);
        let data_decl = self.declarations.get_data_decl(data_id);

        let mut data = Self::data_decl_to_c(data_decl, &type_name);
        // https://gcc.gnu.org/onlinedocs/gcc/Common-Variable-Attributes.html
        let align =
            data_ctx.description().align.unwrap_or(1 /* FIXME correct default alignment */);
        let alignment = if align == 1 {
            String::new()
        } else {
            format!(" __attribute__ ((aligned ({align})))")
        };
        let section =
            if let Some((segment, section)) = &data_ctx.description().custom_segment_section {
                if segment.is_empty() {
                    format!(" __attribute__ ((section (\"{section}\")))")
                } else {
                    format!(" __attribute__ ((section (\"{segment},{section}\")))")
                }
            } else {
                String::new()
            };
        write!(data, "{alignment}{section} = {{").unwrap();

        for (i, element) in data_object_parts.0.into_iter().enumerate() {
            if i != 0 {
                write!(data, ", ").unwrap();
            }
            match element {
                DataObjectPart::Zeros(_size) => write!(data, "{{0}}").unwrap(),
                DataObjectPart::Bytes(contents) => {
                    write!(data, "{{").unwrap();
                    for (i, &byte) in contents.iter().enumerate() {
                        if i != 0 {
                            write!(data, ", ").unwrap();
                        }
                        write!(data, "0x{:02x}", byte).unwrap();
                    }
                    write!(data, "}}").unwrap();
                }
                DataObjectPart::Pointer(reloc_target) => write!(data, "{reloc_target}").unwrap(),
            }
        }

        writeln!(data, "}};\n").unwrap();

        self.source.push_str(&data);

        Ok(())
    }
}

pub struct DataObjectParts<'a>(Vec<DataObjectPart<'a>>);

pub enum DataObjectPart<'a> {
    Zeros(usize),
    Bytes(&'a [u8]),
    Pointer(String),
}

impl<'a> DataObjectParts<'a> {
    /// `relocs` must be sorted.
    fn new(init: &'a Init, reloc_size: usize, mut relocs: Vec<(usize, String)>) -> Self {
        let size = match init {
            Init::Uninitialized => unreachable!(),
            Init::Zeros { size } => *size,
            Init::Bytes { contents } => contents.len(),
        };
        relocs.sort();
        let mut data = vec![];
        let mut current_size = 0;
        for (reloc_offset, reloc_target) in relocs {
            assert!(reloc_offset + reloc_size <= size, "{reloc_offset} + {reloc_size} <= {size}");
            assert!(reloc_offset >= current_size);
            if reloc_offset > current_size {
                match init {
                    Init::Uninitialized => unreachable!(),
                    Init::Zeros { size: _ } => {
                        data.push(DataObjectPart::Zeros(reloc_offset - current_size));
                    }
                    Init::Bytes { contents } => {
                        data.push(DataObjectPart::Bytes(&contents[current_size..reloc_offset]));
                    }
                }
            }
            data.push(DataObjectPart::Pointer(reloc_target));
            current_size = reloc_offset + reloc_size;
        }
        if current_size < size {
            match init {
                Init::Uninitialized => unreachable!(),
                Init::Zeros { size: _ } => {
                    data.push(DataObjectPart::Zeros(size - current_size));
                }
                Init::Bytes { contents } => {
                    data.push(DataObjectPart::Bytes(&contents[current_size..size]));
                }
            }
        }
        DataObjectParts(data)
    }
}
