use cranelift_codegen::Context;
use cranelift_codegen::control::ControlPlane;
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
        let eh_frame = unsafe { self.unwind_context.register_jit(&self.module) };

        let mut symbol_obj = object::write::Object::new(
            object::BinaryFormat::Elf,
            object::Architecture::Aarch64,
            object::Endianness::Little,
        );
        let eh_frame_section = symbol_obj.add_section(
            vec![],
            b".eh_frame".to_vec(),
            object::SectionKind::ReadOnlyData,
        );
        symbol_obj.append_section_data(eh_frame_section, &eh_frame, 1);

        let mut lowest_symbol = u64::MAX;
        let mut highest_symbol = u64::MIN;
        for (func_id, func_decl) in self.module.declarations().get_functions() {
            if !func_decl.linkage.is_final() {
                continue;
            }
            let name = func_decl.linkage_name(func_id).into_owned().into_bytes();
            let addr = self.module.get_finalized_function(func_id).addr() as u64;
            if addr < lowest_symbol {
                lowest_symbol = addr;
            }
            if addr + 1 > highest_symbol {
                highest_symbol = addr + 1;
            }
            symbol_obj.add_symbol(object::write::Symbol {
                name,
                value: addr,
                size: 1,
                kind: object::write::SymbolKind::Text,
                scope: object::write::SymbolScope::Dynamic,
                weak: false,
                section: object::write::SymbolSection::Absolute,
                flags: object::write::SymbolFlags::None,
            });
        }

        let mut symbol_obj = symbol_obj.write().unwrap();

        convert_object_elf_to_loadable_file::<object::LittleEndian>(
            &mut symbol_obj,
            (lowest_symbol as *const u8, (highest_symbol - lowest_symbol) as usize),
        );

        std::fs::write("symbols.elf", symbol_obj).unwrap();

        /*#[unsafe(no_mangle)]
        static mut SYMBOL_OBJECT: *mut u8 = std::ptr::null_mut();
        unsafe {
            SYMBOL_OBJECT = Vec::leak(symbol_obj).as_mut_ptr();
        }*/

        self.module
    }
}

#[cfg(feature = "jit")]
fn convert_object_elf_to_loadable_file<E: object::Endian>(
    bytes: &mut Vec<u8>,
    code_region: (*const u8, usize),
) {
    use object::elf::{ET_DYN, ET_EXEC, FileHeader64, ProgramHeader64, SectionHeader64};
    use object::read::elf::{FileHeader, SectionHeader};

    let e = E::default();

    let header = FileHeader64::<E>::parse(&bytes[..]).unwrap();
    let sections = header.sections(e, &bytes[..]).unwrap();
    let text_range = match sections.section_by_name(e, b".text") {
        Some((i, text)) => {
            let range = text.file_range(e);
            let e_shoff = usize::try_from(header.e_shoff.get(e)).unwrap();
            let off = e_shoff + i.0 * header.e_shentsize.get(e) as usize;

            let section: &mut SectionHeader64<E> =
                object::from_bytes_mut(&mut bytes[off..]).unwrap().0;
            // Patch vaddr, and save file location and its size.
            section.sh_addr.set(e, code_region.0 as u64);
            range
        }
        None => None,
    };

    // LLDB wants segment with virtual address set, placing them at the end of ELF.
    let ph_off = bytes.len();
    let e_phentsize = size_of::<ProgramHeader64<E>>();
    let mut e_phnum = 1;
    bytes.resize(ph_off + e_phentsize * e_phnum, 0);
    //if let Some((sh_offset, sh_size)) = text_range {
    use object::elf::PT_LOAD;

    let (v_offset, size) = code_region;
    let program: &mut ProgramHeader64<E> = object::from_bytes_mut(&mut bytes[ph_off..]).unwrap().0;
    program.p_type.set(e, PT_LOAD);
    program.p_offset.set(e, /*sh_offset*/ 0);
    program.p_vaddr.set(e, v_offset as u64);
    program.p_paddr.set(e, v_offset as u64);
    program.p_filesz.set(e, /*sh_size*/ 0);
    program.p_memsz.set(e, size as u64);
    /*} else {
        //unreachable!();
        e_phnum = 0;
    }*/

    // It is somewhat loadable ELF file at this moment.
    let header: &mut FileHeader64<E> = object::from_bytes_mut(bytes).unwrap().0;
    header.e_type.set(e, ET_EXEC);
    header.e_phoff.set(e, ph_off as u64);
    header.e_phentsize.set(e, u16::try_from(e_phentsize).unwrap());
    header.e_phnum.set(e, u16::try_from(e_phnum).unwrap());
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
