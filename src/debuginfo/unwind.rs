//! Unwind info generation (`.eh_frame`)

use crate::prelude::*;

use cranelift_codegen::ir::Endianness;
use cranelift_codegen::isa::unwind::UnwindInfo;

use cranelift_object::ObjectProduct;
use eh_frame_experiments::{
    ActionTable, CallSiteTable, ExceptionSpecTable, GccExceptTable, TypeInfoTable,
};
use gimli::write::{CieId, EhFrame, FrameTable, Section};
use gimli::{Encoding, Format, RunTimeEndian};

use super::emit::{address_for_data, address_for_func, DebugRelocName};
use super::object::WriteDebugInfo;

pub(crate) struct UnwindContext {
    endian: RunTimeEndian,
    frame_table: FrameTable,
    cie_id: Option<CieId>,
}

impl UnwindContext {
    pub(crate) fn new(module: &mut dyn Module, pic_eh_frame: bool) -> Self {
        let endian = match module.isa().endianness() {
            Endianness::Little => RunTimeEndian::Little,
            Endianness::Big => RunTimeEndian::Big,
        };
        let mut frame_table = FrameTable::default();

        let cie_id = if let Some(mut cie) = module.isa().create_systemv_cie() {
            if pic_eh_frame {
                cie.fde_address_encoding =
                    gimli::DwEhPe(gimli::DW_EH_PE_pcrel.0 | gimli::DW_EH_PE_sdata4.0);
                cie.lsda_encoding =
                    Some(gimli::DwEhPe(gimli::DW_EH_PE_pcrel.0 | gimli::DW_EH_PE_sdata4.0));
            } else {
                cie.fde_address_encoding = gimli::DW_EH_PE_absptr;
                cie.lsda_encoding = Some(gimli::DW_EH_PE_absptr);
            }
            // FIXME use eh_personality lang item instead
            let personality = module
                .declare_function(
                    "rust_eh_personality",
                    Linkage::Import,
                    &Signature {
                        params: vec![
                            AbiParam::new(types::I32),
                            AbiParam::new(types::I32),
                            AbiParam::new(types::I64),
                            AbiParam::new(module.target_config().pointer_type()),
                            AbiParam::new(module.target_config().pointer_type()),
                        ],
                        returns: vec![AbiParam::new(types::I32)],
                        call_conv: module.target_config().default_call_conv,
                    },
                )
                .unwrap();
            cie.personality = Some((
                gimli::DwEhPe(gimli::DW_EH_PE_pcrel.0 | gimli::DW_EH_PE_sdata4.0),
                address_for_func(personality),
            ));
            Some(frame_table.add_cie(cie))
        } else {
            None
        };

        UnwindContext { endian, frame_table, cie_id }
    }

    pub(crate) fn add_function(
        &mut self,
        module: &mut dyn Module,
        func_id: FuncId,
        context: &Context,
    ) {
        let unwind_info = if let Some(unwind_info) =
            context.compiled_code().unwrap().create_unwind_info(module.isa()).unwrap()
        {
            unwind_info
        } else {
            return;
        };

        match unwind_info {
            UnwindInfo::SystemV(unwind_info) => {
                let mut fde = unwind_info.to_fde(address_for_func(func_id));
                // FIXME use unique symbol name derived from function name
                let lsda = module.declare_anonymous_data(false, false).unwrap();

                let encoding = Encoding {
                    format: Format::Dwarf32,
                    version: 1,
                    address_size: module.isa().frontend_config().pointer_bytes(),
                };

                // FIXME add actual exception information here
                let gcc_except_table_data = GccExceptTable {
                    call_sites: CallSiteTable(vec![]),
                    actions: ActionTable::new(),
                    type_info: TypeInfoTable::new(gimli::DW_EH_PE_udata4),
                    exception_specs: ExceptionSpecTable::new(),
                };

                let mut gcc_except_table = super::emit::WriterRelocate::new(self.endian);

                gcc_except_table_data.write(&mut gcc_except_table, encoding).unwrap();

                let mut data = DataDescription::new();
                data.define(gcc_except_table.writer.into_vec().into_boxed_slice());
                data.set_segment_section("", ".gcc_except_table");

                for reloc in &gcc_except_table.relocs {
                    match reloc.name {
                        DebugRelocName::Section(_id) => unreachable!(),
                        DebugRelocName::Symbol(id) => {
                            let id = id.try_into().unwrap();
                            if id & 1 << 31 == 0 {
                                let func_ref =
                                    module.declare_func_in_data(FuncId::from_u32(id), &mut data);
                                data.write_function_addr(reloc.offset, func_ref);
                            } else {
                                let gv = module.declare_data_in_data(
                                    DataId::from_u32(id & !(1 << 31)),
                                    &mut data,
                                );
                                data.write_data_addr(reloc.offset, gv, 0);
                            }
                        }
                    };
                }

                module.define_data(lsda, &data).unwrap();
                fde.lsda = Some(address_for_data(lsda));
                self.frame_table.add_fde(self.cie_id.unwrap(), fde);
            }
            UnwindInfo::WindowsX64(_) => {
                // FIXME implement this
            }
            unwind_info => unimplemented!("{:?}", unwind_info),
        }
    }

    pub(crate) fn emit(self, product: &mut ObjectProduct) {
        let mut eh_frame = EhFrame::from(super::emit::WriterRelocate::new(self.endian));
        self.frame_table.write_eh_frame(&mut eh_frame).unwrap();

        if !eh_frame.0.writer.slice().is_empty() {
            let id = eh_frame.id();
            let section_id = product.add_debug_section(id, eh_frame.0.writer.into_vec());
            let mut section_map = FxHashMap::default();
            section_map.insert(id, section_id);

            for reloc in &eh_frame.0.relocs {
                product.add_debug_reloc(&section_map, &section_id, reloc);
            }
        }
    }

    #[cfg(all(feature = "jit", windows))]
    pub(crate) unsafe fn register_jit(self, _jit_module: &cranelift_jit::JITModule) {}

    #[cfg(all(feature = "jit", not(windows)))]
    pub(crate) unsafe fn register_jit(self, jit_module: &cranelift_jit::JITModule) {
        use std::mem::ManuallyDrop;

        let mut eh_frame = EhFrame::from(super::emit::WriterRelocate::new(self.endian));
        self.frame_table.write_eh_frame(&mut eh_frame).unwrap();

        if eh_frame.0.writer.slice().is_empty() {
            return;
        }

        let mut eh_frame = eh_frame.0.relocate_for_jit(jit_module);

        // GCC expects a terminating "empty" length, so write a 0 length at the end of the table.
        eh_frame.extend(&[0, 0, 0, 0]);

        // FIXME support unregistering unwind tables once cranelift-jit supports deallocating
        // individual functions
        let eh_frame = ManuallyDrop::new(eh_frame);

        // =======================================================================
        // Everything after this line up to the end of the file is loosely based on
        // https://github.com/bytecodealliance/wasmtime/blob/4471a82b0c540ff48960eca6757ccce5b1b5c3e4/crates/jit/src/unwind/systemv.rs
        #[cfg(target_os = "macos")]
        {
            // On macOS, `__register_frame` takes a pointer to a single FDE
            let start = eh_frame.as_ptr();
            let end = start.add(eh_frame.len());
            let mut current = start;

            // Walk all of the entries in the frame table and register them
            while current < end {
                let len = std::ptr::read::<u32>(current as *const u32) as usize;

                // Skip over the CIE
                if current != start {
                    __register_frame(current);
                }

                // Move to the next table entry (+4 because the length itself is not inclusive)
                current = current.add(len + 4);
            }
        }
        #[cfg(not(target_os = "macos"))]
        {
            // On other platforms, `__register_frame` will walk the FDEs until an entry of length 0
            __register_frame(eh_frame.as_ptr());
        }
    }
}

extern "C" {
    // libunwind import
    fn __register_frame(fde: *const u8);
}
