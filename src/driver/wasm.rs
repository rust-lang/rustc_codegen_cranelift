//! The AOT driver uses [`cranelift_object`] to write object files suitable for linking into a
//! standalone executable.

use std::fs::File;
use std::mem;

use cranelift_codegen::binemit::Reloc;
use cranelift_codegen::ir::{BlockCall, ExternalName, InstructionData, LibCall, Opcode};
use cranelift_codegen::isa::{self, CallConv, TargetFrontendConfig};
use cranelift_module::{DataId, ModuleDeclarations, ModuleReloc, ModuleRelocTarget};
use rustc_codegen_ssa::{CodegenResults, CompiledModule, CrateInfo, ModuleKind};
use rustc_metadata::EncodedMetadata;
use rustc_middle::dep_graph::{WorkProduct, WorkProductId};
use rustc_middle::mir::mono::MonoItem;
use rustc_session::Session;
use rustc_session::config::OutputType;
use rustc_span::Symbol;

use crate::BackendConfig;
use crate::debuginfo::TypeDebugContext;
use crate::prelude::*;

pub(crate) struct OngoingCodegen {
    results: CodegenResults,
}

impl OngoingCodegen {
    pub(crate) fn join(
        self,
        sess: &Session,
    ) -> (CodegenResults, FxIndexMap<WorkProductId, WorkProduct>) {
        sess.dcx().abort_if_errors();

        (self.results, FxIndexMap::default())
    }
}

pub(crate) fn compile_wasm(
    tcx: TyCtxt<'_>,
    metadata: EncodedMetadata,
    need_metadata_module: bool,
) -> Box<OngoingCodegen> {
    assert!(!need_metadata_module);

    let cgus = if tcx.sess.opts.output_types.should_codegen() {
        tcx.collect_and_partition_mono_items(()).1
    } else {
        // If only `--emit metadata` is used, we shouldn't perform any codegen.
        // Also `tcx.collect_and_partition_mono_items` may panic in that case.
        return Box::new(OngoingCodegen {
            results: CodegenResults {
                modules: vec![],
                allocator_module: None,
                metadata_module: None,
                metadata,
                crate_info: CrateInfo::new(tcx, String::new()),
            },
        });
    };

    let mono_items =
        cgus.iter().flat_map(|cgu| cgu.items_in_deterministic_order(tcx)).collect::<Vec<_>>();

    let mut module = WasmModule::new();

    let mut cx = crate::CodegenCx::new(
        tcx,
        TargetFrontendConfig {
            default_call_conv: CallConv::SystemV,
            pointer_width: target_lexicon::PointerWidth::U32,
            page_size_align_log2: 1,
        },
        false,
        Symbol::intern("main_cgu"),
    );
    let mut type_dbg = TypeDebugContext::default();
    super::predefine_mono_items(tcx, &mut module, &mono_items);
    let mut codegened_functions = vec![];
    for (mono_item, _) in mono_items {
        match mono_item {
            MonoItem::Fn(inst) => {
                if let Some(codegened_function) = crate::base::codegen_fn(
                    tcx,
                    &mut cx,
                    &mut type_dbg,
                    Function::new(),
                    &mut module,
                    inst,
                ) {
                    codegened_functions.push(codegened_function);
                }
            }
            MonoItem::Static(def_id) => {
                crate::constant::codegen_static(tcx, &mut module, def_id);
            }
            MonoItem::GlobalAsm(item_id) => {
                let item = tcx.hir().item(item_id);
                tcx.dcx().span_fatal(item.span, "Global asm is not supported for WASM");
            }
        }
    }
    crate::main_shim::maybe_create_entry_wrapper(tcx, &mut module, false, true);

    if !cx.global_asm.is_empty() {
        tcx.dcx().fatal("Inline asm is not supported for WASM");
    }

    let mut cached_context = Context::new();
    for codegened_func in codegened_functions {
        crate::base::compile_fn(
            &mut cx,
            &tcx.prof,
            &mut cached_context,
            &mut module,
            codegened_func,
        );
    }

    let tmp_file =
        tcx.output_filenames(()).temp_path(OutputType::Object, Some(&"main_cgu".to_owned()));
    let file = match File::create(&tmp_file) {
        Ok(file) => file,
        Err(err) => tcx.dcx().fatal(format!("error creating object file: {}", err)),
    };

    if let Err(err) = module.write_stream(file) {
        tcx.dcx().fatal(format!("error writing object file: {}", err));
    }

    Box::new(OngoingCodegen {
        results: CodegenResults {
            modules: vec![CompiledModule {
                name: "main_cgu".to_owned(),
                kind: ModuleKind::Regular,
                object: Some(tmp_file),
                dwarf_object: None,
                bytecode: None,
                assembly: None,
                llvm_ir: None,
            }],
            allocator_module: None,
            metadata_module: None,
            metadata,
            crate_info: CrateInfo::new(tcx, String::new()),
        },
    })
}

struct WasmModule {
    waffle_module: waffle::Module<'static>,
    declarations: ModuleDeclarations,
    func_ids: FxHashMap<FuncId, waffle::Func>,
    data_ids: FxHashMap<DataId, waffle::Global>,
    next_data_offset: u64,
    main_memory: waffle::Memory,
    stack_pointer: waffle::Global,
    data_relocs: Vec<(DataId, ModuleReloc)>,
}

impl WasmModule {
    fn new() -> Self {
        let mut waffle_module = waffle::Module::empty();
        let main_memory = waffle_module.memories.push(waffle::MemoryData {
            initial_pages: 0x2_000_000 / 65536, // FIXME modify as necessary
            maximum_pages: None,
            segments: vec![],
        });
        let stack_pointer = waffle_module.globals.push(waffle::GlobalData {
            ty: waffle::Type::I32,
            value: Some(0x1_000_000),
            mutable: true,
        });
        waffle_module.exports.push(waffle::Export {
            name: "__stack_pointer".to_owned(),
            kind: waffle::ExportKind::Global(stack_pointer),
        });
        WasmModule {
            waffle_module,
            declarations: ModuleDeclarations::default(),
            func_ids: FxHashMap::default(),
            data_ids: FxHashMap::default(),
            next_data_offset: 0x1_000_000, // Globals are stored after 1MiB of stack space
            main_memory,
            stack_pointer,
            data_relocs: vec![],
        }
    }

    fn write_stream<W: std::io::Write>(
        mut self,
        mut writer: W,
    ) -> Result<(), Box<dyn std::error::Error>> {
        {
            let signature = &Signature {
                params: vec![AbiParam::new(types::I32)],
                returns: vec![AbiParam::new(types::I32)],
                call_conv: CallConv::SystemV,
            };
            let (malloc_id, _linkage) =
                self.declarations.declare_function("malloc", Linkage::Local, signature)?;

            self.func_ids
                .entry(malloc_id)
                .or_insert_with(|| self.waffle_module.funcs.push(waffle::FuncDecl::None));

            let waffle_sig =
                FunctionBuilder::translate_signature(&mut self.waffle_module, &signature);

            let mut waffle_func = waffle::FunctionBody::new(&mut self.waffle_module, waffle_sig);

            let alloc_size = waffle_func.blocks[waffle_func.entry].params[0].1;

            let page_size = waffle::ValueDef::Operator(
                waffle::Operator::I32Const { value: 65536 },
                waffle_func.arg_pool.from_iter([].into_iter()),
                waffle_func.single_type_list(waffle::Type::I32),
            );
            let page_size = waffle_func.add_value(page_size);
            waffle_func.append_to_block(waffle_func.entry, page_size);

            let alloc_size = waffle::ValueDef::Operator(
                waffle::Operator::I32Add,
                waffle_func.arg_pool.from_iter([alloc_size, page_size].into_iter()),
                waffle_func.single_type_list(waffle::Type::I32),
            );
            let alloc_size = waffle_func.add_value(alloc_size);
            waffle_func.append_to_block(waffle_func.entry, alloc_size);

            let page_count = waffle::ValueDef::Operator(
                waffle::Operator::I32DivU,
                waffle_func.arg_pool.from_iter([alloc_size, page_size].into_iter()),
                waffle_func.single_type_list(waffle::Type::I32),
            );
            let page_count = waffle_func.add_value(page_count);
            waffle_func.append_to_block(waffle_func.entry, page_count);

            let allocated = waffle::ValueDef::Operator(
                waffle::Operator::MemoryGrow { mem: self.main_memory },
                waffle_func.arg_pool.from_iter([page_count].into_iter()),
                waffle_func.single_type_list(waffle::Type::I32),
            );
            let allocated = waffle_func.add_value(allocated);
            waffle_func.append_to_block(waffle_func.entry, allocated);

            waffle_func.set_terminator(waffle_func.entry, waffle::Terminator::Return {
                values: vec![allocated],
            });

            self.waffle_module.funcs[self.func_ids[&malloc_id]] =
                waffle::FuncDecl::Body(waffle_sig, "malloc".to_owned(), waffle_func);
        }

        for (func_id, func_decl) in self.declarations.get_functions() {
            if func_decl.linkage == Linkage::Import {
                let func = self.func_ids[&func_id];
                if let waffle::FuncDecl::None = self.waffle_module.funcs[func] {
                    self.waffle_module.funcs[func] = waffle::FuncDecl::Import(
                        FunctionBuilder::translate_signature(
                            &mut self.waffle_module,
                            &func_decl.signature,
                        ),
                        func_decl.linkage_name(func_id).into_owned(),
                    );
                } else {
                    panic!("defined function with import linkage");
                }
                let name = func_decl.linkage_name(func_id).into_owned();
                if let Some((module, name)) = name.split_once("$$") {
                    self.waffle_module.imports.push(waffle::Import {
                        module: module.to_owned(),
                        name: name.to_owned(),
                        kind: waffle::ImportKind::Func(func),
                    });
                } else {
                    self.waffle_module.imports.push(waffle::Import {
                        module: "env".to_owned(),
                        name,
                        kind: waffle::ImportKind::Func(func),
                    });
                }
            } else if func_decl.linkage == Linkage::Export
                || func_decl.linkage == Linkage::Preemptible
            {
                self.waffle_module.exports.push(waffle::Export {
                    name: func_decl.linkage_name(func_id).into_owned(),
                    kind: waffle::ExportKind::Func(self.func_ids[&func_id]),
                });
            }
        }

        self.waffle_module.exports.push(waffle::Export {
            name: "memory".to_owned(),
            kind: waffle::ExportKind::Memory(self.main_memory),
        });

        let (imports, definitions) = mem::take(&mut self.waffle_module.funcs)
            .into_vec()
            .into_iter()
            .enumerate()
            .map(|(func, func_decl)| (waffle::Func::from(func as u32), func_decl))
            .partition::<Vec<_>, _>(|(_func, func_decl)| {
                matches!(func_decl, waffle::FuncDecl::Import(_, _))
            });

        let mut func_remap =
            waffle::entity::PerEntity::<waffle::Func, Option<waffle::Func>>::default();
        for (func, func_decl) in imports {
            func_remap[func] = Some(self.waffle_module.funcs.push(func_decl));
        }
        for (func, func_decl) in definitions {
            func_remap[func] = Some(self.waffle_module.funcs.push(func_decl));
        }

        for func_decl in self.waffle_module.funcs.values_mut() {
            let func_decl = match func_decl {
                waffle::FuncDecl::Import(_, _) => continue,
                waffle::FuncDecl::Body(_, _, func_decl) => func_decl,
                waffle::FuncDecl::Lazy(_, _, _)
                | waffle::FuncDecl::Compiled(_, _, _)
                | waffle::FuncDecl::None => unreachable!(),
            };
            for value in func_decl.values.values_mut() {
                match value {
                    waffle::ValueDef::Operator(op, _, _) => match op {
                        waffle::Operator::Call { function_index } => {
                            *function_index = func_remap[*function_index].unwrap();
                        }
                        _ => {}
                    },
                    waffle::ValueDef::BlockParam(_, _, _)
                    | waffle::ValueDef::PickOutput(_, _, _)
                    | waffle::ValueDef::Alias(_)
                    | waffle::ValueDef::Placeholder(_)
                    | waffle::ValueDef::None => {}
                }
            }
        }

        for import in &mut self.waffle_module.imports {
            match &mut import.kind {
                waffle::ImportKind::Table(_) => {}
                waffle::ImportKind::Func(func) => *func = func_remap[*func].unwrap(),
                waffle::ImportKind::Global(_) => {}
                waffle::ImportKind::Memory(_) => {}
            }
        }

        for export in &mut self.waffle_module.exports {
            match &mut export.kind {
                waffle::ExportKind::Table(_) => {}
                waffle::ExportKind::Func(func) => *func = func_remap[*func].unwrap(),
                waffle::ExportKind::Global(_) => {}
                waffle::ExportKind::Memory(_) => {}
            }
        }

        // FIXME remap imports
        // FIXME remap exports
        // FIXME remap globals

        for (data_id, reloc) in self.data_relocs.drain(..) {
            assert_eq!(reloc.kind, Reloc::Abs4);

            let reloc_loc = self.waffle_module.globals[self.data_ids[&data_id]].value.unwrap()
                as u32
                + reloc.offset;

            let reloc_target = (self.waffle_module.globals[self.data_ids[&match reloc.name {
                ModuleRelocTarget::User { namespace, index } => {
                    assert_eq!(namespace, 1); // FIXME support function targets
                    DataId::from_u32(index)
                }
                ModuleRelocTarget::FunctionOffset(_, _) => todo!(),
                ModuleRelocTarget::LibCall(_libcall) => todo!(),
                ModuleRelocTarget::KnownSymbol(_ks) => todo!(),
            }]]
                .value
                .unwrap() as i32
                + reloc.addend as i32) as u32;

            self.waffle_module.memories[self.main_memory].segments.push(waffle::MemorySegment {
                offset: reloc_loc as usize,
                data: u32::to_le_bytes(reloc_target).to_vec(),
            });
        }

        println!("{}", self.waffle_module.display());

        let data = self.waffle_module.to_wasm_bytes()?;
        writer.write_all(&data)?;
        Ok(())
    }
}

impl Module for WasmModule {
    fn isa(&self) -> &dyn isa::TargetIsa {
        unreachable!();
    }

    fn target_config(&self) -> TargetFrontendConfig {
        TargetFrontendConfig {
            default_call_conv: CallConv::SystemV,
            pointer_width: target_lexicon::PointerWidth::U32,
            page_size_align_log2: 1,
        }
    }

    fn declarations(&self) -> &ModuleDeclarations {
        &self.declarations
    }

    fn declare_function(
        &mut self,
        name: &str,
        linkage: Linkage,
        signature: &cranelift_codegen::ir::Signature,
    ) -> cranelift_module::ModuleResult<FuncId> {
        let (func_id, _linkage) = self.declarations.declare_function(name, linkage, signature)?;

        self.func_ids
            .entry(func_id)
            .or_insert_with(|| self.waffle_module.funcs.push(waffle::FuncDecl::None));

        Ok(func_id)
    }

    fn declare_anonymous_function(
        &mut self,
        signature: &cranelift_codegen::ir::Signature,
    ) -> cranelift_module::ModuleResult<FuncId> {
        let func_id = self.declarations.declare_anonymous_function(signature)?;

        self.func_ids.insert(func_id, self.waffle_module.funcs.push(waffle::FuncDecl::None));

        Ok(func_id)
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: Linkage,
        writable: bool,
        tls: bool,
    ) -> cranelift_module::ModuleResult<cranelift_module::DataId> {
        let (data_id, _linkage) = self.declarations.declare_data(name, linkage, writable, tls)?;

        self.data_ids.entry(data_id).or_insert_with(|| {
            self.waffle_module.globals.push(waffle::GlobalData {
                ty: waffle::Type::I32,
                value: None,
                mutable: false,
            })
        });

        Ok(data_id)
    }

    fn declare_anonymous_data(
        &mut self,
        writable: bool,
        tls: bool,
    ) -> cranelift_module::ModuleResult<cranelift_module::DataId> {
        let data_id = self.declarations.declare_anonymous_data(writable, tls)?;

        self.data_ids.insert(
            data_id,
            self.waffle_module.globals.push(waffle::GlobalData {
                ty: waffle::Type::I32,
                value: None,
                mutable: false,
            }),
        );

        Ok(data_id)
    }

    fn define_function_with_control_plane(
        &mut self,
        func: FuncId,
        ctx: &mut Context,
        _ctrl_plane: &mut cranelift_codegen::control::ControlPlane,
    ) -> cranelift_module::ModuleResult<()> {
        println!("{}", ctx.func.display());

        let mut b = FunctionBuilder::new(
            &self.declarations,
            &ctx.func,
            &mut self.waffle_module,
            self.stack_pointer,
        );

        for block in ctx.func.layout.blocks() {
            for inst in ctx.func.layout.block_insts(block) {
                match ctx.func.dfg.insts[inst].opcode() {
                    Opcode::Nop => {}
                    Opcode::Iconst => {
                        let imm = match ctx.func.dfg.insts[inst] {
                            InstructionData::UnaryImm { opcode: _, imm } => imm,
                            _ => unreachable!(),
                        };
                        b.emit(inst, match b.clif_func.dfg.ctrl_typevar(inst) {
                            types::I8 | types::I16 | types::I32 => {
                                waffle::Operator::I32Const { value: imm.bits() as u32 }
                            }
                            types::I64 => waffle::Operator::I64Const { value: imm.bits() as u64 },
                            _ => unreachable!(),
                        });
                    }
                    Opcode::F32const => {
                        let imm = match ctx.func.dfg.insts[inst] {
                            InstructionData::UnaryIeee32 { opcode: _, imm } => imm,
                            _ => unreachable!(),
                        };
                        b.emit(inst, waffle::Operator::F32Const { value: imm.bits() });
                    }
                    Opcode::F64const => {
                        let imm = match ctx.func.dfg.insts[inst] {
                            InstructionData::UnaryIeee64 { opcode: _, imm } => imm,
                            _ => unreachable!(),
                        };
                        b.emit(inst, waffle::Operator::F64Const { value: imm.bits() });
                    }
                    Opcode::Iadd => {
                        b.emit(inst, match b.clif_func.dfg.ctrl_typevar(inst) {
                            types::I8 | types::I16 | types::I32 => waffle::Operator::I32Add,
                            types::I64 => waffle::Operator::I64Add,
                            _ => unreachable!(),
                        });
                    }
                    Opcode::Imul => {
                        b.emit(inst, match b.clif_func.dfg.ctrl_typevar(inst) {
                            types::I8 | types::I16 | types::I32 => waffle::Operator::I32Mul,
                            types::I64 => waffle::Operator::I64Mul,
                            _ => unreachable!(),
                        });
                    }
                    Opcode::Urem => {
                        b.emit(inst, match b.clif_func.dfg.ctrl_typevar(inst) {
                            types::I8 | types::I16 => todo!(),
                            types::I32 => waffle::Operator::I32RemU,
                            types::I64 => waffle::Operator::I64RemU,
                            _ => unreachable!(),
                        });
                    }
                    Opcode::Uextend => {
                        let arg = b.get_value(b.clif_func.dfg.inst_args(inst)[0]);
                        let ret = b.get_value(b.clif_func.dfg.inst_results(inst)[0]);
                        match (
                            b.clif_func.dfg.value_type(b.clif_func.dfg.inst_args(inst)[0]),
                            b.clif_func.dfg.value_type(b.clif_func.dfg.inst_results(inst)[0]),
                        ) {
                            (types::I8, types::I16 | types::I32) => {
                                let mask = waffle::ValueDef::Operator(
                                    waffle::Operator::I32Const { value: 255 },
                                    b.waffle_func.arg_pool.from_iter([].into_iter()),
                                    b.waffle_func.single_type_list(waffle::Type::I32),
                                );
                                let mask = b.waffle_func.add_value(mask);
                                b.waffle_func.append_to_block(b.block_map[&block], mask);

                                b.assign_multivalue(
                                    block,
                                    waffle::Operator::I32Add,
                                    &[arg, mask],
                                    &[ret],
                                    &[waffle::Type::I32],
                                );
                            }
                            (types::I32, types::I64) => {
                                b.emit(inst, waffle::Operator::I64ExtendI32U)
                            }
                            (from, to) => todo!("uextend {from} -> {to}"),
                        }
                    }
                    Opcode::Ireduce => {
                        let arg = b.get_value(b.clif_func.dfg.inst_args(inst)[0]);
                        let ret = b.get_value(b.clif_func.dfg.inst_results(inst)[0]);
                        match (
                            b.clif_func.dfg.value_type(b.clif_func.dfg.inst_args(inst)[0]),
                            b.clif_func.dfg.value_type(b.clif_func.dfg.inst_results(inst)[0]),
                        ) {
                            (types::I64, types::I8 | types::I16 | types::I32) => {
                                b.assign_multivalue(
                                    block,
                                    waffle::Operator::I32WrapI64,
                                    &[arg],
                                    &[ret],
                                    &[waffle::Type::I32],
                                );
                            }
                            (types::I32, types::I8 | types::I16) => {
                                b.waffle_func.values[ret] = waffle::ValueDef::Alias(arg);
                            }
                            (from, to) => todo!("uextend {from} -> {to}"),
                        }
                    }
                    Opcode::GlobalValue => {
                        let data_id = match ctx.func.dfg.insts[inst] {
                            InstructionData::UnaryGlobalValue { opcode: _, global_value } => {
                                match ctx.func.global_values[global_value].symbol_name() {
                                    ExternalName::User(user) => {
                                        let name = &ctx.func.params.user_named_funcs()[*user];
                                        assert_eq!(name.namespace, 1);
                                        DataId::from_u32(name.index)
                                    }
                                    ExternalName::TestCase(_) => todo!(),
                                    ExternalName::LibCall(_libcall) => todo!(),
                                    ExternalName::KnownSymbol(_ks) => todo!(),
                                }
                            }
                            _ => unreachable!(),
                        };

                        b.emit(inst, waffle::Operator::GlobalGet {
                            global_index: self.data_ids[&data_id],
                        });
                    }
                    Opcode::StackAddr => {
                        let (stack_slot, offset): (_, i32) = match ctx.func.dfg.insts[inst] {
                            InstructionData::StackLoad { opcode: _, stack_slot, offset } => {
                                (stack_slot, offset.into())
                            }
                            _ => unreachable!(),
                        };

                        let stack_offset = waffle::ValueDef::Operator(
                            waffle::Operator::I32Const {
                                value: (b.stack_map[&stack_slot] as i32 + offset) as u32,
                            },
                            b.waffle_func.arg_pool.from_iter([].into_iter()),
                            b.waffle_func.single_type_list(waffle::Type::I32),
                        );
                        let stack_offset = b.waffle_func.add_value(stack_offset);
                        b.waffle_func.append_to_block(b.block_map[&block], stack_offset);

                        let ret = b.get_value(b.clif_func.dfg.inst_results(inst)[0]);
                        b.assign_multivalue(
                            block,
                            waffle::Operator::I32Add,
                            &[b.stack_value, stack_offset],
                            &[ret],
                            &[waffle::Type::I32],
                        );
                    }
                    Opcode::StackStore => {
                        let (arg, stack_slot, offset): (_, _, i32) = match ctx.func.dfg.insts[inst]
                        {
                            InstructionData::StackStore { opcode: _, arg, stack_slot, offset } => {
                                (arg, stack_slot, offset.into())
                            }
                            _ => unreachable!(),
                        };

                        let ty = b.clif_func.dfg.ctrl_typevar(inst);
                        let arg = b.get_value(arg);
                        let memory = waffle::MemoryArg {
                            align: 0,
                            offset: (b.stack_map[&stack_slot] as i32 + offset) as u32,
                            memory: self.main_memory,
                        };
                        b.assign_multivalue(
                            block,
                            match ty {
                                types::I8 => waffle::Operator::I32Store8 { memory },
                                types::I16 => waffle::Operator::I32Store16 { memory },
                                types::I32 => waffle::Operator::I32Store { memory },
                                types::I64 => waffle::Operator::I64Store { memory },
                                types::F32 => waffle::Operator::F32Store { memory },
                                types::F64 => waffle::Operator::F64Store { memory },
                                _ => unreachable!(),
                            },
                            &[b.stack_value, arg],
                            &[],
                            &[],
                        );
                    }
                    Opcode::StackLoad => {
                        let (stack_slot, offset): (_, i32) = match ctx.func.dfg.insts[inst] {
                            InstructionData::StackLoad { opcode: _, stack_slot, offset } => {
                                (stack_slot, offset.into())
                            }
                            _ => unreachable!(),
                        };

                        let ty = b.clif_func.dfg.ctrl_typevar(inst);
                        let memory = waffle::MemoryArg {
                            align: 0,
                            offset: (b.stack_map[&stack_slot] as i32 + offset) as u32,
                            memory: self.main_memory,
                        };
                        let ret = b.get_value(b.clif_func.dfg.inst_results(inst)[0]);
                        b.assign_multivalue(
                            block,
                            match ty {
                                types::I8 => waffle::Operator::I32Load8U { memory },
                                types::I16 => waffle::Operator::I32Load16U { memory },
                                types::I32 => waffle::Operator::I32Load { memory },
                                types::I64 => waffle::Operator::I64Load { memory },
                                types::F32 => waffle::Operator::F32Load { memory },
                                types::F64 => waffle::Operator::F64Load { memory },
                                _ => unreachable!(),
                            },
                            &[b.stack_value],
                            &[ret],
                            &[FunctionBuilder::translate_ty(ty)],
                        );
                    }
                    Opcode::Load => {
                        let (arg, offset): (_, i32) = match ctx.func.dfg.insts[inst] {
                            InstructionData::Load { opcode: _, arg, offset, flags: _ } => {
                                (arg, offset.into())
                            }
                            _ => unreachable!(),
                        };

                        let ty = b.clif_func.dfg.ctrl_typevar(inst);
                        let arg = b.get_value(arg);
                        let memory = waffle::MemoryArg {
                            align: 0,
                            offset: offset as u32,
                            memory: self.main_memory,
                        };
                        let ret = b.get_value(b.clif_func.dfg.inst_results(inst)[0]);
                        b.assign_multivalue(
                            block,
                            match ty {
                                types::I8 => waffle::Operator::I32Load8U { memory },
                                types::I16 => waffle::Operator::I32Load16U { memory },
                                types::I32 => waffle::Operator::I32Load { memory },
                                types::I64 => waffle::Operator::I64Load { memory },
                                types::F32 => waffle::Operator::F32Load { memory },
                                types::F64 => waffle::Operator::F64Load { memory },
                                _ => unreachable!(),
                            },
                            &[arg],
                            &[ret],
                            &[FunctionBuilder::translate_ty(ty)],
                        );
                    }
                    Opcode::Icmp => {
                        let cond = match ctx.func.dfg.insts[inst] {
                            InstructionData::IntCompare { opcode: _, args: _, cond } => cond,
                            _ => unreachable!(),
                        };

                        let ty = b.clif_func.dfg.ctrl_typevar(inst);
                        match ty {
                            types::I8 | types::I16 => unimplemented!(),
                            types::I32 => {
                                b.emit(inst, match cond {
                                    IntCC::Equal => waffle::Operator::I32Eq,
                                    IntCC::NotEqual => waffle::Operator::I32Ne,
                                    IntCC::SignedLessThan => waffle::Operator::I32LtS,
                                    IntCC::SignedGreaterThanOrEqual => waffle::Operator::I32GeS,
                                    IntCC::SignedGreaterThan => waffle::Operator::I32GtS,
                                    IntCC::SignedLessThanOrEqual => waffle::Operator::I32LeS,
                                    IntCC::UnsignedLessThan => waffle::Operator::I32LtU,
                                    IntCC::UnsignedGreaterThanOrEqual => waffle::Operator::I32GeU,
                                    IntCC::UnsignedGreaterThan => waffle::Operator::I32GtU,
                                    IntCC::UnsignedLessThanOrEqual => waffle::Operator::I32LeU,
                                });
                            }
                            types::I64 => todo!(),
                            _ => unreachable!(),
                        }
                    }
                    Opcode::IcmpImm => {
                        let (arg, cond, imm) = match ctx.func.dfg.insts[inst] {
                            InstructionData::IntCompareImm { opcode: _, arg, cond, imm } => {
                                (arg, cond, imm)
                            }
                            _ => unreachable!(),
                        };

                        let ty = b.clif_func.dfg.ctrl_typevar(inst);
                        let arg = b.get_value(arg);
                        let ret = b.get_value(b.clif_func.dfg.inst_results(inst)[0]);
                        match ty {
                            types::I8 | types::I16 => unimplemented!(),
                            types::I32 => {
                                let imm = waffle::ValueDef::Operator(
                                    waffle::Operator::I32Const { value: imm.bits() as u32 },
                                    b.waffle_func.arg_pool.from_iter([].into_iter()),
                                    b.waffle_func.single_type_list(waffle::Type::I32),
                                );
                                let imm = b.waffle_func.add_value(imm);
                                b.waffle_func.append_to_block(b.block_map[&block], imm);

                                b.assign_multivalue(
                                    block,
                                    match cond {
                                        IntCC::Equal => waffle::Operator::I32Eq,
                                        IntCC::NotEqual => waffle::Operator::I32Ne,
                                        IntCC::SignedLessThan => waffle::Operator::I32LtS,
                                        IntCC::SignedGreaterThanOrEqual => waffle::Operator::I32GeS,
                                        IntCC::SignedGreaterThan => waffle::Operator::I32GtS,
                                        IntCC::SignedLessThanOrEqual => waffle::Operator::I32LeS,
                                        IntCC::UnsignedLessThan => waffle::Operator::I32LtU,
                                        IntCC::UnsignedGreaterThanOrEqual => {
                                            waffle::Operator::I32GeU
                                        }
                                        IntCC::UnsignedGreaterThan => waffle::Operator::I32GtU,
                                        IntCC::UnsignedLessThanOrEqual => waffle::Operator::I32LeU,
                                    },
                                    &[arg, imm],
                                    &[ret],
                                    &[waffle::Type::I32],
                                );
                            }
                            types::I64 => {
                                let imm = waffle::ValueDef::Operator(
                                    waffle::Operator::I64Const { value: imm.bits() as u64 },
                                    b.waffle_func.arg_pool.from_iter([].into_iter()),
                                    b.waffle_func.single_type_list(waffle::Type::I64),
                                );
                                let imm = b.waffle_func.add_value(imm);
                                b.waffle_func.append_to_block(b.block_map[&block], imm);

                                b.assign_multivalue(
                                    block,
                                    match cond {
                                        IntCC::Equal => waffle::Operator::I64Eq,
                                        IntCC::NotEqual => waffle::Operator::I64Ne,
                                        IntCC::SignedLessThan => waffle::Operator::I64LtS,
                                        IntCC::SignedGreaterThanOrEqual => waffle::Operator::I64GeS,
                                        IntCC::SignedGreaterThan => waffle::Operator::I64GtS,
                                        IntCC::SignedLessThanOrEqual => waffle::Operator::I64LeS,
                                        IntCC::UnsignedLessThan => waffle::Operator::I64LtU,
                                        IntCC::UnsignedGreaterThanOrEqual => {
                                            waffle::Operator::I32GeU
                                        }
                                        IntCC::UnsignedGreaterThan => waffle::Operator::I64GtU,
                                        IntCC::UnsignedLessThanOrEqual => waffle::Operator::I64LeU,
                                    },
                                    &[arg, imm],
                                    &[ret],
                                    &[waffle::Type::I32],
                                );
                            }
                            _ => unreachable!(),
                        }
                    }
                    Opcode::Call => match ctx.func.dfg.insts[inst] {
                        InstructionData::Call { opcode: _, args: _, func_ref } => {
                            match ctx.func.dfg.ext_funcs[func_ref].name {
                                ExternalName::User(user) => {
                                    let name = &ctx.func.params.user_named_funcs()[user];
                                    assert_eq!(name.namespace, 0);
                                    let func_id = FuncId::from_u32(name.index);
                                    b.emit(inst, waffle::Operator::Call {
                                        function_index: self.func_ids[&func_id],
                                    });
                                }
                                ExternalName::TestCase(_) => todo!(),
                                ExternalName::LibCall(LibCall::Memcpy | LibCall::Memmove) => {
                                    let block = b.clif_func.layout.inst_block(inst).unwrap();

                                    let args = b
                                        .clif_func
                                        .dfg
                                        .inst_args(inst)
                                        .into_iter()
                                        .map(|&arg| b.get_value(arg))
                                        .collect::<Vec<_>>();

                                    let ret_val =
                                        b.get_value(b.clif_func.dfg.inst_results(inst)[0]);

                                    let copy = waffle::ValueDef::Operator(
                                        waffle::Operator::MemoryCopy {
                                            dst_mem: self.main_memory,
                                            src_mem: self.main_memory,
                                        },
                                        b.waffle_func.arg_pool.from_iter(args.iter().copied()),
                                        b.waffle_func.type_pool.from_iter([].into_iter()),
                                    );
                                    let copy = b.waffle_func.add_value(copy);
                                    b.waffle_func.append_to_block(b.block_map[&block], copy);

                                    b.waffle_func.values[ret_val] =
                                        waffle::ValueDef::Alias(args[0]);
                                }
                                ExternalName::LibCall(LibCall::Memset) => {
                                    let block = b.clif_func.layout.inst_block(inst).unwrap();

                                    let args = b
                                        .clif_func
                                        .dfg
                                        .inst_args(inst)
                                        .into_iter()
                                        .map(|&arg| b.get_value(arg))
                                        .collect::<Vec<_>>();

                                    let ret_val =
                                        b.get_value(b.clif_func.dfg.inst_results(inst)[0]);

                                    let copy = waffle::ValueDef::Operator(
                                        waffle::Operator::MemoryFill { mem: self.main_memory },
                                        b.waffle_func.arg_pool.from_iter(args.iter().copied()),
                                        b.waffle_func.type_pool.from_iter([].into_iter()),
                                    );
                                    let copy = b.waffle_func.add_value(copy);
                                    b.waffle_func.append_to_block(b.block_map[&block], copy);

                                    b.waffle_func.values[ret_val] =
                                        waffle::ValueDef::Alias(args[0]);
                                }
                                ExternalName::LibCall(_libcall) => todo!(),
                                ExternalName::KnownSymbol(_ks) => todo!(),
                            }
                        }
                        _ => unreachable!(),
                    },
                    Opcode::Jump => {
                        let target_block = ctx.func.dfg.insts[inst]
                            .branch_destination(&ctx.func.dfg.jump_tables)[0];
                        let target = b.translate_block_call(target_block);
                        b.waffle_func
                            .set_terminator(b.block_map[&block], waffle::Terminator::Br { target });
                    }
                    Opcode::Brif => {
                        let target_blocks =
                            ctx.func.dfg.insts[inst].branch_destination(&ctx.func.dfg.jump_tables);
                        let cond = b.get_value(b.clif_func.dfg.inst_args(inst)[0]);
                        let if_true = b.translate_block_call(target_blocks[0]);
                        let if_false = b.translate_block_call(target_blocks[1]);
                        b.waffle_func.set_terminator(
                            b.block_map[&block],
                            waffle::Terminator::CondBr { cond, if_true, if_false },
                        );
                    }
                    Opcode::Trap => {
                        b.waffle_func
                            .set_terminator(b.block_map[&block], waffle::Terminator::Unreachable);
                    }
                    Opcode::Return => {
                        let returns = ctx.func.dfg.insts[inst].arguments(&ctx.func.dfg.value_lists);
                        let returns =
                            returns.into_iter().map(|&arg| b.get_value(arg)).collect::<Vec<_>>();

                        b.restore_stack(b.block_map[&block]);

                        b.waffle_func
                            .set_terminator(b.block_map[&block], waffle::Terminator::Return {
                                values: returns,
                            });
                    }
                    _ => {
                        println!("{}", b.waffle_func.display("", Some(&b.waffle_module)));

                        panic!("[{block}] {}", ctx.func.dfg.display_inst(inst));
                    }
                }
            }
        }

        b.waffle_func.validate().unwrap();

        self.waffle_module.funcs[self.func_ids[&func]] = waffle::FuncDecl::Body(
            b.waffle_sig,
            self.declarations.get_function_decl(func).linkage_name(func).into_owned(),
            b.waffle_func,
        );

        //todo!()
        Ok(())
    }

    fn define_function_bytes(
        &mut self,
        func_id: FuncId,
        func: &cranelift_codegen::ir::Function,
        alignment: u64,
        bytes: &[u8],
        relocs: &[cranelift_codegen::FinalizedMachReloc],
    ) -> cranelift_module::ModuleResult<()> {
        unreachable!();
    }

    fn define_data(
        &mut self,
        data_id: cranelift_module::DataId,
        data: &DataDescription,
    ) -> cranelift_module::ModuleResult<()> {
        let global = self.data_ids[&data_id];

        let align = data.align.unwrap_or(1);
        self.next_data_offset = (self.next_data_offset + align - 1) & !(align - 1);

        match &data.init {
            cranelift_module::Init::Uninitialized => unreachable!(),
            cranelift_module::Init::Zeros { size: _ } => {}
            cranelift_module::Init::Bytes { contents } => {
                self.waffle_module.memories[self.main_memory].segments.push(waffle::MemorySegment {
                    offset: self.next_data_offset as usize,
                    data: contents.to_vec(),
                })
            }
        }

        self.waffle_module.globals[global].value = Some(self.next_data_offset);
        self.next_data_offset += data.init.size() as u64;

        self.data_relocs.extend(data.all_relocs(Reloc::Abs4).map(|reloc| (data_id, reloc)));

        Ok(())
    }
}

struct FunctionBuilder<'a, 'b> {
    // Source
    declarations: &'a ModuleDeclarations,
    clif_func: &'a Function,

    // Destination
    waffle_module: &'b mut waffle::Module<'static>,
    waffle_func: waffle::FunctionBody,
    waffle_sig: waffle::Signature,
    stack_pointer: waffle::Global,

    // Translation context
    block_map: FxHashMap<Block, waffle::Block>,
    value_map: FxHashMap<Value, waffle::Value>,
    stack_map: FxHashMap<StackSlot, u32>,
    stack_size: u32,
    stack_value: waffle::Value,
}

impl<'a, 'b> FunctionBuilder<'a, 'b> {
    fn new(
        declarations: &'a ModuleDeclarations,
        clif_func: &'a Function,
        waffle_module: &'b mut waffle::Module<'static>,
        stack_pointer: waffle::Global,
    ) -> Self {
        let waffle_sig = Self::translate_signature(waffle_module, &clif_func.signature);

        let mut waffle_func = waffle::FunctionBody::new(waffle_module, waffle_sig);

        let mut block_map = FxHashMap::default();
        let mut value_map = FxHashMap::default();
        for block in clif_func.layout.blocks() {
            if block == clif_func.layout.entry_block().unwrap() {
                let waffle_block = waffle_func.entry;
                block_map.insert(block, waffle_block);

                for (i, &param) in clif_func.dfg.block_params(block).iter().enumerate() {
                    value_map.insert(param, waffle_func.blocks[waffle_block].params[i].1);
                }
            } else {
                let waffle_block = waffle_func.add_block();
                block_map.insert(block, waffle_block);

                for &param in clif_func.dfg.block_params(block) {
                    value_map.insert(
                        param,
                        waffle_func.add_blockparam(
                            waffle_block,
                            FunctionBuilder::translate_ty(clif_func.dfg.value_type(param)),
                        ),
                    );
                }
            };
        }

        let mut stack_map = FxHashMap::default();
        let mut stack_size = 0;
        for (stack_slot, stack_slot_data) in &clif_func.sized_stack_slots {
            // FIXME handle alignment
            stack_map.insert(stack_slot, stack_size);
            stack_size += stack_slot_data.size;
        }

        let get_stack = waffle::ValueDef::Operator(
            waffle::Operator::GlobalGet { global_index: stack_pointer },
            waffle_func.arg_pool.from_iter([].into_iter()),
            waffle_func.single_type_list(waffle::Type::I32),
        );
        let get_stack = waffle_func.add_value(get_stack);
        waffle_func.append_to_block(waffle_func.entry, get_stack);

        let stack_size_val = waffle::ValueDef::Operator(
            waffle::Operator::I32Const { value: stack_size },
            waffle_func.arg_pool.from_iter([].into_iter()),
            waffle_func.single_type_list(waffle::Type::I32),
        );
        let stack_size_val = waffle_func.add_value(stack_size_val);
        waffle_func.append_to_block(waffle_func.entry, stack_size_val);

        let sub_stack = waffle::ValueDef::Operator(
            waffle::Operator::I32Sub,
            waffle_func.arg_pool.from_iter([get_stack, stack_size_val].into_iter()),
            waffle_func.single_type_list(waffle::Type::I32),
        );
        let sub_stack = waffle_func.add_value(sub_stack);
        waffle_func.append_to_block(waffle_func.entry, sub_stack);

        let set_stack = waffle::ValueDef::Operator(
            waffle::Operator::GlobalSet { global_index: stack_pointer },
            waffle_func.arg_pool.from_iter([sub_stack].into_iter()),
            waffle_func.type_pool.from_iter([].into_iter()),
        );
        let set_stack = waffle_func.add_value(set_stack);
        waffle_func.append_to_block(waffle_func.entry, set_stack);

        FunctionBuilder {
            declarations,
            clif_func,
            waffle_module,
            waffle_func,
            waffle_sig,
            stack_pointer,
            block_map,
            value_map,
            stack_map,
            stack_size,
            stack_value: sub_stack,
        }
    }

    fn restore_stack(&mut self, block: waffle::Block) {
        let stack_size_val = waffle::ValueDef::Operator(
            waffle::Operator::I32Const { value: self.stack_size },
            self.waffle_func.arg_pool.from_iter([].into_iter()),
            self.waffle_func.single_type_list(waffle::Type::I32),
        );
        let stack_size_val = self.waffle_func.add_value(stack_size_val);
        self.waffle_func.append_to_block(block, stack_size_val);

        let add_stack = waffle::ValueDef::Operator(
            waffle::Operator::I32Add,
            self.waffle_func.arg_pool.from_iter([self.stack_value, stack_size_val].into_iter()),
            self.waffle_func.single_type_list(waffle::Type::I32),
        );
        let add_stack = self.waffle_func.add_value(add_stack);
        self.waffle_func.append_to_block(block, add_stack);

        let set_stack = waffle::ValueDef::Operator(
            waffle::Operator::GlobalSet { global_index: self.stack_pointer },
            self.waffle_func.arg_pool.from_iter([add_stack].into_iter()),
            self.waffle_func.type_pool.from_iter([].into_iter()),
        );
        let set_stack = self.waffle_func.add_value(set_stack);
        self.waffle_func.append_to_block(block, set_stack);
    }

    fn translate_ty(ty: types::Type) -> waffle::Type {
        match ty {
            types::I8 | types::I16 | types::I32 => waffle::Type::I32,
            types::I64 => waffle::Type::I64,
            types::F32 => waffle::Type::F32,
            types::F64 => waffle::Type::F64,
            _ => unreachable!(),
        }
    }

    fn translate_signature(
        waffle_module: &mut waffle::Module<'static>,
        signature: &Signature,
    ) -> waffle::Signature {
        waffle_module.signatures.push(waffle::SignatureData {
            params: signature
                .params
                .iter()
                .map(|param| FunctionBuilder::translate_ty(param.value_type))
                .collect(),
            returns: signature
                .returns
                .iter()
                .map(|param| FunctionBuilder::translate_ty(param.value_type))
                .collect(),
        })
    }

    fn translate_block_call(&mut self, block_call: BlockCall) -> waffle::BlockTarget {
        waffle::BlockTarget {
            block: self.block_map[&block_call.block(&self.clif_func.dfg.value_lists)],
            args: block_call
                .args_slice(&self.clif_func.dfg.value_lists)
                .into_iter()
                .map(|&arg| self.get_value(arg))
                .collect::<Vec<_>>(),
        }
    }

    fn get_value(&mut self, value: Value) -> waffle::Value {
        let value = self.clif_func.dfg.resolve_aliases(value);

        *self.value_map.entry(value).or_insert_with(|| {
            self.waffle_func.add_value(waffle::ValueDef::None /* FIXME use Placeholder */)
        })
    }

    fn assign_multivalue(
        &mut self,
        block: Block,
        operator: waffle::Operator,
        args: &[waffle::Value],
        ret_vals: &[waffle::Value],
        ret_tys: &[waffle::Type],
    ) {
        if ret_tys.len() == 1 {
            self.waffle_func.values[ret_vals[0]] = waffle::ValueDef::Operator(
                operator,
                self.waffle_func.arg_pool.from_iter(args.into_iter().cloned()),
                self.waffle_func.type_pool.from_iter(ret_tys.into_iter().cloned()),
            );

            self.waffle_func.append_to_block(self.block_map[&block], ret_vals[0]);
        } else {
            let res_def = waffle::ValueDef::Operator(
                operator,
                self.waffle_func.arg_pool.from_iter(args.into_iter().cloned()),
                self.waffle_func.type_pool.from_iter(ret_tys.into_iter().cloned()),
            );
            let res = self.waffle_func.add_value(res_def);
            self.waffle_func.append_to_block(self.block_map[&block], res);

            for (i, (&res_val, &ret_ty)) in ret_vals.into_iter().zip(ret_tys).enumerate() {
                self.waffle_func.values[res_val] =
                    waffle::ValueDef::PickOutput(res, i as u32, ret_ty);
                self.waffle_func.append_to_block(self.block_map[&block], res_val);
            }
        }
    }

    fn emit(&mut self, inst: Inst, operator: waffle::Operator) {
        let block = self.clif_func.layout.inst_block(inst).unwrap();

        let args = self
            .clif_func
            .dfg
            .inst_args(inst)
            .into_iter()
            .map(|&arg| self.get_value(arg))
            .collect::<Vec<_>>();

        let mut ret_vals = vec![];
        let mut ret_tys = vec![];
        for &res in self.clif_func.dfg.inst_results(inst) {
            ret_vals.push(self.get_value(res));
            ret_tys.push(FunctionBuilder::translate_ty(self.clif_func.dfg.value_type(res)));
        }

        self.assign_multivalue(block, operator, &args, &ret_vals, &ret_tys);
    }
}
