//! The AOT driver uses [`cranelift_object`] to write object files suitable for linking into a
//! standalone executable.

use std::fs::File;

use cranelift_codegen::ir::{ExternalName, InstructionData, Opcode, ValueDef};
use cranelift_codegen::isa::{self, CallConv, TargetFrontendConfig};
use cranelift_module::{ModuleDeclarations, ModuleRelocTarget};
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
}

impl WasmModule {
    fn new() -> Self {
        WasmModule {
            waffle_module: waffle::Module::empty(),
            declarations: ModuleDeclarations::default(),
            func_ids: FxHashMap::default(),
        }
    }

    fn write_stream<W: std::io::Write>(
        mut self,
        mut writer: W,
    ) -> Result<(), Box<dyn std::error::Error>> {
        for (func_id, func_decl) in self.declarations.get_functions() {
            if func_decl.linkage == Linkage::Import {
                self.waffle_module.imports.push(waffle::Import {
                    module: "env".to_owned(),
                    name: func_decl.linkage_name(func_id).into_owned(),
                    kind: waffle::ImportKind::Func(self.func_ids[&func_id]),
                });
            } else if func_decl.linkage == Linkage::Export
                || func_decl.linkage == Linkage::Preemptible
            {
                self.waffle_module.exports.push(waffle::Export {
                    name: func_decl.linkage_name(func_id).into_owned(),
                    kind: waffle::ExportKind::Func(self.func_ids[&func_id]),
                });
            }
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

        Ok(data_id)
    }

    fn declare_anonymous_data(
        &mut self,
        writable: bool,
        tls: bool,
    ) -> cranelift_module::ModuleResult<cranelift_module::DataId> {
        let data_id = self.declarations.declare_anonymous_data(writable, tls)?;

        Ok(data_id)
    }

    fn define_function_with_control_plane(
        &mut self,
        func: FuncId,
        ctx: &mut Context,
        _ctrl_plane: &mut cranelift_codegen::control::ControlPlane,
    ) -> cranelift_module::ModuleResult<()> {
        fn translate_ty(ty: types::Type) -> waffle::Type {
            match ty {
                types::I8 | types::I16 | types::I32 => waffle::Type::I32,
                types::I64 => waffle::Type::I64,
                types::F32 => waffle::Type::F32,
                types::F64 => waffle::Type::F64,
                _ => unreachable!(),
            }
        }

        let waffle_sig = self.waffle_module.signatures.push(waffle::SignatureData {
            params: ctx
                .func
                .signature
                .params
                .iter()
                .map(|param| translate_ty(param.value_type))
                .collect(),
            returns: ctx
                .func
                .signature
                .returns
                .iter()
                .map(|param| translate_ty(param.value_type))
                .collect(),
        });

        let mut func_body = waffle::FunctionBody::new(&self.waffle_module, waffle_sig);

        let mut block_map = FxHashMap::default();
        let mut value_map = FxHashMap::default();
        for block in ctx.func.layout.blocks() {
            if block == ctx.func.layout.entry_block().unwrap() {
                let waffle_block = func_body.entry;
                block_map.insert(block, waffle_block);

                for (i, &param) in ctx.func.dfg.block_params(block).iter().enumerate() {
                    value_map.insert(param, func_body.blocks[waffle_block].params[i].1);
                }
            } else {
                let waffle_block = func_body.add_block();
                block_map.insert(block, waffle_block);

                for &param in ctx.func.dfg.block_params(block) {
                    value_map.insert(
                        param,
                        func_body.add_blockparam(
                            waffle_block,
                            translate_ty(ctx.func.dfg.value_type(param)),
                        ),
                    );
                }
            };
        }

        for block in ctx.func.layout.blocks() {
            for inst in ctx.func.layout.block_insts(block) {
                println!("[{block}] {}", ctx.func.dfg.display_inst(inst));
                match ctx.func.dfg.insts[inst].opcode() {
                    Opcode::Nop => {}
                    Opcode::Iconst => {
                        let res = *value_map
                            .entry(ctx.func.dfg.inst_results(inst)[0])
                            .or_insert_with(|| func_body.add_value(waffle::ValueDef::None));

                        func_body.values[res] = waffle::ValueDef::Operator(
                            waffle::Operator::I32Const {
                                value: match ctx.func.dfg.insts[inst] {
                                    InstructionData::UnaryImm { opcode: _, imm } => {
                                        imm.bits() as u32
                                    }
                                    _ => unreachable!(),
                                },
                            },
                            func_body.arg_pool.from_iter([].into_iter()),
                            func_body.single_type_list(waffle::Type::I32),
                        );

                        func_body.append_to_block(block_map[&block], res);
                    }
                    Opcode::Iadd => {
                        let res = *value_map
                            .entry(ctx.func.dfg.inst_results(inst)[0])
                            .or_insert_with(|| func_body.add_value(waffle::ValueDef::None));
                        let args = ctx.func.dfg.inst_args(inst);
                        let lhs = value_map[&ctx.func.dfg.resolve_aliases(args[0])];
                        let rhs = value_map[&ctx.func.dfg.resolve_aliases(args[1])];

                        func_body.values[res] = waffle::ValueDef::Operator(
                            waffle::Operator::I32Add,
                            func_body.arg_pool.from_iter([lhs, rhs].into_iter()),
                            func_body.single_type_list(waffle::Type::I32),
                        );

                        func_body.append_to_block(block_map[&block], res);
                    }
                    Opcode::Call => {
                        let (args, func_id) = match ctx.func.dfg.insts[inst] {
                            InstructionData::Call { opcode: _, args, func_ref } => (
                                args.as_slice(&ctx.func.dfg.value_lists),
                                match ctx.func.dfg.ext_funcs[func_ref].name {
                                    ExternalName::User(user) => {
                                        let name = &ctx.func.params.user_named_funcs()[user];
                                        assert_eq!(name.namespace, 0);
                                        FuncId::from_u32(name.index)
                                    }
                                    ExternalName::TestCase(_) => todo!(),
                                    ExternalName::LibCall(libcall) => todo!(),
                                    ExternalName::KnownSymbol(ks) => todo!(),
                                },
                            ),
                            _ => unreachable!(),
                        };

                        let mut res_vals = vec![];
                        let mut ret_tys = vec![];
                        for &res in ctx.func.dfg.inst_results(inst) {
                            res_vals.push(
                                *value_map
                                    .entry(res)
                                    .or_insert_with(|| func_body.add_value(waffle::ValueDef::None)),
                            );
                            ret_tys.push(translate_ty(ctx.func.dfg.value_type(res)));
                        }

                        let args = ctx
                            .func
                            .dfg
                            .inst_args(inst)
                            .into_iter()
                            .map(|&arg| value_map[&ctx.func.dfg.resolve_aliases(arg)]);

                        if res_vals.len() == 1 {
                            func_body.values[res_vals[0]] = waffle::ValueDef::Operator(
                                waffle::Operator::Call { function_index: self.func_ids[&func_id] },
                                func_body.arg_pool.from_iter(args),
                                func_body.type_pool.from_iter(ret_tys.into_iter()),
                            );

                            func_body.append_to_block(block_map[&block], res_vals[0]);
                        } else {
                            let res_def = waffle::ValueDef::Operator(
                                waffle::Operator::Call { function_index: self.func_ids[&func_id] },
                                func_body.arg_pool.from_iter(args),
                                func_body.type_pool.from_iter(ret_tys.iter().cloned()),
                            );
                            let res = func_body.add_value(res_def);
                            func_body.append_to_block(block_map[&block], res);

                            for (i, (res_val, ret_ty)) in
                                res_vals.into_iter().zip(ret_tys).enumerate()
                            {
                                func_body.values[res_val] =
                                    waffle::ValueDef::PickOutput(res, i as u32, ret_ty);
                                func_body.append_to_block(block_map[&block], res_val);
                            }
                        }
                    }
                    Opcode::Jump => {
                        let target_block = ctx.func.dfg.insts[inst]
                            .branch_destination(&ctx.func.dfg.jump_tables)[0];
                        func_body.set_terminator(block_map[&block], waffle::Terminator::Br {
                            target: waffle::BlockTarget {
                                block: block_map[&target_block.block(&ctx.func.dfg.value_lists)],
                                args: vec![/* TODO */],
                            },
                        });
                    }
                    Opcode::Return => {
                        let returns = ctx.func.dfg.insts[inst].arguments(&ctx.func.dfg.value_lists);
                        let returns = returns
                            .into_iter()
                            .map(|&arg| value_map[&ctx.func.dfg.resolve_aliases(arg)])
                            .collect::<Vec<_>>();
                        func_body.set_terminator(block_map[&block], waffle::Terminator::Return {
                            values: returns,
                        });
                    }
                    _ => {}
                }
            }
        }

        // FIXME fill body

        func_body.validate().unwrap();

        self.waffle_module.funcs[self.func_ids[&func]] = waffle::FuncDecl::Body(
            waffle_sig,
            self.declarations.get_function_decl(func).linkage_name(func).into_owned(),
            func_body,
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
        todo!()
    }
}
