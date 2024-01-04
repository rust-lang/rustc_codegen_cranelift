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
        let mut b = FunctionBuilder::new(&self.declarations, &ctx.func, &mut self.waffle_module);

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
                    Opcode::Iadd => {
                        b.emit(inst, match b.clif_func.dfg.ctrl_typevar(inst) {
                            types::I8 | types::I16 | types::I32 => waffle::Operator::I32Add,
                            types::I64 => waffle::Operator::I64Add,
                            _ => unreachable!(),
                        });
                    }
                    Opcode::Call => {
                        let func_id = match ctx.func.dfg.insts[inst] {
                            InstructionData::Call { opcode: _, args: _, func_ref } => {
                                match ctx.func.dfg.ext_funcs[func_ref].name {
                                    ExternalName::User(user) => {
                                        let name = &ctx.func.params.user_named_funcs()[user];
                                        assert_eq!(name.namespace, 0);
                                        FuncId::from_u32(name.index)
                                    }
                                    ExternalName::TestCase(_) => todo!(),
                                    ExternalName::LibCall(libcall) => todo!(),
                                    ExternalName::KnownSymbol(ks) => todo!(),
                                }
                            }
                            _ => unreachable!(),
                        };

                        b.emit(inst, waffle::Operator::Call {
                            function_index: self.func_ids[&func_id],
                        });
                    }
                    Opcode::Jump => {
                        let target_block = ctx.func.dfg.insts[inst]
                            .branch_destination(&ctx.func.dfg.jump_tables)[0];
                        b.waffle_func.set_terminator(b.block_map[&block], waffle::Terminator::Br {
                            target: waffle::BlockTarget {
                                block: b.block_map[&target_block.block(&ctx.func.dfg.value_lists)],
                                args: vec![/* TODO */],
                            },
                        });
                    }
                    Opcode::Return => {
                        let returns = ctx.func.dfg.insts[inst].arguments(&ctx.func.dfg.value_lists);
                        let returns = returns
                            .into_iter()
                            .map(|&arg| b.value_map[&ctx.func.dfg.resolve_aliases(arg)])
                            .collect::<Vec<_>>();
                        b.waffle_func
                            .set_terminator(b.block_map[&block], waffle::Terminator::Return {
                                values: returns,
                            });
                    }
                    _ => {
                        panic!("[{block}] {}", ctx.func.dfg.display_inst(inst));
                    }
                }
            }
        }

        // FIXME fill body

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
        todo!()
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

    // Translation context
    block_map: FxHashMap<Block, waffle::Block>,
    value_map: FxHashMap<Value, waffle::Value>,
}

impl<'a, 'b> FunctionBuilder<'a, 'b> {
    fn new(
        declarations: &'a ModuleDeclarations,
        clif_func: &'a Function,
        waffle_module: &'b mut waffle::Module<'static>,
    ) -> Self {
        let waffle_sig = waffle_module.signatures.push(waffle::SignatureData {
            params: clif_func
                .signature
                .params
                .iter()
                .map(|param| FunctionBuilder::translate_ty(param.value_type))
                .collect(),
            returns: clif_func
                .signature
                .returns
                .iter()
                .map(|param| FunctionBuilder::translate_ty(param.value_type))
                .collect(),
        });

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

        FunctionBuilder {
            declarations,
            clif_func,
            waffle_module,
            waffle_func,
            waffle_sig,
            block_map,
            value_map,
        }
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

    fn get_value(&mut self, value: Value) -> waffle::Value {
        let value = self.clif_func.dfg.resolve_aliases(value);

        *self
            .value_map
            .entry(value)
            .or_insert_with(|| self.waffle_func.add_value(waffle::ValueDef::None))
    }

    fn emit(&mut self, inst: Inst, operator: waffle::Operator) {
        let block = self.clif_func.layout.inst_block(inst).unwrap();

        let args = self
            .clif_func
            .dfg
            .inst_args(inst)
            .into_iter()
            .map(|&arg| self.get_value(self.clif_func.dfg.resolve_aliases(arg)))
            .collect::<Vec<_>>();

        let mut res_vals = vec![];
        let mut ret_tys = vec![];
        for &res in self.clif_func.dfg.inst_results(inst) {
            res_vals.push(self.get_value(res));
            ret_tys.push(FunctionBuilder::translate_ty(self.clif_func.dfg.value_type(res)));
        }

        if res_vals.len() == 1 {
            self.waffle_func.values[res_vals[0]] = waffle::ValueDef::Operator(
                operator,
                self.waffle_func.arg_pool.from_iter(args.into_iter()),
                self.waffle_func.type_pool.from_iter(ret_tys.into_iter()),
            );

            self.waffle_func.append_to_block(self.block_map[&block], res_vals[0]);
        } else {
            let res_def = waffle::ValueDef::Operator(
                operator,
                self.waffle_func.arg_pool.from_iter(args.into_iter()),
                self.waffle_func.type_pool.from_iter(ret_tys.iter().cloned()),
            );
            let res = self.waffle_func.add_value(res_def);
            self.waffle_func.append_to_block(self.block_map[&block], res);

            for (i, (res_val, ret_ty)) in res_vals.into_iter().zip(ret_tys).enumerate() {
                self.waffle_func.values[res_val] =
                    waffle::ValueDef::PickOutput(res, i as u32, ret_ty);
                self.waffle_func.append_to_block(self.block_map[&block], res_val);
            }
        }
    }
}
