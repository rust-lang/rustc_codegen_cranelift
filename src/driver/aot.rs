//! The AOT driver uses [`cranelift_object`] to write object files suitable for linking into a
//! standalone executable.

use std::convert::Infallible;
use std::fs::File;
use std::io::BufWriter;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;

use cranelift_codegen::incremental_cache::CacheKvStore;
use cranelift_object::{ObjectBuilder, ObjectModule};
use rustc_ast::expand::allocator::AllocatorMethod;
use rustc_codegen_ssa::back::lto::ThinModule;
use rustc_codegen_ssa::back::write::{
    CodegenContext, FatLtoInput, ModuleConfig, SharedEmitter, TargetMachineFactoryFn, ThinLtoInput,
};
use rustc_codegen_ssa::traits::{ExtraBackendMethods, WriteBackendMethods};
use rustc_codegen_ssa::{CompiledModule, ModuleCodegen, ModuleKind};
use rustc_data_structures::fingerprint::Fingerprint;
use rustc_data_structures::profiling::SelfProfilerRef;
use rustc_data_structures::stable_hash::{StableHash, StableHasher};
use rustc_errors::{DiagCtxt, DiagCtxtHandle};
use rustc_hir::attrs::Linkage as RLinkage;
use rustc_middle::dep_graph::WorkProduct;
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::mono::{MonoItem, MonoItemData, Visibility};
use rustc_session::Session;
use rustc_session::config::{OptLevel, OutputFilenames, OutputType};
use rustc_span::Symbol;

use crate::global_asm::{GlobalAsmConfig, GlobalAsmContext};
use crate::prelude::*;
use crate::serializable_module::SerializableModule;
use crate::unwind_module::{FileCache, UnwindModule};

pub(crate) struct AotModule {
    producer: String,
    global_asm_config: GlobalAsmConfig,
    module: UnwindModule<ObjectModule>,
    codegened_functions: Vec<(SerializableModule, Option<Fingerprint>)>,
    global_asm: String,
}

fn make_module(tcx: TyCtxt<'_>, cgu_name: &str) -> AotModule {
    let isa = crate::build_isa(tcx.sess, false);

    let mut builder = ObjectBuilder::new(
        isa,
        cgu_name.to_owned() + ".o",
        cranelift_module::default_libcall_names(),
    )
    .unwrap();

    // Disable function sections by default on MSVC as it causes significant slowdowns with link.exe.
    // Maybe link.exe has exponential behavior when there are many sections with the same name? Also
    // explicitly disable it on MinGW as rustc already disables it by default on MinGW and as such
    // isn't tested. If rustc enables it in the future on MinGW, we can re-enable it too once it has
    // been on MinGW.
    let default_function_sections =
        tcx.sess.target.function_sections && !tcx.sess.target.is_like_windows;
    builder.per_function_section(
        tcx.sess.opts.unstable_opts.function_sections.unwrap_or(default_function_sections),
    );

    let module = UnwindModule::new(ObjectModule::new(builder), cgu_name, true);

    let producer = crate::debuginfo::producer(tcx.sess);
    let global_asm_config = GlobalAsmConfig::new(tcx.sess);
    let codegened_functions = vec![];
    let global_asm = String::new();

    AotModule { producer, global_asm_config, module, codegened_functions, global_asm }
}

fn emit_module(
    output_filenames: &OutputFilenames,
    prof: &SelfProfilerRef,
    module: UnwindModule<ObjectModule>,
    kind: ModuleKind,
    name: String,
    global_asm_object: Option<PathBuf>,
    producer_str: &str,
) -> Result<CompiledModule, String> {
    let mut product = module.finish();

    if product.object.format() == cranelift_object::object::BinaryFormat::Elf {
        let comment_section = product.object.add_section(
            Vec::new(),
            b".comment".to_vec(),
            cranelift_object::object::SectionKind::OtherString,
        );
        let mut producer = vec![0];
        producer.extend(producer_str.as_bytes());
        producer.push(0);
        product.object.set_section_data(comment_section, producer, 1);
    }

    let tmp_file = output_filenames.temp_path_for_cgu(OutputType::Object, &name);
    let file = match File::create(&tmp_file) {
        Ok(file) => file,
        Err(err) => return Err(format!("error creating object file: {}", err)),
    };

    let mut file = BufWriter::new(file);
    if let Err(err) = product.object.write_stream(&mut file) {
        return Err(format!("error writing object file: {}", err));
    }
    let file = match file.into_inner() {
        Ok(file) => file,
        Err(err) => return Err(format!("error writing object file: {}", err)),
    };

    if prof.enabled() {
        prof.artifact_size(
            "object_file",
            tmp_file.file_name().unwrap().to_string_lossy(),
            file.metadata().unwrap().len(),
        );
    }

    Ok(CompiledModule {
        name,
        kind,
        object: Some(tmp_file),
        global_asm_object,
        dwarf_object: None,
        bytecode: None,
        assembly: None,
        llvm_ir: None,
        links_from_incr_cache: Vec::new(),
    })
}

fn codegen_cgu(tcx: TyCtxt<'_>, cgu_name: Symbol) -> AotModule {
    let _timer = tcx.prof.generic_activity_with_arg("codegen cgu", cgu_name.as_str());

    let cgu = tcx.codegen_unit(cgu_name);
    let mono_items = cgu.items_in_deterministic_order(tcx);

    let mut module = make_module(tcx, cgu_name.as_str());
    super::predefine_mono_items(tcx, &mut module.module, &mono_items);
    let isa = crate::build_isa(tcx.sess, false);
    for (mono_item, item_data) in mono_items {
        match mono_item {
            MonoItem::Fn(instance) => {
                let flags = tcx.codegen_instance_attrs(instance.def).flags;
                if flags.contains(CodegenFnAttrFlags::NAKED) {
                    rustc_codegen_ssa::mir::naked_asm::codegen_naked_asm(
                        &mut GlobalAsmContext { tcx, global_asm: &mut module.global_asm },
                        instance,
                        MonoItemData {
                            linkage: RLinkage::External,
                            visibility: if item_data.linkage == RLinkage::Internal {
                                Visibility::Hidden
                            } else {
                                item_data.visibility
                            },
                            ..item_data
                        },
                    );
                    continue;
                }

                let dep_node = mono_item.codegen_dep_node(tcx);
                let mut hasher = StableHasher::new();
                tcx.with_stable_hashing_context(|mut hcx| {
                    // Different crates may use different symbol name mangling for the same private function
                    tcx.stable_crate_id(LOCAL_CRATE).stable_hash(&mut hcx, &mut hasher);
                    instance.stable_hash(&mut hcx, &mut hasher);
                });
                let cache_key: Fingerprint = hasher.finish();

                // FIXME don't call try_mark_green if already compiled in another cgu
                if tcx.dep_graph.is_fully_enabled()
                    && tcx.dep_graph.try_mark_green(tcx, &dep_node).is_some()
                {
                    let data = FileCache.get(&cache_key.to_le_bytes()).unwrap();
                    module
                        .codegened_functions
                        .push((SerializableModule::deserialize(&data, isa.clone()), None));
                    continue;
                };

                let (ser_module, _) = tcx.dep_graph.with_task(
                    dep_node,
                    tcx,
                    || {
                        let mut ser_module =
                            SerializableModule::new(crate::build_isa(tcx.sess, false));
                        let codegened_function = crate::base::codegen_fn(
                            tcx,
                            cgu.name(),
                            Function::new(),
                            &mut ser_module,
                            instance,
                        );
                        let mut cached_context = Context::new();
                        let mut global_asm = String::new();
                        crate::base::compile_fn(
                            &tcx.prof,
                            tcx.dcx(),
                            &tcx.output_filenames(()),
                            crate::pretty_clif::should_write_ir(tcx.sess),
                            &mut cached_context,
                            &mut ser_module,
                            &mut global_asm,
                            codegened_function,
                        );

                        ser_module.add_global_asm(&global_asm);

                        ser_module
                    },
                    Some(rustc_middle::dep_graph::hash_result),
                );

                module.codegened_functions.push((ser_module, Some(cache_key)));
            }
            MonoItem::Static(def_id) => {
                crate::constant::codegen_static(tcx, &mut module.module, def_id);
            }
            MonoItem::GlobalAsm(item_id) => {
                rustc_codegen_ssa::base::codegen_global_asm(
                    &mut GlobalAsmContext { tcx, global_asm: &mut module.global_asm },
                    item_id,
                );
            }
        }
    }
    crate::main_shim::maybe_create_entry_wrapper(tcx, &mut module.module, false, cgu.is_primary());

    module
}

fn compile_cgu(
    prof: &SelfProfilerRef,
    _dcx: DiagCtxtHandle<'_>,
    output_filenames: &OutputFilenames,
    _should_write_ir: bool,
    mut aot_module: AotModule,
    cgu_name: String,
    kind: ModuleKind,
) -> Result<CompiledModule, String> {
    prof.generic_activity_with_arg("compile functions", &*cgu_name).run(|| {
        cranelift_codegen::timing::set_thread_profiler(Box::new(super::MeasuremeProfiler(
            prof.clone(),
        )));

        for (mut codegened_func, cache_key) in aot_module.codegened_functions {
            if let Some(cache_key) = cache_key {
                let data = codegened_func.serialize();
                FileCache.insert(&cache_key.to_le_bytes(), data.to_vec());
            }

            let asm = codegened_func.apply_to(&mut aot_module.module);
            aot_module.global_asm.push_str(&asm);
        }
    });

    let global_asm_object_file =
        prof.generic_activity_with_arg("compile assembly", &*cgu_name).run(|| {
            if aot_module.global_asm.is_empty() {
                return Ok::<_, String>(None);
            }

            let global_asm_object_file = output_filenames.temp_path_ext_for_cgu("asm.o", &cgu_name);
            crate::global_asm::compile_global_asm(
                &aot_module.global_asm_config,
                aot_module.global_asm,
                &global_asm_object_file,
            )?;

            Ok(Some(global_asm_object_file))
        })?;

    prof.generic_activity_with_arg("write object file", &*cgu_name).run(|| {
        emit_module(
            output_filenames,
            prof,
            aot_module.module,
            kind,
            cgu_name.clone(),
            global_asm_object_file,
            &aot_module.producer,
        )
    })
}

#[derive(Copy, Clone)]
pub(crate) struct AotDriver;

impl ExtraBackendMethods for AotDriver {
    type Module = AotModule;

    fn codegen_allocator<'tcx>(
        &self,
        tcx: TyCtxt<'tcx>,
        module_name: &str,
        methods: &[AllocatorMethod],
    ) -> Self::Module {
        let mut allocator_module = make_module(tcx, module_name);
        crate::allocator::codegen(tcx, &mut allocator_module.module, methods);
        allocator_module
    }

    fn compile_codegen_unit(
        &self,
        tcx: TyCtxt<'_>,
        cgu_name: Symbol,
    ) -> (ModuleCodegen<Self::Module>, u64) {
        let start_time = Instant::now();

        let dep_node = tcx.codegen_unit(cgu_name).codegen_dep_node(tcx);
        let (module, _) = tcx.dep_graph.with_task(
            dep_node,
            tcx,
            || {
                let aot_module = codegen_cgu(tcx, cgu_name);
                ModuleCodegen::new_regular(cgu_name.as_str().to_owned(), aot_module)
            },
            Some(rustc_middle::dep_graph::hash_result),
        );

        let time_to_codegen = start_time.elapsed();

        // We assume that the cost to run LLVM on a CGU is proportional to
        // the time we needed for codegenning it.
        let cost = time_to_codegen.as_nanos() as u64;

        (module, cost)
    }
}

impl WriteBackendMethods for AotDriver {
    type Module = AotModule;
    type ModuleBuffer = Infallible;
    type TargetMachine = ();
    type ThinData = Infallible;

    fn target_machine_factory(
        &self,
        _sess: &Session,
        _opt_level: OptLevel,
        _target_features: &[String],
    ) -> TargetMachineFactoryFn<Self> {
        Arc::new(|_, _| ())
    }

    fn optimize_and_codegen_fat_lto(
        _sess: &Session,
        _cgcx: &CodegenContext,
        _shared_emitter: &SharedEmitter,
        _tm_factory: TargetMachineFactoryFn<Self>,
        _exported_symbols_for_lto: &[String],
        _each_linked_rlib_for_lto: &[PathBuf],
        _modules: Vec<FatLtoInput<Self>>,
    ) -> CompiledModule {
        unreachable!()
    }

    fn run_thin_lto(
        _cgcx: &CodegenContext,
        _prof: &SelfProfilerRef,
        _dcx: rustc_errors::DiagCtxtHandle<'_>,
        _exported_symbols_for_lto: &[String],
        _each_linked_rlib_for_lto: &[PathBuf],
        _modules: Vec<ThinLtoInput<Self>>,
    ) -> (Vec<ThinModule<Self>>, Vec<WorkProduct>) {
        unreachable!()
    }

    fn optimize(
        _cgcx: &CodegenContext,
        _prof: &SelfProfilerRef,
        _shared_emitter: &SharedEmitter,
        _module: &mut ModuleCodegen<Self::Module>,
        _config: &ModuleConfig,
    ) {
    }

    fn optimize_and_codegen_thin(
        _cgcx: &CodegenContext,
        _prof: &SelfProfilerRef,
        _shared_emitter: &SharedEmitter,
        _tm_factory: TargetMachineFactoryFn<Self>,
        _thin: ThinModule<Self>,
    ) -> CompiledModule {
        unreachable!()
    }

    fn codegen(
        cgcx: &CodegenContext,
        prof: &SelfProfilerRef,
        shared_emitter: &SharedEmitter,
        module: ModuleCodegen<Self::Module>,
        config: &ModuleConfig,
    ) -> CompiledModule {
        let dcx = DiagCtxt::new(Box::new(shared_emitter.clone()));
        compile_cgu(
            prof,
            dcx.handle(),
            &cgcx.output_filenames,
            config.emit_ir,
            module.module_llvm,
            module.name,
            module.kind,
        )
        .unwrap_or_else(|err| dcx.handle().fatal(err))
    }

    fn serialize_module(_module: Self::Module, _is_thin: bool) -> Self::ModuleBuffer {
        unreachable!()
    }
}
