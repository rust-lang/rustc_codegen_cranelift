//! FIXME

// FIXME handle debuginfo

// FIXME dedup as much with aot.rs as possible

use object::{Object, ObjectSection};
use rustc_codegen_ssa::{CodegenResults, CompiledModule, CrateInfo, ModuleKind};
use rustc_middle::dep_graph::{WorkProduct, WorkProductId};
use rustc_session::Session;
use rustc_session::config::OutputFilenames;

use crate::driver::aot;
use crate::prelude::*;
use crate::serializable_module::SerializableModule;

pub(crate) struct OngoingCodegen {
    modules: Vec<CompiledModule>,
    allocator_module: Option<CompiledModule>,
    crate_info: CrateInfo,
}

impl OngoingCodegen {
    pub(crate) fn join(
        self,
        sess: &Session,
        outputs: &OutputFilenames,
    ) -> (CodegenResults, FxIndexMap<WorkProductId, WorkProduct>) {
        let mut modules = vec![];

        for module in self.modules {
            modules.push(module);
        }

        sess.dcx().abort_if_errors();

        let codegen_results = CodegenResults {
            modules,
            allocator_module: self.allocator_module,
            crate_info: self.crate_info,
        };

        aot::produce_final_output_artifacts(sess, &codegen_results, outputs);

        (codegen_results, FxIndexMap::default())
    }
}

fn make_module(sess: &Session) -> SerializableModule {
    let isa = crate::build_isa(sess, false);

    SerializableModule::new(isa)
}

fn compile_module_to_object(
    tcx: TyCtxt<'_>,
    serialize_module: SerializableModule,
    name: String,
) -> Result<CompiledModule, String> {
    let mut module = aot::make_module(tcx.sess, name.clone());
    serialize_module.apply_to(&mut module);
    let product = module.finish();

    aot::emit_module(
        tcx.output_filenames(()),
        tcx.sess.invocation_temp.as_deref(),
        &tcx.sess.prof,
        product.object,
        ModuleKind::Regular,
        name,
        &crate::debuginfo::producer(tcx.sess),
    )
}

fn serialize_module_as_bitcode(
    tcx: TyCtxt<'_>,
    serialize_module: SerializableModule,
    name: String,
) -> Result<CompiledModule, String> {
    let mut object = object::write::Object::new(
        object::BinaryFormat::Elf,
        object::Architecture::Aarch64,
        object::Endianness::Little,
    );
    object.set_subsections_via_symbols();
    let symbol = object.add_symbol(object::write::Symbol {
        name: b"cgclif_lto".to_vec(),
        value: 0,
        size: 0,
        kind: object::write::SymbolKind::Data,
        scope: object::write::SymbolScope::Compilation,
        weak: false,
        section: object::write::SymbolSection::Undefined,
        flags: object::write::SymbolFlags::None,
    });
    let section =
        object.add_subsection(object::write::StandardSection::ReadOnlyData, b"cgclif_lto");
    object.add_symbol_data(symbol, section, &serialize_module.serialize(), 1);

    aot::emit_module(
        tcx.output_filenames(()),
        tcx.sess.invocation_temp.as_deref(),
        &tcx.sess.prof,
        object,
        ModuleKind::Regular,
        name,
        &crate::debuginfo::producer(tcx.sess),
    )
}

fn module_codegen(tcx: TyCtxt<'_>, cgu_name: rustc_span::Symbol) -> CompiledModule {
    let mut module = make_module(tcx.sess);

    let (mut debug_context, codegened_functions, mut global_asm) =
        aot::codegen_cgu_content(tcx, &mut module, cgu_name);

    let cgu_name = cgu_name.as_str().to_owned();

    let profiler = tcx.prof.clone();
    let output_filenames = tcx.output_filenames(()).clone();
    let should_write_ir = crate::pretty_clif::should_write_ir(tcx.sess);

    match (move || {
        tcx.prof.clone().verbose_generic_activity_with_arg("compile functions", &*cgu_name).run(
            || {
                let mut cached_context = Context::new();
                for codegened_func in codegened_functions {
                    crate::base::compile_fn(
                        &profiler,
                        &output_filenames,
                        should_write_ir,
                        &mut cached_context,
                        &mut module,
                        debug_context.as_mut(),
                        &mut global_asm,
                        codegened_func,
                    );
                }
            },
        );

        // FIXME handle inline asm
        if !global_asm.is_empty() {
            tcx.sess.dcx().fatal("Inline asm is not yet supported in LTO mode");
        }

        let codegen_result =
            tcx.prof.verbose_generic_activity_with_arg("write object file", &*cgu_name).run(|| {
                if tcx.crate_types() != [rustc_session::config::CrateType::Rlib] {
                    compile_module_to_object(tcx, module, cgu_name.clone())
                } else {
                    serialize_module_as_bitcode(tcx, module, cgu_name.clone())
                }
            });
        codegen_result
    })() {
        Ok(res) => res,
        Err(err) => tcx.sess.dcx().fatal(err),
    }
}

pub(super) fn load_lto_modules(
    tcx: TyCtxt<'_>,
    crate_info: &CrateInfo,
) -> Vec<(String, SerializableModule)> {
    if tcx.crate_types() == [rustc_session::config::CrateType::Rlib] {
        return vec![];
    }

    let mut each_linked_rlib_for_lto = Vec::new();
    drop(rustc_codegen_ssa::back::link::each_linked_rlib(&crate_info, None, &mut |cnum, path| {
        if rustc_codegen_ssa::back::link::ignored_for_lto(tcx.sess, &crate_info, cnum) {
            return;
        }
        each_linked_rlib_for_lto.push((cnum, path.to_path_buf()));
    }));

    let mut modules = vec![];

    for (_cnum, path) in each_linked_rlib_for_lto {
        let archive_data = unsafe {
            rustc_data_structures::memmap::Mmap::map(
                std::fs::File::open(&path).expect("couldn't open rlib"),
            )
            .expect("couldn't map rlib")
        };
        let archive =
            object::read::archive::ArchiveFile::parse(&*archive_data).expect("wanted an rlib");
        let obj_files = archive
            .members()
            .filter_map(|child| {
                child
                    .ok()
                    .and_then(|c| std::str::from_utf8(c.name()).ok().map(|name| (name.trim(), c)))
            })
            .filter(|&(name, _)| rustc_codegen_ssa::looks_like_rust_object_file(name));
        for (name, child) in obj_files {
            let lto_object =
                object::read::File::parse(child.data(&*archive_data).expect("corrupt rlib"))
                    .unwrap();
            let module = SerializableModule::deserialize(
                lto_object
                    .section_by_name(".rodata.cgclif_lto")
                    .unwrap_or_else(|| {
                        tcx.sess
                            .dcx()
                            .fatal(format!("no LTO data found for {}({name})", path.display()));
                    })
                    .data()
                    .unwrap(),
                crate::build_isa(tcx.sess, false),
            );
            modules.push((name.to_owned(), module));
        }
    }

    modules
}

pub(crate) fn run_lto(tcx: TyCtxt<'_>) -> Box<OngoingCodegen> {
    tcx.dcx().note(format!("Using LTO for {}", tcx.crate_name(LOCAL_CRATE)));

    // FIXME handle `-Ctarget-cpu=native`
    let target_cpu = match tcx.sess.opts.cg.target_cpu {
        Some(ref name) => name,
        None => tcx.sess.target.cpu.as_ref(),
    }
    .to_owned();

    let crate_info = CrateInfo::new(tcx, target_cpu);

    let cgus = tcx.collect_and_partition_mono_items(()).codegen_units;

    let mut modules = tcx.sess.time("codegen mono items", || {
        cgus.iter().map(|cgu| module_codegen(tcx, cgu.name())).collect::<Vec<_>>()
    });

    for (name, module) in load_lto_modules(tcx, &crate_info) {
        modules.push(compile_module_to_object(tcx, module, name).unwrap());
    }

    let allocator_module = aot::emit_allocator_module(tcx);

    Box::new(OngoingCodegen { modules, allocator_module, crate_info })
}
