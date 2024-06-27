//! FIXME

// FIXME handle debuginfo

// FIXME dedup as much with aot.rs as possible

use std::fs::File;
use std::io::BufWriter;

use object::{Object, ObjectSection};
use rustc_codegen_ssa::back::rmeta_link;
use rustc_codegen_ssa::base::{allocator_kind_for_codegen, allocator_shim_contents};
use rustc_codegen_ssa::traits::ExtraBackendMethods;
use rustc_codegen_ssa::{CompiledModule, CompiledModules, CrateInfo, ModuleKind};
use rustc_middle::dep_graph::WorkProductMap;
use rustc_session::Session;
use rustc_session::config::{OutputFilenames, OutputType};

use crate::driver::aot::{self, AotModule};
use crate::global_asm::GlobalAsmConfig;
use crate::prelude::*;
use crate::serializable_module::SerializableModule;

pub(crate) struct OngoingCodegen {
    modules: Vec<CompiledModule>,
    allocator_module: Option<CompiledModule>,
    only_rlib: bool,
}

impl OngoingCodegen {
    pub(crate) fn join(
        self,
        sess: &Session,
        outputs: &OutputFilenames,
        crate_info: &CrateInfo,
    ) -> (CompiledModules, WorkProductMap) {
        let mut modules = self.modules;

        if !self.only_rlib {
            for (name, module, global_asm) in load_lto_modules(sess, crate_info) {
                modules.push(
                    compile_module_to_object(sess, outputs, module, global_asm, name).unwrap(),
                );
            }
        }

        sess.dcx().abort_if_errors();

        let compiled_modules = CompiledModules { modules, allocator_module: self.allocator_module };

        rustc_codegen_ssa::back::write::produce_final_output_artifacts(
            sess,
            &compiled_modules,
            outputs,
        );

        (compiled_modules, WorkProductMap::default())
    }
}

fn make_module(sess: &Session) -> AotModule<SerializableModule> {
    let isa = crate::build_isa(sess, false);

    let module = SerializableModule::new(isa);

    let producer = crate::debuginfo::producer(sess);
    let global_asm_config = GlobalAsmConfig::new(sess);
    let codegened_functions = vec![];
    let global_asm = String::new();

    AotModule {
        producer,
        global_asm_config,
        module,
        debug_context: None,
        codegened_functions,
        global_asm,
    }
}

fn compile_module_to_object(
    sess: &Session,
    output_filenames: &OutputFilenames,
    serialize_module: SerializableModule,
    global_asm: String,
    name: String,
) -> Result<CompiledModule, String> {
    let mut aot_module = aot::make_module(sess, &name);
    aot_module.global_asm = global_asm;
    serialize_module.apply_to(&mut aot_module.module);

    let global_asm_object_file =
        sess.prof.generic_activity_with_arg("compile assembly", &*name).run(|| {
            if aot_module.global_asm.is_empty() {
                return Ok::<_, String>(None);
            }

            let global_asm_object_file = output_filenames.temp_path_ext_for_cgu("asm.o", &name);
            crate::global_asm::compile_global_asm(
                &aot_module.global_asm_config,
                aot_module.global_asm,
                &global_asm_object_file,
            )?;

            Ok(Some(global_asm_object_file))
        })?;

    aot::emit_module(
        output_filenames,
        &sess.prof,
        aot_module.module,
        None,
        ModuleKind::Regular,
        name,
        global_asm_object_file,
        &aot_module.producer,
    )
}

fn serialize_module_as_bitcode(
    tcx: TyCtxt<'_>,
    serialize_module: SerializableModule,
    global_asm: String,
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
    object.add_symbol_data(symbol, section, &serialize_module.serialize(global_asm), 1);

    let tmp_file = tcx.output_filenames(()).temp_path_for_cgu(OutputType::Object, &name);
    let file = match File::create(&tmp_file) {
        Ok(file) => file,
        Err(err) => return Err(format!("error creating object file: {}", err)),
    };

    let mut file = BufWriter::new(file);
    if let Err(err) = object.write_stream(&mut file) {
        return Err(format!("error writing object file: {}", err));
    }
    let file = match file.into_inner() {
        Ok(file) => file,
        Err(err) => return Err(format!("error writing object file: {}", err)),
    };

    if tcx.sess.prof.enabled() {
        tcx.sess.prof.artifact_size(
            "object_file",
            tmp_file.file_name().unwrap().to_string_lossy(),
            file.metadata().unwrap().len(),
        );
    }

    Ok(CompiledModule {
        name,
        kind: ModuleKind::Regular,
        object: Some(tmp_file),
        global_asm_object: None, // FIXME
        dwarf_object: None,
        bytecode: None,
        assembly: None,
        llvm_ir: None,
        links_from_incr_cache: Vec::new(),
    })
}

fn module_codegen(tcx: TyCtxt<'_>, cgu_name: rustc_span::Symbol) -> CompiledModule {
    let mut aot_module = make_module(tcx.sess);
    aot::codegen_cgu(tcx, &mut aot_module, cgu_name);

    let cgu_name = cgu_name.as_str().to_owned();

    let prof = tcx.prof.clone();
    let output_filenames = tcx.output_filenames(()).clone();
    let should_write_ir = crate::pretty_clif::should_write_ir(tcx.sess);

    match (move || {
        tcx.prof.clone().verbose_generic_activity_with_arg("compile functions", &*cgu_name).run(
            || {
                let mut cached_context = Context::new();
                for codegened_func in aot_module.codegened_functions {
                    crate::base::compile_fn(
                        &prof,
                        &output_filenames,
                        should_write_ir,
                        &mut cached_context,
                        &mut aot_module.module,
                        aot_module.debug_context.as_mut(),
                        &mut aot_module.global_asm,
                        codegened_func,
                    );
                }
            },
        );

        let codegen_result =
            tcx.prof.verbose_generic_activity_with_arg("write object file", &*cgu_name).run(|| {
                if tcx.crate_types() != [rustc_session::config::CrateType::Rlib] {
                    compile_module_to_object(
                        tcx.sess,
                        tcx.output_filenames(()),
                        aot_module.module,
                        aot_module.global_asm,
                        cgu_name.clone(),
                    )
                } else {
                    serialize_module_as_bitcode(
                        tcx,
                        aot_module.module,
                        aot_module.global_asm,
                        cgu_name.clone(),
                    )
                }
            });
        codegen_result
    })() {
        Ok(res) => res,
        Err(err) => tcx.sess.dcx().fatal(err),
    }
}

pub(super) fn load_lto_modules(
    sess: &Session,
    crate_info: &CrateInfo,
) -> Vec<(String, SerializableModule, String)> {
    let mut each_linked_rlib_for_lto = Vec::new();
    drop(rustc_codegen_ssa::back::link::each_linked_rlib(&crate_info, None, &mut |cnum, path| {
        if rustc_codegen_ssa::back::link::ignored_for_lto(sess, &crate_info, cnum) {
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
        let metadata_link = rmeta_link::read_from_data(&archive_data, &path).unwrap();
        let archive =
            object::read::archive::ArchiveFile::parse(&*archive_data).expect("wanted an rlib");
        let obj_files = archive
            .members()
            .filter_map(|child| {
                child
                    .ok()
                    .and_then(|c| std::str::from_utf8(c.name()).ok().map(|name| (name.trim(), c)))
            })
            .filter(|&(name, _)| metadata_link.rust_object_files.iter().any(|f| f == name));
        for (name, child) in obj_files {
            let lto_object =
                object::read::File::parse(child.data(&*archive_data).expect("corrupt rlib"))
                    .unwrap();
            let (module, global_asm) = SerializableModule::deserialize(
                lto_object
                    .section_by_name(".rodata.cgclif_lto")
                    .unwrap_or_else(|| {
                        sess.dcx()
                            .fatal(format!("no LTO data found for {}({name})", path.display()));
                    })
                    .data()
                    .unwrap(),
                crate::build_isa(sess, false),
            );
            modules.push((name.to_owned(), module, global_asm));
        }
    }

    modules
}

pub(crate) fn run_lto(tcx: TyCtxt<'_>) -> Box<OngoingCodegen> {
    tcx.dcx().note(format!("Using LTO for {}", tcx.crate_name(LOCAL_CRATE)));

    let cgus = tcx.collect_and_partition_mono_items(()).codegen_units;

    let modules = tcx.sess.time("codegen mono items", || {
        cgus.iter().map(|cgu| module_codegen(tcx, cgu.name())).collect::<Vec<_>>()
    });

    let allocator_module = if let Some(kind) = allocator_kind_for_codegen(tcx) {
        let aot_module = aot::AotDriver.codegen_allocator(
            tcx,
            "allocator_shim",
            &allocator_shim_contents(tcx, kind),
        );
        Some(
            aot::compile_cgu(
                &tcx.sess.prof,
                tcx.output_filenames(()),
                false,
                aot_module,
                "allocator_shim".to_owned(),
                ModuleKind::Allocator,
            )
            .unwrap(),
        )
    } else {
        None
    };

    Box::new(OngoingCodegen {
        modules,
        allocator_module,
        only_rlib: tcx.crate_types() == [rustc_session::config::CrateType::Rlib],
    })
}
