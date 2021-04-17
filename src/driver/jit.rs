//! The JIT driver uses [`cranelift_jit`] to JIT execute programs without writing any object
//! files.

use std::ffi::CString;
use std::sync::Mutex;

use cranelift_codegen::binemit::{NullStackMapSink, NullTrapSink};
use lazy_static::lazy_static;
use rustc_codegen_ssa::CrateInfo;
use rustc_data_structures::fx::FxHashSet;
use rustc_middle::mir::mono::MonoItem;

use cranelift_jit::{JITBuilder, JITModule};

use crate::{prelude::*, BackendConfig};
use crate::{CodegenCx, CodegenMode};

struct JitState {
    backend_config: BackendConfig,
    jit_module: JITModule,
    defined_functions: FxHashSet<String>,
}

// FIXME make `JITModule` implement `Send`
unsafe impl Send for JitState {}

lazy_static! {
    static ref JIT_STATE: Mutex<Option<JitState>> = Mutex::new(None);
}

pub enum JitCompilationResult {
    Launch(extern "C" fn(usize, *const *const u8) -> isize, usize, *const *const u8),
    Swapped,
}

unsafe impl Send for JitCompilationResult {}

fn create_jit_module<'tcx>(
    tcx: TyCtxt<'tcx>,
    backend_config: &BackendConfig,
    hotswap: bool,
    extra_symbols: Vec<(String, *const u8)>,
) -> JITModule {
    let imported_symbols = load_imported_symbols_for_jit(tcx);

    let isa = crate::build_isa(tcx.sess, backend_config);
    let mut jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    jit_builder.hotswap(hotswap);
    crate::compiler_builtins::register_functions_for_jit(&mut jit_builder);
    jit_builder.symbols(imported_symbols);
    jit_builder.symbols(extra_symbols);
    let mut jit_module = JITModule::new(jit_builder);

    let mut cx = crate::CodegenCx::new(tcx, backend_config, jit_module.isa(), false);

    crate::allocator::codegen(tcx, &mut jit_module, &mut cx.unwind_context);
    crate::main_shim::maybe_create_entry_wrapper(
        tcx,
        &mut jit_module,
        &mut cx.unwind_context,
        true,
    );

    jit_module
}

fn make_args(crate_name: &str, args: &[String]) -> (usize, *const *const u8) {
    let mut argv = std::iter::once(crate_name)
        .chain(args.iter().map(|arg| &**arg))
        .map(|arg| CString::new(arg).unwrap().into_raw() as *const u8)
        .collect::<Vec<_>>();

    // Push a null pointer as a terminating argument. This is required by POSIX and
    // useful as some dynamic linkers use it as a marker to jump over.
    argv.push(std::ptr::null());

    let (ptr, size, _cap) = argv.into_raw_parts();
    (size, ptr)
}

fn get_start_fn(jit_module: &mut JITModule) -> extern "C" fn(usize, *const *const u8) -> isize {
    let start_sig = Signature {
        params: vec![
            AbiParam::new(jit_module.target_config().pointer_type()),
            AbiParam::new(jit_module.target_config().pointer_type()),
        ],
        returns: vec![AbiParam::new(jit_module.target_config().pointer_type() /*isize*/)],
        call_conv: jit_module.target_config().default_call_conv,
    };
    let start_func_id = jit_module.declare_function("main", Linkage::Import, &start_sig).unwrap();
    let finalized_start: *const u8 = jit_module.get_finalized_function(start_func_id);
    unsafe { ::std::mem::transmute(finalized_start) }
}

pub fn run_jit(
    tcx: TyCtxt<'_>,
    backend_config: BackendConfig,
    extra_symbols: Vec<(String, *const u8)>,
) -> JitCompilationResult {
    if !tcx.sess.opts.output_types.should_codegen() {
        tcx.sess.fatal("JIT mode doesn't work with `cargo check`");
    }

    if !tcx.sess.crate_types().contains(&rustc_session::config::CrateType::Executable) {
        tcx.sess.fatal("can't jit non-executable crate");
    }

    let mut jit_state = JIT_STATE.lock().unwrap_or_else(|_| std::process::abort());

    let is_fresh = jit_state.is_none();
    if is_fresh {
        let jit_module = create_jit_module(tcx, &backend_config, true, extra_symbols);

        *jit_state =
            Some(JitState { backend_config, jit_module, defined_functions: FxHashSet::default() });
    }

    let jit_state = jit_state.as_mut().unwrap();

    let backend_config = &jit_state.backend_config;
    let jit_module = &mut jit_state.jit_module;
    let defined_functions = &mut jit_state.defined_functions;
    let mut cx = CodegenCx::new(tcx, &backend_config, jit_module.isa(), false);

    let (_, cgus) = tcx.collect_and_partition_mono_items(LOCAL_CRATE);
    let mono_items = cgus
        .iter()
        .map(|cgu| cgu.items_in_deterministic_order(tcx).into_iter())
        .flatten()
        .collect::<FxHashMap<_, (_, _)>>()
        .into_iter()
        .collect::<Vec<(_, (_, _))>>();

    super::time(tcx, backend_config.display_cg_time, "codegen mono items", || {
        super::predefine_mono_items(tcx, jit_module, &mono_items);
        for (mono_item, _) in mono_items {
            match mono_item {
                MonoItem::Fn(inst) => match backend_config.codegen_mode {
                    CodegenMode::Aot => unreachable!(),
                    CodegenMode::Jit => {
                        cx.tcx.sess.time("codegen fn", || {
                            crate::base::codegen_fn(&mut cx, jit_module, inst)
                        });
                    }
                    CodegenMode::JitLazy => codegen_shim(&mut cx, jit_module, inst),
                    CodegenMode::JitHotSwap => {
                        let symbol_name = tcx.symbol_name(inst).name;
                        if !defined_functions.insert(symbol_name.to_owned()) {
                            // Already defined in the past. Ensure that we can re-define it.
                            match jit_module.get_name(symbol_name).unwrap() {
                                cranelift_module::FuncOrDataId::Func(func_id) => {
                                    jit_module.prepare_for_function_redefine(func_id).unwrap();
                                }
                                cranelift_module::FuncOrDataId::Data(_) => unreachable!(),
                            }
                        }
                        tcx.sess.time("codegen fn", || {
                            crate::base::codegen_fn(&mut cx, jit_module, inst)
                        });
                    }
                },
                MonoItem::Static(def_id) => match backend_config.codegen_mode {
                    CodegenMode::Aot => unreachable!(),
                    CodegenMode::Jit | CodegenMode::JitLazy => {
                        crate::constant::codegen_static(tcx, jit_module, def_id);
                    }
                    CodegenMode::JitHotSwap => {
                        tcx.sess.span_warn(
                            tcx.def_span(def_id),
                            "hot swapping is not supported for statics",
                        );
                        if is_fresh {
                            crate::constant::codegen_static(tcx, jit_module, def_id);
                        }
                    }
                },
                MonoItem::GlobalAsm(item_id) => {
                    let item = tcx.hir().item(item_id);
                    tcx.sess.span_fatal(item.span, "Global asm is not supported in JIT mode");
                }
            }
        }
    });

    if !cx.global_asm.is_empty() {
        tcx.sess.fatal("Inline asm is not supported in JIT mode");
    }

    tcx.sess.abort_if_errors();

    jit_module.finalize_definitions();
    unsafe { cx.unwind_context.register_jit(jit_module) };

    let (argc, argv) = make_args(&tcx.crate_name(LOCAL_CRATE).as_str(), &backend_config.jit_args);

    let start_fn = get_start_fn(jit_module);

    match backend_config.codegen_mode {
        CodegenMode::Aot => unreachable!(),
        CodegenMode::Jit | CodegenMode::JitLazy => {
            JitCompilationResult::Launch(start_fn, argc, argv)
        }
        CodegenMode::JitHotSwap => {
            if is_fresh {
                JitCompilationResult::Launch(start_fn, argc, argv)
            } else {
                JitCompilationResult::Swapped
            }
        }
    }
}

#[no_mangle]
extern "C" fn __clif_jit_fn(instance_ptr: *const Instance<'static>) -> *const u8 {
    rustc_middle::ty::tls::with(|tcx| {
        // lift is used to ensure the correct lifetime for instance.
        let instance = tcx.lift(unsafe { *instance_ptr }).unwrap();

        let mut jit_state = JIT_STATE.lock().unwrap();
        let jit_state = jit_state.as_mut().unwrap();
        let jit_module = &mut jit_state.jit_module;

        let name = tcx.symbol_name(instance).name.to_string();
        let sig = crate::abi::get_function_sig(tcx, jit_module.isa().triple(), instance);
        let func_id = jit_module.declare_function(&name, Linkage::Export, &sig).unwrap();
        jit_module.prepare_for_function_redefine(func_id).unwrap();

        let mut cx = crate::CodegenCx::new(tcx, &jit_state.backend_config, jit_module.isa(), false);
        tcx.sess.time("codegen fn", || crate::base::codegen_fn(&mut cx, jit_module, instance));

        assert!(cx.global_asm.is_empty());
        jit_module.finalize_definitions();
        unsafe { cx.unwind_context.register_jit(&jit_module) };
        jit_module.get_finalized_function(func_id)
    })
}

fn load_imported_symbols_for_jit(tcx: TyCtxt<'_>) -> Vec<(String, *const u8)> {
    use rustc_middle::middle::dependency_format::Linkage;

    let mut dylib_paths = Vec::new();

    let crate_info = CrateInfo::new(tcx);
    let formats = tcx.dependency_formats(LOCAL_CRATE);
    let data = &formats
        .iter()
        .find(|(crate_type, _data)| *crate_type == rustc_session::config::CrateType::Executable)
        .unwrap()
        .1;
    for &(cnum, _) in &crate_info.used_crates_dynamic {
        let src = &crate_info.used_crate_source[&cnum];
        match data[cnum.as_usize() - 1] {
            Linkage::NotLinked | Linkage::IncludedFromDylib => {}
            Linkage::Static => {
                let name = tcx.crate_name(cnum);
                let mut err =
                    tcx.sess.struct_err(&format!("Can't load static lib {}", name.as_str()));
                err.note("rustc_codegen_cranelift can only load dylibs in JIT mode.");
                err.emit();
            }
            Linkage::Dynamic => {
                dylib_paths.push(src.dylib.as_ref().unwrap().0.clone());
            }
        }
    }

    let mut imported_symbols = Vec::new();
    for path in dylib_paths {
        use object::{Object, ObjectSymbol};
        let lib = libloading::Library::new(&path).unwrap();
        let obj = std::fs::read(path).unwrap();
        let obj = object::File::parse(&obj).unwrap();
        imported_symbols.extend(obj.dynamic_symbols().filter_map(|symbol| {
            let name = symbol.name().unwrap().to_string();
            if name.is_empty() || !symbol.is_global() || symbol.is_undefined() {
                return None;
            }
            if name.starts_with("rust_metadata_") {
                // The metadata is part of a section that is not loaded by the dynamic linker in
                // case of cg_llvm.
                return None;
            }
            let dlsym_name = if cfg!(target_os = "macos") {
                // On macOS `dlsym` expects the name without leading `_`.
                assert!(name.starts_with('_'), "{:?}", name);
                &name[1..]
            } else {
                &name
            };
            let symbol: libloading::Symbol<'_, *const u8> =
                unsafe { lib.get(dlsym_name.as_bytes()) }.unwrap();
            Some((name, *symbol))
        }));
        std::mem::forget(lib)
    }

    tcx.sess.abort_if_errors();

    imported_symbols
}

fn codegen_shim<'tcx>(cx: &mut CodegenCx<'tcx>, module: &mut JITModule, inst: Instance<'tcx>) {
    let pointer_type = module.target_config().pointer_type();

    let name = cx.tcx.symbol_name(inst).name.to_string();
    let sig = crate::abi::get_function_sig(cx.tcx, module.isa().triple(), inst);
    let func_id = module.declare_function(&name, Linkage::Export, &sig).unwrap();

    let instance_ptr = Box::into_raw(Box::new(inst));

    let jit_fn = module
        .declare_function(
            "__clif_jit_fn",
            Linkage::Import,
            &Signature {
                call_conv: module.target_config().default_call_conv,
                params: vec![AbiParam::new(pointer_type)],
                returns: vec![AbiParam::new(pointer_type)],
            },
        )
        .unwrap();

    cx.cached_context.clear();
    let trampoline = &mut cx.cached_context.func;
    trampoline.signature = sig.clone();

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut trampoline_builder = FunctionBuilder::new(trampoline, &mut builder_ctx);

    let jit_fn = module.declare_func_in_func(jit_fn, trampoline_builder.func);
    let sig_ref = trampoline_builder.func.import_signature(sig);

    let entry_block = trampoline_builder.create_block();
    trampoline_builder.append_block_params_for_function_params(entry_block);
    let fn_args = trampoline_builder.func.dfg.block_params(entry_block).to_vec();

    trampoline_builder.switch_to_block(entry_block);
    let instance_ptr = trampoline_builder.ins().iconst(pointer_type, instance_ptr as u64 as i64);
    let jitted_fn = trampoline_builder.ins().call(jit_fn, &[instance_ptr]);
    let jitted_fn = trampoline_builder.func.dfg.inst_results(jitted_fn)[0];
    let call_inst = trampoline_builder.ins().call_indirect(sig_ref, jitted_fn, &fn_args);
    let ret_vals = trampoline_builder.func.dfg.inst_results(call_inst).to_vec();
    trampoline_builder.ins().return_(&ret_vals);

    module
        .define_function(
            func_id,
            &mut cx.cached_context,
            &mut NullTrapSink {},
            &mut NullStackMapSink {},
        )
        .unwrap();
}
