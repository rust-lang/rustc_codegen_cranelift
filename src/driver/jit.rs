//! The JIT driver uses [`cranelift_jit`] to JIT execute programs without writing any object
//! files.

use std::ffi::CString;
use std::os::raw::{c_char, c_int};
use std::sync::Arc;

use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::FuncOrDataId;
use rustc_codegen_ssa::CrateInfo;
use rustc_codegen_ssa::base::{allocator_kind_for_codegen, allocator_shim_contents};
use rustc_data_structures::fx::{FxHashSet, FxIndexMap};
use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::mono::MonoItem;
use rustc_session::Session;
use rustc_session::config::{DebugInfo, OutputFilenames};
use rustc_span::sym;

use super::jit_tls;
use crate::constant::{THREAD_LOCAL_ACCESSOR, ThreadLocalKey};
use crate::debuginfo::TypeDebugContext;
use crate::prelude::*;
use crate::unwind_module::UnwindModule;

fn create_jit_module(
    tcx: TyCtxt<'_>,
    dylibs: &Arc<SymbolLookup>,
) -> (UnwindModule<JITModule>, Option<DebugContext>) {
    let isa = crate::build_isa(tcx.sess, true);
    let mut jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    crate::compiler_builtins::register_functions_for_jit(&mut jit_builder);
    jit_builder.symbol(THREAD_LOCAL_ACCESSOR, jit_tls::get as *const u8);
    let dylibs = dylibs.clone();
    jit_builder.symbol_lookup_fn(Box::new(move |name| (*dylibs)(name)));
    let mut jit_module = UnwindModule::new(
        JITModule::new(jit_builder),
        &rustc_symbol_mangling::eh_personality_symbol(tcx),
        false,
        tcx.sess.opts.debuginfo != DebugInfo::None,
    );

    let cx = DebugContext::new(tcx, jit_module.isa(), false, "dummy_cgu_name");

    if let Some(kind) = allocator_kind_for_codegen(tcx) {
        crate::allocator::codegen(tcx, &mut jit_module, &allocator_shim_contents(tcx, kind));
    }

    (jit_module, cx)
}

pub(crate) fn run_jit(tcx: TyCtxt<'_>, target_cpu: String, jit_args: Vec<String>) -> ! {
    if !tcx.crate_types().contains(&rustc_structures::CrateType::Executable) {
        tcx.dcx().fatal("can't jit non-executable crate");
    }

    let output_filenames = tcx.output_filenames(());
    let crate_info = CrateInfo::new(tcx, target_cpu);
    let should_write_ir = crate::pretty_clif::should_write_ir(tcx.sess);
    let dylibs = Arc::new(dep_symbol_lookup_fn(tcx.sess, crate_info));
    let (mut jit_module, mut debug_context) = create_jit_module(tcx, &dylibs);
    let mut cached_context = Context::new();
    // Key = index.
    let mut thread_local_keys = FxIndexMap::<String, DefId>::default();

    let cgus = tcx.collect_and_partition_mono_items(()).codegen_units;
    let mono_items = cgus
        .iter()
        .map(|cgu| cgu.items_in_deterministic_order(tcx).into_iter())
        .flatten()
        .collect::<FxHashMap<_, _>>()
        .into_iter()
        .collect::<Vec<(_, _)>>();
    let thread_local_images: FxHashSet<DefId> = mono_items
        .iter()
        .filter_map(|&(item, _)| match item {
            MonoItem::Static(def_id) if jit_tls::is_thread_local(tcx, def_id) => Some(def_id),
            _ => None,
        })
        .collect();

    tcx.sess.time("codegen mono items", || {
        super::predefine_mono_items(tcx, &mut jit_module, &mono_items);
        for (mono_item, _) in mono_items {
            match mono_item {
                MonoItem::Fn(inst) => {
                    codegen_and_compile_fn(
                        tcx,
                        &output_filenames,
                        should_write_ir,
                        debug_context.as_mut(),
                        &mut cached_context,
                        &mut jit_module,
                        Some(&mut |symbol, def_id| {
                            thread_local_keys.insert_full(symbol.to_owned(), def_id).0
                        }),
                        inst,
                    );
                }
                MonoItem::Static(def_id) if thread_local_images.contains(&def_id) => {
                    crate::constant::codegen_thread_local_image(tcx, &mut jit_module, def_id);
                }
                MonoItem::Static(def_id) => {
                    crate::constant::codegen_static(tcx, &mut jit_module, def_id);
                }
                MonoItem::GlobalAsm(item_id) => {
                    let item = tcx.hir_item(item_id);
                    tcx.dcx().span_fatal(item.span, "Global asm is not supported in JIT mode");
                }
            }
        }
    });

    crate::main_shim::maybe_create_entry_wrapper(tcx, &mut jit_module, true, true);

    let new_thread_locals = jit_tls::classify_new_thread_locals(
        tcx,
        thread_local_keys
            .iter()
            .enumerate()
            .map(|(key, (symbol, &def_id))| (key, &**symbol, def_id)),
        &thread_local_images,
        &dylibs,
    );

    tcx.dcx().abort_if_errors();

    let mut jit_module = jit_module.finalize_definitions();
    let image_address = |symbol: &str| match jit_module.declarations().get_name(symbol) {
        Some(FuncOrDataId::Data(id)) => jit_module.get_finalized_data(id).0.addr(),
        _ => unreachable!("thread-local image `{symbol}` was defined as data"),
    };
    for new in new_thread_locals {
        let (key, source) = jit_tls::source(new, image_address, &dylibs);
        jit_tls::define(key, source);
    }

    println!(
        "Rustc codegen cranelift will JIT run the executable, because -Cllvm-args=mode=jit was passed"
    );

    let args = std::iter::once(&*tcx.crate_name(LOCAL_CRATE).as_str().to_string())
        .chain(jit_args.iter().map(|arg| &**arg))
        .map(|arg| CString::new(arg).unwrap())
        .collect::<Vec<_>>();

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

    let f: extern "C" fn(c_int, *const *const c_char) -> c_int =
        unsafe { ::std::mem::transmute(finalized_start) };

    let mut argv = args.iter().map(|arg| arg.as_ptr()).collect::<Vec<_>>();

    // Push a null pointer as a terminating argument. This is required by POSIX and
    // useful as some dynamic linkers use it as a marker to jump over.
    argv.push(std::ptr::null());

    let ret = f(args.len() as c_int, argv.as_ptr());
    std::process::exit(ret);
}

fn codegen_and_compile_fn<'tcx>(
    tcx: TyCtxt<'tcx>,
    output_filenames: &OutputFilenames,
    should_write_ir: bool,
    mut debug_context: Option<&mut DebugContext>,
    cached_context: &mut Context,
    module: &mut dyn Module,
    thread_local_key: Option<ThreadLocalKey<'_>>,
    instance: Instance<'tcx>,
) {
    if tcx.codegen_instance_attrs(instance.def).flags.contains(CodegenFnAttrFlags::NAKED) {
        tcx.dcx()
            .span_fatal(tcx.def_span(instance.def_id()), "Naked asm is not supported in JIT mode");
    }

    cranelift_codegen::timing::set_thread_profiler(Box::new(super::MeasuremeProfiler(
        tcx.prof.clone(),
    )));

    tcx.prof.generic_activity("codegen and compile fn").run(|| {
        let _inst_guard =
            crate::PrintOnPanic(|| format!("{:?} {}", instance, tcx.symbol_name(instance).name));

        let cached_func = std::mem::replace(&mut cached_context.func, Function::new());
        let codegened_func = crate::base::codegen_fn(
            tcx,
            sym::dummy_cgu_name,
            debug_context.as_deref_mut(),
            &mut TypeDebugContext::default(),
            cached_func,
            module,
            thread_local_key,
            instance,
        );

        let mut global_asm = String::new();
        crate::base::compile_fn(
            &tcx.prof,
            tcx.dcx(),
            output_filenames,
            should_write_ir,
            cached_context,
            module,
            debug_context.as_deref_mut(),
            &mut global_asm,
            codegened_func,
        );
        if !global_asm.is_empty() {
            tcx.dcx().fatal("Inline asm is not supported in JIT mode");
        }
    });
}

pub(super) type SymbolLookup = Box<dyn Fn(&str) -> Option<*const u8> + Send + Sync>;

fn dep_symbol_lookup_fn(sess: &Session, crate_info: CrateInfo) -> SymbolLookup {
    use rustc_middle::middle::dependency_format::Linkage;

    let mut dylib_paths = Vec::new();

    let data = &crate_info.dependency_formats[&rustc_structures::CrateType::Executable];
    // `used_crates` is in reverse postorder in terms of dependencies. Reverse the order here to
    // get a postorder which ensures that all dependencies of a dylib are loaded before the dylib
    // itself. This helps the dynamic linker to find dylibs not in the regular dynamic library
    // search path.
    for &cnum in crate_info.used_crates.iter().rev() {
        let src = &crate_info.used_crate_source[&cnum];
        match data[cnum] {
            Linkage::NotLinked | Linkage::IncludedFromDylib => {}
            Linkage::Static => {
                let name = crate_info.crate_name[&cnum];
                let mut diag = sess.dcx().struct_err(format!("Can't load static lib {}", name));
                diag.note("rustc_codegen_cranelift can only load dylibs in JIT mode.");
                diag.emit();
            }
            Linkage::Dynamic => {
                dylib_paths.push(src.dylib.as_ref().unwrap().clone());
            }
        }
    }

    let imported_dylibs = Box::leak(
        dylib_paths
            .into_iter()
            .map(|path| unsafe { libloading::Library::new(&path).unwrap() })
            .collect::<Box<[_]>>(),
    );

    sess.dcx().abort_if_errors();

    Box::new(move |sym_name| {
        for dylib in &*imported_dylibs {
            if let Ok(sym) = unsafe { dylib.get::<*const u8>(sym_name.as_bytes()) } {
                return Some(*sym);
            }
        }
        None
    })
}
