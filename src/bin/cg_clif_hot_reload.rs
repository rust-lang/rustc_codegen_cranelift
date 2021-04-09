#![feature(rustc_private, never_type)]

#[cfg(not(feature = "jit"))]
compile_error!("The jit feature needs to be enabled for cg_clif_hot_reload");

extern crate rustc_codegen_ssa;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

use std::any::Any;
use std::sync::{mpsc, Mutex};

use rustc_codegen_ssa::traits::CodegenBackend;
use rustc_codegen_ssa::CodegenResults;
use rustc_data_structures::fx::FxHashMap;
use rustc_errors::ErrorReported;
use rustc_hir::def_id::LOCAL_CRATE;
use rustc_interface::interface;
use rustc_middle::dep_graph::{WorkProduct, WorkProductId};
use rustc_middle::middle::cstore::{EncodedMetadata, MetadataLoader};
use rustc_middle::mir::mono::MonoItem;
use rustc_middle::ty::query::Providers;
use rustc_middle::ty::TyCtxt;
use rustc_session::config::OutputFilenames;
use rustc_session::Session;
use rustc_target::spec::PanicStrategy;

use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};

use rustc_codegen_cranelift::*;

struct CraneliftPassesCallbacks;

impl rustc_driver::Callbacks for CraneliftPassesCallbacks {
    fn config(&mut self, config: &mut interface::Config) {
        config.opts.cg.panic = Some(PanicStrategy::Abort);
        config.opts.debugging_opts.panic_abort_tests = true;
        config.opts.maybe_sysroot = Some(config.opts.maybe_sysroot.clone().unwrap_or_else(|| {
            std::env::current_exe().unwrap().parent().unwrap().parent().unwrap().to_owned()
        }));
    }
}

lazy_static::lazy_static! {
    static ref HOT_RELOAD_STATE: Mutex<Option<HotReloadState>> = Mutex::new(None);
}

fn main() {
    rustc_driver::init_rustc_env_logger();
    rustc_driver::install_ice_hook();

    match run_compiler() {
        Ok(CompilationResult::Successful(start_fn)) => {
            std::process::exit(start_fn(0, std::ptr::null()) as i32);
        }
        Err(_) => {}
    }
}

extern "C" fn __cg_clif_try_hot_swap() {
    let _ = run_compiler();
}

fn run_compiler() -> Result<CompilationResult, mpsc::RecvError> {
    let (tx, rx) = mpsc::sync_channel(0);
    let mut callbacks = CraneliftPassesCallbacks;
    std::thread::spawn(move || {
        rustc_driver::catch_with_exit_code(|| {
            let args = rustc_codegen_cranelift::driver::get_rustc_args();
            let mut run_compiler = rustc_driver::RunCompiler::new(&args, &mut callbacks);
            run_compiler.set_make_codegen_backend(Some(Box::new(move |_| {
                Box::new(HotReloadCodegenBackend { tx })
            })));
            run_compiler.run()
        })
    });
    rx.recv()
}

struct HotReloadState {
    backend_config: BackendConfig,
    jit_module: JITModule,
}

// FIXME
unsafe impl Send for HotReloadState {}

enum CompilationResult {
    Successful(extern "C" fn(usize, *const *const u8) -> isize),
}

struct HotReloadCodegenBackend {
    tx: mpsc::SyncSender<CompilationResult>,
}

impl CodegenBackend for HotReloadCodegenBackend {
    fn init(&self, sess: &Session) {
        use rustc_session::config::Lto;
        match sess.lto() {
            Lto::No | Lto::ThinLocal => {}
            Lto::Thin | Lto::Fat => sess.warn("LTO is not supported. You may get a linker error."),
        }
    }

    fn metadata_loader(&self) -> Box<dyn MetadataLoader + Sync> {
        Box::new(CraneliftMetadataLoader)
    }

    fn provide(&self, _providers: &mut Providers) {}
    fn provide_extern(&self, _providers: &mut Providers) {}

    fn target_features(&self, _sess: &Session) -> Vec<rustc_span::Symbol> {
        vec![]
    }

    fn codegen_crate(
        &self,
        tcx: TyCtxt<'_>,
        _metadata: EncodedMetadata,
        _need_metadata_module: bool,
    ) -> Box<dyn Any> {
        tcx.sess.abort_if_errors();

        let mut locked_hot_reload_state =
            HOT_RELOAD_STATE.lock().unwrap_or_else(|_| std::process::exit(1));

        let is_fresh = locked_hot_reload_state.is_none();
        if is_fresh {
            let backend_config = BackendConfig::from_opts(&tcx.sess.opts.cg.llvm_args)
                .unwrap_or_else(|err| tcx.sess.fatal(&err));
            let (jit_module, _cx) = driver::jit::create_jit_module(
                tcx,
                &backend_config,
                true,
                vec![(
                    "__cg_clif_try_hot_swap".to_string(),
                    __cg_clif_try_hot_swap as extern "C" fn() as *const u8,
                )],
            );

            *locked_hot_reload_state = Some(HotReloadState { backend_config, jit_module });
        }

        let hot_reload_state = locked_hot_reload_state.as_mut().unwrap();

        let (_, cgus) = tcx.collect_and_partition_mono_items(LOCAL_CRATE);
        let mono_items = cgus
            .iter()
            .map(|cgu| cgu.items_in_deterministic_order(tcx).into_iter())
            .flatten()
            .collect::<FxHashMap<_, (_, _)>>()
            .into_iter()
            .collect::<Vec<(_, (_, _))>>();

        let mut cx = CodegenCx::new(
            tcx,
            hot_reload_state.backend_config.clone(),
            hot_reload_state.jit_module.isa(),
            false,
        );

        driver::time(
            tcx,
            hot_reload_state.backend_config.display_cg_time,
            "codegen mono items",
            || {
                let defined_functions = hot_reload_state
                    .jit_module
                    .declarations()
                    .get_functions()
                    .filter_map(|(func_id, func)| {
                        if func.linkage != Linkage::Import && func.name != "main" {
                            Some(func_id)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                for func_id in defined_functions {
                    hot_reload_state.jit_module.prepare_for_function_redefine(func_id).unwrap();
                }

                driver::predefine_mono_items(tcx, &mut hot_reload_state.jit_module, &mono_items);
                for (mono_item, _) in mono_items {
                    match mono_item {
                        MonoItem::Fn(inst) => {
                            cx.tcx.sess.time("codegen fn", || {
                                crate::base::codegen_fn(
                                    &mut cx,
                                    &mut hot_reload_state.jit_module,
                                    inst,
                                )
                            });
                        }
                        MonoItem::Static(def_id) => {
                            if is_fresh {
                                crate::constant::codegen_static(
                                    tcx,
                                    &mut hot_reload_state.jit_module,
                                    def_id,
                                );
                            }
                        }
                        MonoItem::GlobalAsm(item_id) => {
                            let item = tcx.hir().item(item_id);
                            tcx.sess
                                .span_fatal(item.span, "Global asm is not supported in JIT mode");
                        }
                    }
                }
            },
        );

        if !cx.global_asm.is_empty() {
            tcx.sess.fatal("Inline asm is not supported in JIT mode");
        }

        tcx.sess.abort_if_errors();

        hot_reload_state.jit_module.finalize_definitions();
        let main_fn = driver::jit::get_main_fn(&mut hot_reload_state.jit_module);

        std::mem::drop(locked_hot_reload_state);
        self.tx.send(CompilationResult::Successful(main_fn)).unwrap();
        std::panic::resume_unwind(Box::new(()));
    }

    fn join_codegen(
        &self,
        _ongoing_codegen: Box<dyn Any>,
        _sess: &Session,
    ) -> Result<(CodegenResults, FxHashMap<WorkProductId, WorkProduct>), ErrorReported> {
        unreachable!();
    }

    fn link(
        &self,
        _sess: &Session,
        _codegen_results: CodegenResults,
        _outputs: &OutputFilenames,
    ) -> Result<(), ErrorReported> {
        unreachable!();
    }
}
