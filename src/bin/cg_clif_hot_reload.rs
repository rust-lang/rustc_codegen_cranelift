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

use std::sync::mpsc;

use rustc_interface::interface;
use rustc_target::spec::PanicStrategy;

use rustc_codegen_cranelift::driver::jit::{run_jit, JitCompilationResult};

fn main() {
    rustc_driver::init_rustc_env_logger();
    rustc_driver::install_ice_hook();

    match run_compiler() {
        Ok(JitCompilationResult::Launch(start_fn, argc, argv)) => {
            std::process::exit(start_fn(argc, argv) as i32);
        }
        Ok(JitCompilationResult::Swapped) => unreachable!(),
        Err(_) => {}
    }
}

extern "C" fn __cg_clif_try_hot_swap() -> bool {
    match run_compiler() {
        Ok(JitCompilationResult::Launch(_, _, _)) => unreachable!(),
        Ok(JitCompilationResult::Swapped) => true,
        Err(_) => false,
    }
}

fn run_compiler() -> Result<JitCompilationResult, mpsc::RecvError> {
    let (tx, rx) = mpsc::sync_channel(0);
    let mut callbacks = HotReloadCallbacks { tx };
    std::thread::spawn(move || {
        rustc_driver::catch_with_exit_code(|| {
            let args = rustc_codegen_cranelift::driver::get_rustc_args();
            let mut run_compiler = rustc_driver::RunCompiler::new(&args, &mut callbacks);
            run_compiler.set_make_codegen_backend(Some(Box::new(move |_| {
                Box::new(rustc_codegen_cranelift::CraneliftCodegenBackend { config: None })
            })));
            run_compiler.run()
        })
    });
    rx.recv()
}

struct HotReloadCallbacks {
    tx: mpsc::SyncSender<JitCompilationResult>,
}

impl rustc_driver::Callbacks for HotReloadCallbacks {
    fn config(&mut self, config: &mut interface::Config) {
        config.opts.cg.panic = Some(PanicStrategy::Abort);
        config.opts.debugging_opts.panic_abort_tests = true;
        config.opts.maybe_sysroot = Some(config.opts.maybe_sysroot.clone().unwrap_or_else(|| {
            std::env::current_exe().unwrap().parent().unwrap().parent().unwrap().to_owned()
        }));
    }

    fn after_analysis<'tcx>(
        &mut self,
        compiler: &interface::Compiler,
        queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> rustc_driver::Compilation {
        compiler.session().abort_if_errors();
        queries.global_ctxt().unwrap().peek_mut().enter(|tcx| {
            let mut backend_config =
                rustc_codegen_cranelift::BackendConfig::from_opts(&tcx.sess.opts.cg.llvm_args)
                    .unwrap_or_else(|err| tcx.sess.fatal(&err));
            backend_config.codegen_mode = rustc_codegen_cranelift::CodegenMode::JitHotSwap;
            let res = run_jit(
                tcx,
                backend_config,
                vec![(
                    "__cg_clif_try_hot_swap".to_string(),
                    __cg_clif_try_hot_swap as extern "C" fn() -> bool as *const u8,
                )],
            );
            self.tx.send(res).unwrap();
        });
        rustc_driver::Compilation::Stop
    }
}
