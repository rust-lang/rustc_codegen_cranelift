#![feature(rustc_private)]

extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_target;

use rustc_codegen_cranelift::{BackendConfig, CodegenMode};
use rustc_data_structures::profiling::{get_resident_set_size, print_time_passes_entry};
use rustc_errors::ErrorReported;
use rustc_interface::interface;
use rustc_target::spec::PanicStrategy;

use rustc_codegen_cranelift::driver::jit::{run_jit, JitCompilationResult};

struct CraneliftCallbacks<'a> {
    time_passes: &'a mut bool,
    compilation_result: Option<JitCompilationResult>,
}

impl rustc_driver::Callbacks for CraneliftCallbacks<'_> {
    fn config(&mut self, config: &mut interface::Config) {
        // If a --prints=... option has been given, we don't print the "total"
        // time because it will mess up the --prints output. See #64339.
        *self.time_passes = config.opts.prints.is_empty()
            && (config.opts.debugging_opts.time_passes || config.opts.debugging_opts.time);

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
            let backend_config = BackendConfig::from_opts(&tcx.sess.opts.cg.llvm_args)
                .unwrap_or_else(|err| tcx.sess.fatal(&err));
            match backend_config.codegen_mode {
                // Regular AOT compilation. Continue to the CodegenBackend implementation.
                CodegenMode::Aot => rustc_driver::Compilation::Continue,
                // Lazy JIT requires running on the rustc thread. Continue to the CodegenBackend
                // implementation.
                // FIXME allow running in the main thread
                CodegenMode::JitLazy => rustc_driver::Compilation::Continue,
                // Regular and hot swapping JIT can run on the main thread.
                CodegenMode::Jit | CodegenMode::JitHotSwap => {
                    #[cfg(not(feature = "jit"))]
                    tcx.sess
                        .fatal("jit support was disabled when compiling rustc_codegen_cranelift");

                    #[cfg(feature = "jit")]
                    {
                        let res = run_jit(
                            tcx,
                            backend_config,
                            vec![(
                                "__cg_clif_try_hot_swap".to_string(),
                                __cg_clif_try_hot_swap as extern "C" fn() -> bool as *const u8,
                            )],
                        );
                        assert!(self.compilation_result.is_none());
                        self.compilation_result = Some(res);
                        rustc_driver::Compilation::Stop
                    }
                }
            }
        })
    }
}

extern "C" fn __cg_clif_try_hot_swap() -> bool {
    let mut callbacks = CraneliftCallbacks { time_passes: &mut false, compilation_result: None };

    let res = rustc_driver::catch_fatal_errors(|| {
        let args = rustc_codegen_cranelift::driver::get_rustc_args();
        let mut run_compiler = rustc_driver::RunCompiler::new(&args, &mut callbacks);
        run_compiler.set_make_codegen_backend(Some(Box::new(move |_| {
            Box::new(rustc_codegen_cranelift::CraneliftCodegenBackend { config: None })
        })));
        run_compiler.run()
    })
    .and_then(|res| res);
    match res {
        Ok(()) => true,
        Err(ErrorReported) => false,
    }
}

fn main() {
    let start_time = std::time::Instant::now();
    let start_rss = get_resident_set_size();
    rustc_driver::init_rustc_env_logger();
    rustc_driver::install_ice_hook();

    let mut time_passes = false;
    let mut callbacks =
        CraneliftCallbacks { time_passes: &mut time_passes, compilation_result: None };

    let res = rustc_driver::catch_fatal_errors(|| {
        let args = rustc_codegen_cranelift::driver::get_rustc_args();
        let mut run_compiler = rustc_driver::RunCompiler::new(&args, &mut callbacks);
        run_compiler.set_make_codegen_backend(Some(Box::new(move |_| {
            Box::new(rustc_codegen_cranelift::CraneliftCodegenBackend { config: None })
        })));
        run_compiler.run()?;
        Ok(callbacks.compilation_result)
    })
    .and_then(|res| res);

    if time_passes {
        let end_rss = get_resident_set_size();
        print_time_passes_entry("total", start_time.elapsed(), start_rss, end_rss);
    }

    match res {
        // Regular AOT compilation
        Ok(None) => std::process::exit(rustc_driver::EXIT_SUCCESS),
        Ok(Some(JitCompilationResult::Launch(start_fn, argc, argv))) => {
            std::process::exit(start_fn(argc, argv) as i32);
        }
        Ok(Some(JitCompilationResult::Swapped)) => unreachable!(),
        Err(ErrorReported) => std::process::exit(rustc_driver::EXIT_FAILURE),
    }
}
