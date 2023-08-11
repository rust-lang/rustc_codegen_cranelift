use std::env;
use std::path::Path;
use std::process::Command;

use crate::build_system::utils::{spawn_and_wait, Compiler};
use crate::build_system::{build_sysroot, SysrootKind};

use super::path::{Dirs, RelPath};
use super::prepare::GitRepo;
use super::rustc_info::get_file_name;
use super::utils::hyperfine_command;

static SIMPLE_RAYTRACER_REPO: GitRepo = GitRepo::github(
    "ebobby",
    "simple-raytracer",
    "804a7a21b9e673a482797aa289a18ed480e4d813",
    "<none>",
);

pub(crate) fn benchmark(
    dirs: &Dirs,
    channel: &str,
    sysroot_kind: SysrootKind,
    cg_clif_dylib_src: &Path,
    bootstrap_host_compiler: &Compiler,
    target_triple: String,
) {
    eprintln!("Benchmarking panic=unwind");

    build_sysroot::build_sysroot(
        &dirs,
        channel,
        sysroot_kind,
        cg_clif_dylib_src,
        &bootstrap_host_compiler,
        target_triple.clone(),
        false,
    );

    benchmark_simple_raytracer(dirs, false);

    eprintln!("Benchmarking panic=abort");

    build_sysroot::build_sysroot(
        &dirs,
        channel,
        sysroot_kind,
        cg_clif_dylib_src,
        &bootstrap_host_compiler,
        target_triple.clone(),
        true,
    );

    benchmark_simple_raytracer(dirs, true);
}

fn benchmark_simple_raytracer(dirs: &Dirs, panic_abort: bool) {
    if std::process::Command::new("hyperfine").output().is_err() {
        eprintln!("Hyperfine not installed");
        eprintln!("Hint: Try `cargo install hyperfine` to install hyperfine");
        std::process::exit(1);
    }

    if !SIMPLE_RAYTRACER_REPO.source_dir().to_path(dirs).exists() {
        SIMPLE_RAYTRACER_REPO.fetch(dirs);
    }

    let bench_runs = env::var("BENCH_RUNS").unwrap_or_else(|_| "10".to_string()).parse().unwrap();

    eprintln!("[BENCH COMPILE] ebobby/simple-raytracer");
    let cargo_clif =
        RelPath::DIST.to_path(dirs).join(get_file_name("cargo_clif", "bin").replace('_', "-"));
    let manifest_path = SIMPLE_RAYTRACER_REPO.source_dir().to_path(dirs).join("Cargo.toml");
    let target_dir = RelPath::BUILD.join("simple_raytracer").to_path(dirs);

    let do_build = |cmd: &str, channel: &str, out_exe: &str| {
        let _ = std::fs::remove_dir_all(&target_dir);

        let build_cmd = format!(
            "RUSTC=rustc {cmd} --manifest-path {manifest_path} --target-dir {target_dir}",
            manifest_path = manifest_path.display(),
            target_dir = target_dir.display(),
        );
        let mut bench = Command::new("sh");
        bench.arg("-c").arg(&build_cmd);
        eprintln!("{build_cmd}");
        spawn_and_wait(bench);

        let _ = std::fs::remove_file(RelPath::BUILD.to_path(dirs).join(out_exe));
        std::fs::hard_link(
            target_dir.join(channel).join("main"),
            RelPath::BUILD.to_path(dirs).join(out_exe),
        )
        .unwrap();
    };

    if !panic_abort {
        do_build("cargo build", "debug", "raytracer_cg_llvm");
        do_build("cargo build --release", "release", "raytracer_cg_llvm_opt");
    }

    do_build(
        &format!("{cargo_clif} build", cargo_clif = cargo_clif.display()),
        "debug",
        "raytracer_cg_clif",
    );
    do_build(
        &format!("{cargo_clif} build --release", cargo_clif = cargo_clif.display()),
        "release",
        "raytracer_cg_clif_opt",
    );

    eprintln!("[BENCH RUN] ebobby/simple-raytracer");

    if panic_abort {
        // cargo -Zbuild-std has a bug where compiling with panic=abort fails. As such we can't test
        // the LLVM backend with panic=abort. To avoid confusion skip running the benchmarks
        // compiled with LLVM when benchmarking panic=abort.

        hyperfine_command(
            0,
            bench_runs,
            None,
            &[
                Path::new(".").join(get_file_name("raytracer_cg_clif", "bin")).to_str().unwrap(),
                Path::new(".")
                    .join(get_file_name("raytracer_cg_clif_opt", "bin"))
                    .to_str()
                    .unwrap(),
            ],
            &RelPath::BUILD.to_path(dirs),
        );
    } else {
        hyperfine_command(
            0,
            bench_runs,
            None,
            &[
                Path::new(".").join(get_file_name("raytracer_cg_llvm", "bin")).to_str().unwrap(),
                Path::new(".")
                    .join(get_file_name("raytracer_cg_llvm_opt", "bin"))
                    .to_str()
                    .unwrap(),
                Path::new(".").join(get_file_name("raytracer_cg_clif", "bin")).to_str().unwrap(),
                Path::new(".")
                    .join(get_file_name("raytracer_cg_clif_opt", "bin"))
                    .to_str()
                    .unwrap(),
            ],
            &RelPath::BUILD.to_path(dirs),
        );
    }
}
