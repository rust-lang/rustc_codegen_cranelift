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
    if std::process::Command::new("hyperfine").output().is_err() {
        eprintln!("Hyperfine not installed");
        eprintln!("Hint: Try `cargo install hyperfine` to install hyperfine");
        std::process::exit(1);
    }

    benchmark_raytracer(
        dirs,
        channel,
        sysroot_kind,
        cg_clif_dylib_src,
        bootstrap_host_compiler,
        target_triple.clone(),
    );

    benchmark_css_parser(
        dirs,
        channel,
        sysroot_kind,
        cg_clif_dylib_src,
        bootstrap_host_compiler,
        target_triple,
    );
}

pub(crate) fn benchmark_raytracer(
    dirs: &Dirs,
    channel: &str,
    sysroot_kind: SysrootKind,
    cg_clif_dylib_src: &Path,
    bootstrap_host_compiler: &Compiler,
    target_triple: String,
) {
    if !SIMPLE_RAYTRACER_REPO.source_dir().to_path(dirs).exists() {
        SIMPLE_RAYTRACER_REPO.fetch(dirs);
    }

    let bench_runs = env::var("BENCH_RUNS").unwrap_or_else(|_| "10".to_string()).parse().unwrap();

    eprintln!("[BUILD] ebobby/simple-raytracer");
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

    do_build("cargo build", "debug", "raytracer_cg_llvm_unwind");

    build_sysroot::build_sysroot(
        &dirs,
        channel,
        sysroot_kind,
        cg_clif_dylib_src,
        &bootstrap_host_compiler,
        target_triple.clone(),
        false,
    );

    do_build(
        &format!("{cargo_clif} build", cargo_clif = cargo_clif.display()),
        "debug",
        "raytracer_cg_clif_unwind",
    );
    do_build(
        &format!("{cargo_clif} build --release", cargo_clif = cargo_clif.display()),
        "release",
        "raytracer_cg_clif_unwind_opt",
    );

    build_sysroot::build_sysroot(
        &dirs,
        channel,
        sysroot_kind,
        cg_clif_dylib_src,
        &bootstrap_host_compiler,
        target_triple.clone(),
        true,
    );

    // cargo build -Zbuild-std has a bug where compiling with panic=abort fails. As such we can't
    // test the LLVM backend with panic=abort. To avoid confusion skip running the benchmarks
    // compiled with LLVM when benchmarking panic=abort.

    do_build(
        &format!("{cargo_clif} build", cargo_clif = cargo_clif.display()),
        "debug",
        "raytracer_cg_clif_abort",
    );
    do_build(
        &format!("{cargo_clif} build --release", cargo_clif = cargo_clif.display()),
        "release",
        "raytracer_cg_clif_abort_opt",
    );

    eprintln!("[BENCH RUN] ebobby/simple-raytracer");

    hyperfine_command(
        0,
        bench_runs,
        None,
        &[
            Path::new(".").join(get_file_name("raytracer_cg_llvm_unwind", "bin")).to_str().unwrap(),
            Path::new(".").join(get_file_name("raytracer_cg_clif_unwind", "bin")).to_str().unwrap(),
            Path::new(".")
                .join(get_file_name("raytracer_cg_clif_unwind_opt", "bin"))
                .to_str()
                .unwrap(),
            Path::new(".").join(get_file_name("raytracer_cg_clif_abort", "bin")).to_str().unwrap(),
            Path::new(".")
                .join(get_file_name("raytracer_cg_clif_abort_opt", "bin"))
                .to_str()
                .unwrap(),
        ],
        &RelPath::BUILD.to_path(dirs),
    );
}

pub(crate) fn benchmark_css_parser(
    dirs: &Dirs,
    channel: &str,
    sysroot_kind: SysrootKind,
    cg_clif_dylib_src: &Path,
    bootstrap_host_compiler: &Compiler,
    target_triple: String,
) {
    let bench_runs = env::var("BENCH_RUNS").unwrap_or_else(|_| "10".to_string()).parse().unwrap();

    eprintln!("[BUILD] rust-lang/rustc-perf: runtime-benchmarks/css");
    let cargo_clif =
        RelPath::DIST.to_path(dirs).join(get_file_name("cargo_clif", "bin").replace('_', "-"));
    let manifest_path =
        RelPath::SOURCE.join("rustc_perf_bench_css").to_path(dirs).join("Cargo.toml");
    let target_dir = RelPath::BUILD.join("css_bench").to_path(dirs);

    if !manifest_path.parent().unwrap().join("data").join("fb.css").exists() {
        std::fs::create_dir_all(manifest_path.parent().unwrap().join("data")).unwrap();
        let mut download_css = Command::new("wget");
        download_css.arg("https://github.com/rust-lang/rustc-perf/raw/master/collector/runtime-benchmarks/css/data/fb.css");
        download_css.current_dir(manifest_path.parent().unwrap().join("data"));
        spawn_and_wait(download_css);
    }

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
            target_dir.join(channel).join("css-bench"),
            RelPath::BUILD.to_path(dirs).join(out_exe),
        )
        .unwrap();
    };

    do_build("cargo build", "debug", "css_bench_cg_llvm_unwind");

    build_sysroot::build_sysroot(
        &dirs,
        channel,
        sysroot_kind,
        cg_clif_dylib_src,
        &bootstrap_host_compiler,
        target_triple.clone(),
        false,
    );

    do_build(
        &format!("{cargo_clif} build", cargo_clif = cargo_clif.display()),
        "debug",
        "css_bench_cg_clif_unwind",
    );
    do_build(
        &format!("{cargo_clif} build --release", cargo_clif = cargo_clif.display()),
        "release",
        "css_bench_cg_clif_unwind_opt",
    );

    build_sysroot::build_sysroot(
        &dirs,
        channel,
        sysroot_kind,
        cg_clif_dylib_src,
        &bootstrap_host_compiler,
        target_triple.clone(),
        true,
    );

    // cargo build -Zbuild-std has a bug where compiling with panic=abort fails. As such we can't
    // test the LLVM backend with panic=abort. To avoid confusion skip running the benchmarks
    // compiled with LLVM when benchmarking panic=abort.

    do_build(
        &format!("{cargo_clif} build", cargo_clif = cargo_clif.display()),
        "debug",
        "css_bench_cg_clif_abort",
    );
    do_build(
        &format!("{cargo_clif} build --release", cargo_clif = cargo_clif.display()),
        "release",
        "css_bench_cg_clif_abort_opt",
    );

    eprintln!("[BENCH RUN] rust-lang/rustc-perf: runtime-benchmarks/css");

    hyperfine_command(
        0,
        bench_runs,
        None,
        &[
            Path::new(".").join(get_file_name("css_bench_cg_llvm_unwind", "bin")).to_str().unwrap(),
            Path::new(".").join(get_file_name("css_bench_cg_clif_unwind", "bin")).to_str().unwrap(),
            Path::new(".")
                .join(get_file_name("css_bench_cg_clif_unwind_opt", "bin"))
                .to_str()
                .unwrap(),
            Path::new(".").join(get_file_name("css_bench_cg_clif_abort", "bin")).to_str().unwrap(),
            Path::new(".")
                .join(get_file_name("css_bench_cg_clif_abort_opt", "bin"))
                .to_str()
                .unwrap(),
        ],
        &RelPath::BUILD.to_path(dirs),
    );
}
