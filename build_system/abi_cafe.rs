use crate::build_sysroot;
use crate::path::Dirs;
use crate::prepare::GitRepo;
use crate::utils::{spawn_and_wait, CargoProject, Compiler};
use crate::{CodegenBackend, SysrootKind};

static ABI_CAFE_REPO: GitRepo = GitRepo::github(
    "Gankra",
    "abi-cafe",
    "dd3747097ad3b15b19869f24db6102d0baf4d2bd",
    "163bdaa82c859661",
    "abi-cafe",
);

static ABI_CAFE: CargoProject = CargoProject::new(&ABI_CAFE_REPO.source_dir(), "abi_cafe_target");

pub(crate) fn run(
    channel: &str,
    sysroot_kind: SysrootKind,
    dirs: &Dirs,
    cg_clif_dylib: &CodegenBackend,
    rustup_toolchain_name: Option<&str>,
    bootstrap_host_compiler: &Compiler,
) {
    ABI_CAFE_REPO.fetch(dirs);
    ABI_CAFE_REPO.patch(dirs);

    eprintln!("Building sysroot for abi-cafe");
    build_sysroot::build_sysroot(
        dirs,
        channel,
        sysroot_kind,
        cg_clif_dylib,
        bootstrap_host_compiler,
        rustup_toolchain_name,
        bootstrap_host_compiler.triple.clone(),
    );

    eprintln!("Running abi-cafe");

    let pairs = ["rustc_calls_cgclif", "cgclif_calls_rustc", "cgclif_calls_cc", "cc_calls_cgclif"];
    let pairs =
        if cfg!(not(any(target_os = "macos", all(target_os = "windows", target_env = "msvc")))) {
            &pairs[..]
        } else {
            &pairs[..2]
        };

    let mut cmd = ABI_CAFE.run(bootstrap_host_compiler, dirs);
    cmd.arg("--");
    for pair in pairs {
        cmd.arg("--pairs").arg(pair);
    }
    cmd.arg("--add-rustc-codegen-backend");
    match cg_clif_dylib {
        CodegenBackend::Local(path) => {
            cmd.arg(format!("cgclif:{}", path.display()));
        }
        CodegenBackend::Builtin(name) => {
            cmd.arg(format!("cgclif:{name}"));
        }
    }
    cmd.current_dir(ABI_CAFE.source_dir(dirs));

    spawn_and_wait(cmd);
}
