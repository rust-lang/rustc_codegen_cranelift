use std::env;
use std::ffi::OsString;
#[cfg(unix)]
use std::os::unix::process::CommandExt;
use std::process::Command;

fn main() {
    let current_exe = env::current_exe().unwrap();
    let mut sysroot = current_exe.parent().unwrap();
    if sysroot.file_name().unwrap().to_str().unwrap() == "bin" {
        sysroot = sysroot.parent().unwrap();
    }

    let cg_clif_dylib_path = sysroot.join(if cfg!(windows) { "bin" } else { "lib" }).join(
        env::consts::DLL_PREFIX.to_string() + "rustc_codegen_cranelift" + env::consts::DLL_SUFFIX,
    );

    let passed_args = std::env::args_os().skip(1).collect::<Vec<_>>();
    let mut args = vec![];
    let mut codegen_backend_arg = OsString::from("-Zcodegen-backend=");
    codegen_backend_arg.push(cg_clif_dylib_path);
    args.push(codegen_backend_arg);
    if !passed_args.contains(&OsString::from("--sysroot")) {
        args.push(OsString::from("--sysroot"));
        args.push(OsString::from(sysroot.to_str().unwrap()));
    }
    args.extend(passed_args);

    // Ensure that the right toolchain is used
    env::set_var("RUSTUP_TOOLCHAIN", env!("TOOLCHAIN_NAME"));

    #[cfg(unix)]
    panic!("Failed to spawn rustc: {}", Command::new("rustc").args(args).exec());

    #[cfg(not(unix))]
    std::process::exit(
        Command::new("rustc").args(args).spawn().unwrap().wait().unwrap().code().unwrap_or(1),
    );
}
