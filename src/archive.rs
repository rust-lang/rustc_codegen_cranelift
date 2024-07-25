use std::env;
use std::ffi::OsString;
use std::fmt::Write;
use std::path::{Path, PathBuf};

use rustc_codegen_ssa::back::archive::{
    ArArchiveBuilder, ArchiveBuilder, ArchiveBuilderBuilder, DEFAULT_OBJECT_READER,
};
use rustc_session::cstore::{DllCallingConvention, DllImport, PeImportNameType};
use rustc_session::Session;
use rustc_target::spec::Target;

pub(crate) struct ArArchiveBuilderBuilder;

fn is_mingw_gnu_toolchain(target: &Target) -> bool {
    target.vendor == "pc" && target.os == "windows" && target.env == "gnu" && target.abi.is_empty()
}

fn i686_decorated_name(dll_import: &DllImport, mingw: bool, disable_name_mangling: bool) -> String {
    let name = dll_import.name.as_str();

    let (add_prefix, add_suffix) = match dll_import.import_name_type {
        Some(PeImportNameType::NoPrefix) => (false, true),
        Some(PeImportNameType::Undecorated) => (false, false),
        _ => (true, true),
    };

    // Worst case: +1 for disable name mangling, +1 for prefix, +4 for suffix (@@__).
    let mut decorated_name = String::with_capacity(name.len() + 6);

    if disable_name_mangling {
        // LLVM uses a binary 1 ('\x01') prefix to a name to indicate that mangling needs to be disabled.
        decorated_name.push('\x01');
    }

    let prefix = if add_prefix && dll_import.is_fn {
        match dll_import.calling_convention {
            DllCallingConvention::C | DllCallingConvention::Vectorcall(_) => None,
            DllCallingConvention::Stdcall(_) => (!mingw
                || dll_import.import_name_type == Some(PeImportNameType::Decorated))
            .then_some('_'),
            DllCallingConvention::Fastcall(_) => Some('@'),
        }
    } else if !dll_import.is_fn && !mingw {
        // For static variables, prefix with '_' on MSVC.
        Some('_')
    } else {
        None
    };
    if let Some(prefix) = prefix {
        decorated_name.push(prefix);
    }

    decorated_name.push_str(name);

    if add_suffix && dll_import.is_fn {
        match dll_import.calling_convention {
            DllCallingConvention::C => {}
            DllCallingConvention::Stdcall(arg_list_size)
            | DllCallingConvention::Fastcall(arg_list_size) => {
                write!(&mut decorated_name, "@{arg_list_size}").unwrap();
            }
            DllCallingConvention::Vectorcall(arg_list_size) => {
                write!(&mut decorated_name, "@@{arg_list_size}").unwrap();
            }
        }
    }

    decorated_name
}

fn find_binutils_dlltool(sess: &Session) -> OsString {
    assert!(sess.target.options.is_like_windows && !sess.target.options.is_like_msvc);
    if let Some(dlltool_path) = &sess.opts.cg.dlltool {
        return dlltool_path.clone().into_os_string();
    }

    let tool_name: OsString = if sess.host.options.is_like_windows {
        // If we're compiling on Windows, always use "dlltool.exe".
        "dlltool.exe"
    } else {
        // On other platforms, use the architecture-specific name.
        match sess.target.arch.as_ref() {
            "x86_64" => "x86_64-w64-mingw32-dlltool",
            "x86" => "i686-w64-mingw32-dlltool",
            "aarch64" => "aarch64-w64-mingw32-dlltool",

            // For non-standard architectures (e.g., aarch32) fallback to "dlltool".
            _ => "dlltool",
        }
    }
    .into();

    // NOTE: it's not clear how useful it is to explicitly search PATH.
    for dir in env::split_paths(&env::var_os("PATH").unwrap_or_default()) {
        let full_path = dir.join(&tool_name);
        if full_path.is_file() {
            return full_path.into_os_string();
        }
    }

    // The user didn't specify the location of the dlltool binary, and we weren't able
    // to find the appropriate one on the PATH. Just return the name of the tool
    // and let the invocation fail with a hopefully useful error message.
    tool_name
}

impl ArchiveBuilderBuilder for ArArchiveBuilderBuilder {
    fn new_archive_builder<'a>(&self, sess: &'a Session) -> Box<dyn ArchiveBuilder + 'a> {
        Box::new(ArArchiveBuilder::new(sess, &DEFAULT_OBJECT_READER))
    }

    fn create_dll_import_lib(
        &self,
        sess: &Session,
        lib_name: &str,
        dll_imports: &[DllImport],
        tmpdir: &Path,
        is_direct_dependency: bool,
    ) -> PathBuf {
        let name_suffix = if is_direct_dependency { "_imports" } else { "_imports_indirect" };
        let output_path = tmpdir.join(format!("{lib_name}{name_suffix}.lib"));

        let target = &sess.target;
        let mingw_gnu_toolchain = is_mingw_gnu_toolchain(target);

        let import_name_and_ordinal_vector: Vec<(String, Option<u16>)> = dll_imports
            .iter()
            .map(|import: &DllImport| {
                if sess.target.arch == "x86" {
                    (i686_decorated_name(import, mingw_gnu_toolchain, false), import.ordinal())
                } else {
                    (import.name.to_string(), import.ordinal())
                }
            })
            .collect();

        if mingw_gnu_toolchain {
            // The binutils linker used on -windows-gnu targets cannot read the import
            // libraries generated by LLVM: in our attempts, the linker produced an .EXE
            // that loaded but crashed with an AV upon calling one of the imported
            // functions. Therefore, use binutils to create the import library instead,
            // by writing a .DEF file to the temp dir and calling binutils's dlltool.
            let def_file_path = tmpdir.join(format!("{lib_name}{name_suffix}.def"));

            let def_file_content = format!(
                "EXPORTS\n{}",
                import_name_and_ordinal_vector
                    .into_iter()
                    .map(|(name, ordinal)| {
                        match ordinal {
                            Some(n) => format!("{name} @{n} NONAME"),
                            None => name,
                        }
                    })
                    .collect::<Vec<String>>()
                    .join("\n")
            );

            match std::fs::write(&def_file_path, def_file_content) {
                Ok(_) => {}
                Err(e) => {
                    sess.dcx().fatal(format!("{e:?}"));
                }
            };

            // --no-leading-underscore: For the `import_name_type` feature to work, we need to be
            // able to control the *exact* spelling of each of the symbols that are being imported:
            // hence we don't want `dlltool` adding leading underscores automatically.
            let dlltool = find_binutils_dlltool(sess);
            let temp_prefix = {
                let mut path = PathBuf::from(&output_path);
                path.pop();
                path.push(lib_name);
                path
            };
            // dlltool target architecture args from:
            // https://github.com/llvm/llvm-project-release-prs/blob/llvmorg-15.0.6/llvm/lib/ToolDrivers/llvm-dlltool/DlltoolDriver.cpp#L69
            let (dlltool_target_arch, dlltool_target_bitness) = match sess.target.arch.as_ref() {
                "x86_64" => ("i386:x86-64", "--64"),
                "x86" => ("i386", "--32"),
                "aarch64" => ("arm64", "--64"),
                "arm" => ("arm", "--32"),
                _ => panic!("unsupported arch {}", sess.target.arch),
            };
            let mut dlltool_cmd = std::process::Command::new(&dlltool);
            dlltool_cmd
                .arg("-d")
                .arg(def_file_path)
                .arg("-D")
                .arg(lib_name)
                .arg("-l")
                .arg(&output_path)
                .arg("-m")
                .arg(dlltool_target_arch)
                .arg("-f")
                .arg(dlltool_target_bitness)
                .arg("--no-leading-underscore")
                .arg("--temp-prefix")
                .arg(temp_prefix);

            match dlltool_cmd.output() {
                Err(_e) => {
                    panic!();
                }
                // dlltool returns '0' on failure, so check for error output instead.
                Ok(output) if !output.stderr.is_empty() => {
                    panic!();
                }
                _ => {}
            }
        } else {
            let ffi_exports: Vec<ar_archive_writer::COFFShortExport> =
                import_name_and_ordinal_vector
                    .iter()
                    .map(|(name, ordinal)| ar_archive_writer::COFFShortExport {
                        name: name.clone(),
                        ext_name: None,
                        symbol_name: None,
                        alias_target: None,
                        ordinal: ordinal.unwrap_or(0),
                        noname: ordinal.is_some(),
                        data: false,
                        private: false,
                        constant: false,
                    })
                    .collect();
            let mut w = std::fs::File::create(output_path.clone()).unwrap();
            ar_archive_writer::write_import_library(
                &mut w,
                lib_name,
                &ffi_exports,
                match &*sess.target.arch {
                    "x86_64" => ar_archive_writer::MachineTypes::AMD64,
                    "x86" => ar_archive_writer::MachineTypes::I386,
                    "aarch64" => ar_archive_writer::MachineTypes::ARM64,
                    "arm64ec" => ar_archive_writer::MachineTypes::ARM64EC,
                    "arm" => ar_archive_writer::MachineTypes::ARMNT,
                    cpu => panic!("unsupported cpu type {cpu}"),
                },
                !sess.target.is_like_msvc,
            )
            .unwrap();
        };

        output_path
    }
}
