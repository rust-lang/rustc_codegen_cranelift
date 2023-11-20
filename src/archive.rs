use std::fs;
use std::path::{Path, PathBuf};

use rustc_codegen_ssa::back::archive::{
    get_native_object_symbols, ArArchiveBuilder, ArchiveBuilder, ArchiveBuilderBuilder,
};
use rustc_session::Session;

struct UnsupportedTargetForRawDyLib;

pub(crate) struct ArArchiveBuilderBuilder;

impl ArchiveBuilderBuilder for ArArchiveBuilderBuilder {
    fn new_archive_builder<'a>(&self, sess: &'a Session) -> Box<dyn ArchiveBuilder<'a> + 'a> {
        Box::new(ArArchiveBuilder::new(sess, get_import_or_native_object_symbols))
    }

    fn create_dll_import_lib(
        &self,
        sess: &Session,
        lib_name: &str,
        dll_imports: &[rustc_session::cstore::DllImport],
        tmpdir: &Path,
        _is_direct_dependency: bool,
    ) -> PathBuf {
        if sess.target.arch != "x86_64" || !sess.target.is_like_msvc {
            sess.span_fatal(
                dll_imports.iter().map(|import| import.span).collect::<Vec<_>>(),
                "cranelift codegen currently only supports raw_dylib on x86_64 msvc targets.",
            )
        }

        let mut import_lib = crate::dll_import_lib::ImportLibraryBuilder::new(
            lib_name,
            crate::dll_import_lib::Machine::X86_64,
        );

        for import in dll_imports {
            import_lib.add_import(crate::dll_import_lib::Import {
                symbol_name: import.name.to_string(),
                ordinal_or_hint: import.ordinal(),
                name_type: match import.import_name_type {
                    Some(rustc_session::cstore::PeImportNameType::Ordinal(_)) => {
                        crate::dll_import_lib::ImportNameType::Ordinal
                    }
                    None | Some(rustc_session::cstore::PeImportNameType::Decorated) => {
                        crate::dll_import_lib::ImportNameType::Name
                    }
                    Some(rustc_session::cstore::PeImportNameType::NoPrefix) => {
                        crate::dll_import_lib::ImportNameType::NameNoPrefix
                    }
                    Some(rustc_session::cstore::PeImportNameType::Undecorated) => {
                        crate::dll_import_lib::ImportNameType::NameUndecorate
                    }
                },
                import_type: crate::dll_import_lib::ImportType::Code,
            });
        }

        let lib_path = tmpdir.join(format!(
            "{prefix}{lib_name}_import{suffix}",
            prefix = sess.target.staticlib_prefix,
            suffix = sess.target.staticlib_suffix,
        ));

        let mut file = match fs::OpenOptions::new().write(true).create_new(true).open(&lib_path) {
            Ok(file) => file,
            Err(error) => {
                sess.fatal(format!(
                    "failed to create import library file `{path}`: {error}",
                    path = lib_path.display(),
                ));
            }
        };

        // import_lib.write() internally uses BufWriter, so we don't need anything here.
        if let Err(error) = import_lib.write(&mut file) {
            sess.fatal(format!(
                "failed to write import library `{path}`: {error}",
                path = lib_path.display(),
            ));
        }

        lib_path
    }
}

fn get_import_or_native_object_symbols(
    buf: &[u8],
    f: &mut dyn FnMut(&[u8]) -> std::io::Result<()>,
) -> std::io::Result<bool> {
    let sig1 = u16::from_le_bytes([buf[0], buf[1]]);
    let sig2 = u16::from_le_bytes([buf[2], buf[3]]);
    if sig1 == 0 && sig2 == 0xFFFF {
        let data = &buf[20..];
        let name_end =
            data.iter().position(|&c| c == b'\0').expect("import object missing name terminator");
        let name = &data[..name_end];
        f(name)?;
        Ok(true)
    } else {
        get_native_object_symbols(buf, f)
    }
}
