use std::fs;
use std::path::{Path, PathBuf};

use rustc_codegen_ssa::back::archive::{
    get_native_object_symbols, ArArchiveBuilder, ArchiveBuilder, ArchiveBuilderBuilder,
};
use rustc_session::Session;

pub(crate) struct ArArchiveBuilderBuilder;

impl ArchiveBuilderBuilder for ArArchiveBuilderBuilder {
    fn new_archive_builder<'a>(&self, sess: &'a Session) -> Box<dyn ArchiveBuilder<'a> + 'a> {
        Box::new(ArArchiveBuilder::new(sess, get_import_or_native_object_symbols))
    }

    fn create_dll_import_lib(
        &self,
        _sess: &Session,
        lib_name: &str,
        dll_imports: &[rustc_session::cstore::DllImport],
        tmpdir: &Path,
        _is_direct_dependency: bool,
    ) -> PathBuf {
        let mut import_names = Vec::new();
        for dll_import in dll_imports {
            import_names.push(dll_import.name.as_str());
        }
        let lib_path = tmpdir.join(format!("{lib_name}_import.lib"));
        // todo: emit session error instead of expects
        fs::write(&lib_path, crate::dll_import_lib::generate(lib_name, &import_names))
            .expect("failed to write import library");

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
