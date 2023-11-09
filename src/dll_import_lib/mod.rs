// todo: pull out to a proper location. Really should be in `object` crate!
// todo: support ordinals
// todo: support name types (e.g. verbatim+)
// todo: support windows-gnu flavor?
// todo: provide machine
// todo: remove any panics, nice errors

use std::ffi::CStr;

use object::{Object, ObjectSymbol};

use data::DataWriter;

mod data;
mod string_table;

mod coff;

pub(crate) fn generate(dll_name: &str, import_names: &[&str]) -> Vec<u8> {
    let mut members = Vec::new();

    // foo.dll => foo so we can construct the import descriptor symbol.
    // At least for the Windows system dlls, don't seem to need any further
    // escaping, e.g. "api-ms-win-appmodel-runtime-l1-1-1.dll" =>
    // "__IMPORT_DESCRIPTOR_api-ms-win-appmodel-runtime-l1-1-1"
    // Not using std::path to avoid having to handle non-unicode paths.
    let mut dll_basename = String::from(dll_name);
    if let Some(index) = dll_basename.rfind('.') {
        dll_basename.truncate(index);
    }

    let import_descriptor_symbol = format!("__IMPORT_DESCRIPTOR_{dll_basename}");
    let null_thunk_data_symbol = format!("\x7f{dll_basename}_NULL_THUNK_DATA");

    fn coff_get_symbols(
        buf: &[u8],
        f: &mut dyn FnMut(&[u8]) -> std::io::Result<()>,
    ) -> std::io::Result<bool> {
        type NtCoffFile<'data> =
            object::read::coff::CoffFile<'data, &'data [u8], object::pe::ImageFileHeader>;
        let file = NtCoffFile::parse(buf).unwrap();
        for symbol in file.symbols() {
            if symbol.is_definition() {
                f(symbol.name_bytes().unwrap())?;
            }
        }
        Ok(true)
    }

    {
        // import descriptor member
        let mut buf = DataWriter::new();
        coff::write_import_descriptor(
            &mut buf,
            dll_name,
            &import_descriptor_symbol,
            &null_thunk_data_symbol,
        );
        members.push(ar_archive_writer::NewArchiveMember {
            member_name: dll_name.to_string(),
            buf: Box::new(buf.into_data()),
            get_symbols: coff_get_symbols,
            mtime: 0,
            uid: 0,
            gid: 0,
            perms: 0,
        });
    }

    {
        // null thunk data member
        let mut buf = DataWriter::new();
        coff::write_null_thunk_data(&mut buf, &null_thunk_data_symbol);
        members.push(ar_archive_writer::NewArchiveMember {
            member_name: dll_name.to_string(),
            buf: Box::new(buf.into_data()),
            get_symbols: coff_get_symbols,
            mtime: 0,
            uid: 0,
            gid: 0,
            perms: 0,
        });
    }

    {
        // null import descriptor member
        let mut buf = DataWriter::new();
        coff::write_null_import_descriptor(&mut buf);
        members.push(ar_archive_writer::NewArchiveMember {
            member_name: dll_name.to_string(),
            buf: Box::new(buf.into_data()),
            get_symbols: coff_get_symbols,
            mtime: 0,
            uid: 0,
            gid: 0,
            perms: 0,
        });
    }

    // short import object members
    for name in import_names.iter() {
        let mut buf = DataWriter::new();
        coff::write_short_import(&mut buf, dll_name, name, None);
        members.push(ar_archive_writer::NewArchiveMember {
            member_name: dll_name.to_string(),
            buf: Box::new(buf.into_data()),
            get_symbols: |buf, f| {
                const NAME_OFFSET: usize = std::mem::size_of::<object::pe::ImportObjectHeader>();
                let name = CStr::from_bytes_until_nul(&buf[NAME_OFFSET..]).unwrap();
                f(name.to_bytes())?;
                f(format!("__imp_{}", name.to_str().unwrap()).as_bytes())?;
                Ok(true)
            },
            mtime: 0,
            uid: 0,
            gid: 0,
            perms: 0,
        });
    }

    let mut result = Vec::new();
    let write_symtab = true;
    let deterministic = true;
    let thin = false;
    ar_archive_writer::write_archive_to_stream(
        &mut std::io::Cursor::new(&mut result),
        &members,
        write_symtab,
        ar_archive_writer::ArchiveKind::Gnu,
        deterministic,
        thin,
    )
    .expect("write ar failed");

    result
}
