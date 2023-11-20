// todo: pull out to a proper location. Really should be in `object` crate!
// todo: support windows-gnu flavor?
// todo: provide machine
// todo: remove any panics, nice errors

use std::ffi::CStr;

use object::{Object, ObjectSymbol};

mod coff;
mod data;
mod string_table;

use crate::dll_import_lib::coff::ImportDescriptorValues;
pub(crate) use coff::{Import, ImportNameType, ImportType, Machine};

pub(crate) struct ImportLibraryBuilder {
    dll_name: String,
    machine: Machine,
    members: Vec<ar_archive_writer::NewArchiveMember<'static>>,
}

impl ImportLibraryBuilder {
    pub(crate) fn new(dll_name: &str, machine: Machine) -> Self {
        let values = ImportDescriptorValues::new(dll_name.to_string(), machine);
        let mut members = Vec::new();
        members.push(coff_member(dll_name, coff::generate_import_descriptor(&values)));
        members.push(coff_member(
            dll_name,
            coff::generate_null_thunk_data(machine, &values.null_thunk_data_symbol),
        ));
        members.push(coff_member(dll_name, coff::generate_null_import_descriptor(machine)));
        Self { dll_name: values.dll_name, machine, members }
    }

    pub(crate) fn add_import(&mut self, import: Import) {
        self.members.push(import_member(&self.dll_name, self.machine, &import));
    }

    pub(crate) fn write<W>(&self, w: &mut W) -> std::io::Result<()>
    where
        W: ?Sized + std::io::Write + std::io::Seek,
    {
        let mut w = std::io::BufWriter::new(w);
        let write_symtab = true;
        let deterministic = true;
        let thin = false;
        ar_archive_writer::write_archive_to_stream(
            &mut w,
            &self.members,
            write_symtab,
            ar_archive_writer::ArchiveKind::Gnu,
            deterministic,
            thin,
        )?;
        // must flush before drop to ensure any final IO errors are reported.
        std::io::Write::flush(&mut w)?;
        Ok(())
    }
}

fn coff_member(dll_name: &str, buf: Vec<u8>) -> ar_archive_writer::NewArchiveMember<'static> {
    ar_archive_writer::NewArchiveMember {
        member_name: dll_name.to_string(),
        buf: Box::new(buf),
        get_symbols: coff_get_symbols,
        mtime: 0,
        uid: 0,
        gid: 0,
        perms: 0,
    }
}

fn import_member(
    dll_name: &str,
    machine: Machine,
    import: &Import,
) -> ar_archive_writer::NewArchiveMember<'static> {
    ar_archive_writer::NewArchiveMember {
        member_name: dll_name.to_string(),
        buf: Box::new(coff::write_short_import(dll_name, machine, import)),
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
    }
}

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
