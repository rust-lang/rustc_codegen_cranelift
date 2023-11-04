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
        fs::write(&lib_path, windows_import_lib::generate(lib_name, &import_names))
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

// todo: pull out to a proper location. Really should be in `object` crate!
// todo: support ordinals
// todo: support name types (e.g. verbatim+)
// todo: support windows-gnu flavor?
// todo: provide machine
// todo: remove any panics, nice errors
mod windows_import_lib {
    use std::ops::{Deref, DerefMut};

    // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#archive-library-file-format
    //
    // Windows .lib files are System-V (aka. GNU) flavored ar files with a couple of extra lookup
    // members.
    //
    // An archive is the 8 bytes b"!<arch>\n"
    // followed by a sequence of 60 byte member headers:
    //   0: name: [u8; 16], // member name, terminated with "/". If it is longer than 15, then
    //                      // use "/n" where "n" is a decimal for the offset in bytes into
    //                      // the longnames ("//") member contents.
    //  16: date: [u8; 12], // ASCII decimal seconds since UNIX epoch - always -1 for MSVC
    //  28: uid: [u8; 6],   // ASCII decimal user id. Always blank for MSVC
    //  34: gid: [u8; 6],   // ditto for group id.
    //  40: mode: [u8; 8],  // ASCII octal UNIX mode. 0 for MSVC
    //  48: size: [u8; 10], // ASCII decimal data size.
    //  58: end: b"`\n",
    // then size bytes of payload. If payload is odd sized, pad
    // to an even offset with \n.
    //
    // You must store two extra members at the start, a standard System-V / GNU symbol lookup table
    // member (the "first linker member" in the linked documentation), and the Windows symbol table,
    // (the "second linker member") both with empty ("/") names.
    //
    // The standard System-V/GNU symbol table member has the name "/" with following contents,
    // using big-endian numbers:
    //   count: u32,            // number of indexed symbols
    //   offsets: [u32, count], // file offsets to the header of the member that contains
    //                          // that symbol.
    //   names: *               // sequence of null terminated symbol names.
    //
    // The Windows table member also has the name "/", and has the following contents, using
    // little-endian numbers, where the symbol_members must be sorted to allow binary search:
    //   member_count: u32,                   // number of members
    //   member_offsets: [u32; member_count], // file offsets to each member header
    //   symbol_count: u32,                   // number of symbols
    //   symbol_member: [u16; symbol_count],  // *1-based* index of the member that contains
    //                                        // each symbol
    //   symbol_names: *                      // sequence of null terminated symbol names
    //
    // Then the long names member ("//") as with regular GNU ar files, just a sequence of
    // null terminated strings indexed by members using the long name format "/n" as described
    // above.
    //
    // Then regular members follow.
    //
    // This library emits only import libraries, that is, libraries with a short import object
    // describing an import from a dll. That means each member contains exactly one import
    // definition, although to be usable from MSVC __declspec(dllimport) it must include an alias
    // with an `__imp_` prefix.
    //
    // The member name doesn't seem to matter, including duplicates, we will use the dll name since
    // that's what's in the files generated by MSVC tools.
    //
    // The short import object has the form:
    //   header:
    //      0: sig1: 0u16
    //      2: sig2: 0xFFFFu16
    //      4: version: u16, // normally 0
    //      6: machine: u16, // IMAGE_MACHINE_* value, e.g. 0x8664 for AMD64
    //      8: time_date_stamp: u32, // normally 0
    //     12: size_of_data: u32, // size following the header
    //     16: ordinal_or_hint: u16, // depending on flag
    //     18: object_type: u2, // IMPORT_OBJECT_{CODE,DATA,CONST} = 0, 1, 2
    //         name_type: u3,   // IMPORT_OBJECT_{ORDINAL,NAME,NAME_NO_PREFIX,NAME_UNDECORATE,NAME_EXPORTAS} = 0, 1, 2, 3, 4
    //         reserved: u11,
    //   20: data:  // size_of_data bytes
    //      name: * // import name; null terminated string
    //      dll_name: * // dll name; null terminated string
    pub fn generate(dll_name: &str, import_names: &[&str]) -> Vec<u8> {
        // member count: one for each import_name in argument order, followed by the import
        // descriptor.
        let member_count = import_names.len() + 1;

        assert!(member_count <= 0xFFFF, "too many import names");

        // foo.dll => foo so we can construct the import descriptor symbol.
        // At least for the Windows system dlls, don't seem to need any further
        // escaping, e.g. "api-ms-win-appmodel-runtime-l1-1-1.dll" =>
        // "__IMPORT_DESCRIPTOR_api-ms-win-appmodel-runtime-l1-1-1"
        // Not using std::path to avoid having to handle non-unicode paths.
        let mut dll_basename = String::from(dll_name);
        if let Some(index) = dll_basename.rfind('.') {
            dll_basename.truncate(index);
        }

        // Identify the target of a symbol
        #[derive(Copy, Clone)]
        enum SymbolValue {
            // a short import object, specifically for import_names[.0]
            Import(usize),
            // the __IMPORT_DESCRIPTOR_{dll_basename} used to build the final .idata section.
            Descriptor,
        }

        // Note we are using the behavior of BTee* that it keeps its keys in sorted order: this
        // is required by the MSVC symbol table so the linker can use binary search.
        let mut symbols =
            std::collections::BTreeMap::<std::borrow::Cow<'_, str>, SymbolValue>::new();

        for (index, &name) in import_names.iter().enumerate() {
            symbols.insert(name.into(), SymbolValue::Import(index));
            symbols.insert(format!("__imp_{name}").into(), SymbolValue::Import(index));
        }

        let import_descriptor_symbol = format!("__IMPORT_DESCRIPTOR_{dll_basename}");
        symbols.insert(import_descriptor_symbol.as_str().into(), SymbolValue::Descriptor);

        let symbol_count = symbols.len();

        let mut writer = Writer::new();

        // member names are all the dll_name with the MSVC tools.
        let member_name = writer.member_name(dll_name);

        // Standard System-V / GNU symbol table member
        let mut gnu_symbols = writer.start_member(MemberName::SymbolTable);
        // member table: one entry per symbol (duplicates allowed for aliasing)
        gnu_symbols.write_u32_be(symbol_count as u32);
        // reserve space for member offsets.
        let gnu_member_table_offset = gnu_symbols.reserve_bytes(symbol_count * 4);
        // symbol string table
        for name in symbols.keys() {
            gnu_symbols.write_c_str(name);
        }
        // done with legacy symbol directory
        drop(gnu_symbols);

        // MSVC tools symbol table member
        let mut ms_symbols = writer.start_member(MemberName::SymbolTable);
        // member offset table
        ms_symbols.write_u32_le(member_count as u32);
        let ms_member_table_offset = ms_symbols.reserve_bytes(member_count * 4);
        // symbol table
        ms_symbols.write_u32_le(symbol_count as u32);
        //   member index we assume symbols are in the same order as the member table.
        for &value in symbols.values() {
            let member_index = match value {
                SymbolValue::Import(index) => index,
                SymbolValue::Descriptor => import_names.len(),
            };
            // Yep, it's a 1-based index. Who knows why.
            // cast to u16 should be safe due to assert!() on member_count above.
            ms_symbols.write_u16_le(1 + member_index as u16);
        }
        // string table again (could just copy from legacy string table above?)
        for name in symbols.keys() {
            ms_symbols.write_c_str(name);
        }
        // done with current symbol directory
        drop(ms_symbols);

        writer.write_long_names();
        // can't use writer.member_name() from here

        // short import object members
        for (index, name) in import_names.iter().enumerate() {
            let mut member = writer.start_member(member_name);
            // update member offsets
            let member_offset = member.header_offset as u32;
            // fixme: also update the GNU alias symbol entry
            member.set_u32_be(gnu_member_table_offset + index * 4, member_offset);

            member.set_u32_le(ms_member_table_offset + index * 4, member_offset);
            // write import object:
            //   signature
            member.write_u16_le(object::pe::IMAGE_FILE_MACHINE_UNKNOWN);
            member.write_u16_le(object::pe::IMPORT_OBJECT_HDR_SIG2);
            //   version
            member.write_u16_le(0);
            //   machine = AMD64
            member.write_u16_le(object::pe::IMAGE_FILE_MACHINE_AMD64);
            //   time_date_stamp
            member.write_u32_le(0);
            //   size_of_data
            member.write_u32_le((name.len() + 1 + dll_name.len() + 1) as u32);
            //   ordinal_or_hint
            member.write_u16_le(0);
            //   object_type | name_type = IMPORT_OBJECT_CODE | IMPORT_OBJECT_NAME
            member.write_u16_le({
                use object::pe::*;
                IMPORT_OBJECT_CODE << IMPORT_OBJECT_TYPE_SHIFT
                    | IMPORT_OBJECT_NAME << IMPORT_OBJECT_NAME_SHIFT
            });
            // data:
            //   name
            member.write_c_str(name);
            //   dll_name
            member.write_c_str(dll_name);

            drop(member);
        }

        // import descriptor member
        let mut import_descriptor = writer.start_member(member_name);
        let member_offset = import_descriptor.header_offset as u32;
        import_descriptor
            .set_u32_be(gnu_member_table_offset + import_names.len() * 4, member_offset);
        import_descriptor
            .set_u32_le(ms_member_table_offset + import_names.len() * 4, member_offset);
        // This is a COFF object containing two sections:
        //   .idata$2: import directory entry:
        //      20 bytes, all 0 on disk, an Import Directory Table entry
        //      filled out by the linker with relocations.
        //   .idata$6: DLL name:
        //       The null terminated file name of the dll
        // The import directory entry has three relocations:
        //    0: [3] import lookup table rva => points to UNDEF symbol .idata$4
        //   12: [2] name rva => points to DLL name section .idata$6
        //   16: [4] import address table rva => points to UNDEF symbol .idata$5
        // The COFF symbol table contains the following symbols:
        //  [0]: external __IMPORT_DESCRIPTOR_{dll_basename} => section 1
        //  [1]: section .idata$2 => section 1
        //  [2]: static .idata$6 => section 2
        //  [3]: section .idata$4 => undef
        //  [4]: section .idata$5 => undef
        // Unfortunately, the object crate doesn't support writing COFF objects.
        // For now, continue to use dumb explicit writer code, but this should be cleaned up.
        //
        // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#coff-file-header-object-and-image
        //
        // COFF file:
        // 0: header:
        //     0: machine: u16,                 // an IMAGE_MACHINE_* value
        //     2: number_of_sections: u16,
        //     4: time_date_stamp: u32,         // 0 in MSVC tools
        //     8: pointer_to_symbol_table: u32, // offset in COFF file to COFF symbol table
        //    12: number_of_symbols: u32,       // number of entries in symbol table
        //    16: size_of_optional_header: u16, // 0 for object file
        //    18: characteristics: u16,         // union of IMAGE_FILE_* flags; 0 for our output
        // 20: sections table:
        //     0: name: [u8; 8],                // null padded section name
        //     8: virtual_size: u32,            // 0 for object files
        //    12: virtual_address: u32,         // 0 for object files
        //    16: size_of_raw_data: u32,        // size of section on disk
        //    20: pointer_to_raw_data: u32,     // section data COFF file offset
        //    24: pointer_to_relocations: u32,  // relocation table COFF file offset
        //    28: pointer_to_line_number: u32,  // 0
        //    32: number_of_relocations: u16,   // number of entries in relocation tables
        //    34: number_of_line_numbers: u16,  // 0
        //    36: characteristics: u32,         // union of IMAGE_SCN_* flags
        // section in sections:
        //   section.pointer_to_raw_data: raw_data: [u8; section.size_of_raw_data]
        //   section.pointer_to_relocations:
        //      0: virtual_address: u32,        // rva of relocation
        //      4: symbol_table_index: u32,     // index into COFF symbol table
        //      8: type: u16,                   // IMAGE_REL_* value
        // pointer_to_symbol_table:
        //   i in 0..number_of_symbols:
        //     0: name: [u8; 8] | { 0u32; offset_in_string_table: u32 };
        //     8: value: u32,
        //    12: section_number: u16,          // IMAGE_SYM_* or 1-based section index
        //    14: base_type: u8,                // IMAGE_SYM_TYPE_*, always NULL
        //    15: complex_type: u8,             // IMAGE_SYM_DTYPE_*, always NULL for our output
        //    16: storage_class: u8,            // IMAGE_SYM_CLASS_*
        //    17: number_of_aux_symbols: u8,    // 0 for our output
        // pointer_to_symbol_table + number_of_symbols * 18: string_table:
        //   sequence of null-terminated strings

        // COFF File header:
        import_descriptor.write_u16_le(object::pe::IMAGE_FILE_MACHINE_AMD64);
        //   number_of_sections
        import_descriptor.write_u16_le(2);
        //   time_date_stamp
        import_descriptor.write_u32_le(0);
        //   pointer_to_symbol_table
        let pointer_to_symbol_table_offset = import_descriptor.reserve_bytes(4);
        //   number_of_symbols
        import_descriptor.write_u32_le(3);
        //   size_of_optional_header:
        import_descriptor.write_u16_le(0);
        //   characteristics:
        import_descriptor.write_u16_le(0);

        // Section table:
        //   [0] .idata$2: import directory entry
        //     name:
        import_descriptor.write_pad_len(".idata$2".as_bytes(), 0u8, 8);
        //     virtual_size:
        import_descriptor.write_u32_le(0);
        //     virtual_address:
        import_descriptor.write_u32_le(0);
        //     size_of_raw_data:
        import_descriptor.write_u32_le(20);
        //     pointer_to_raw_data:
        let import_directory_entry_section_pointer_to_raw_data_offset =
            import_descriptor.reserve_bytes(4);
        //     pointer_to_relocations:
        let import_directory_entry_section_pointer_to_relocations_offset =
            import_descriptor.reserve_bytes(4);
        //     pointer_to_line_number:
        import_descriptor.write_u32_le(0);
        //     number_of_relocations:
        import_descriptor.write_u16_le(3);
        //     number_of_line_numbers:
        import_descriptor.write_u16_le(0);
        //     characteristics:
        import_descriptor.write_u32_le(
            object::pe::IMAGE_SCN_ALIGN_4BYTES
                | object::pe::IMAGE_SCN_CNT_INITIALIZED_DATA
                | object::pe::IMAGE_SCN_MEM_READ
                | object::pe::IMAGE_SCN_MEM_WRITE,
        );

        //   [1] .idata$6: dll name
        //     name:
        import_descriptor.write_pad_len(b".idata$6", 0u8, 8);
        //     virtual_size:
        import_descriptor.write_u32_le(0);
        //     virtual_address:
        import_descriptor.write_u32_le(0);
        //     size_of_raw_data: (padding to alignment shouldn't be needed, but this matches MSVC)
        import_descriptor.write_u32_le((dll_name.len() as u32 + 1).next_multiple_of(2));
        //     pointer_to_raw_data:
        let dll_name_section_pointer_to_raw_data_offset = import_descriptor.reserve_bytes(4);
        //     pointer_to_relocations:
        import_descriptor.write_u32_le(0);
        //     pointer_to_line_number:
        import_descriptor.write_u32_le(0);
        //     number_of_relocations:
        import_descriptor.write_u16_le(0);
        //     number_of_line_numbers:
        import_descriptor.write_u16_le(0);
        //     characteristics:
        import_descriptor.write_u32_le(
            object::pe::IMAGE_SCN_ALIGN_2BYTES
                | object::pe::IMAGE_SCN_CNT_INITIALIZED_DATA
                | object::pe::IMAGE_SCN_MEM_READ
                | object::pe::IMAGE_SCN_MEM_WRITE,
        );

        // [0] section .idata$2 data
        let import_directory_entry_section_pointer_to_raw_data =
            import_descriptor.data.len() - import_descriptor.header_offset;
        import_descriptor.set_u32_le(
            import_directory_entry_section_pointer_to_raw_data_offset,
            import_directory_entry_section_pointer_to_raw_data as u32,
        );
        import_descriptor.reserve_bytes(20);
        // [0] section .idata$2 relocations
        let import_directory_entry_section_pointer_to_relocations =
            import_descriptor.data.len() - import_descriptor.header_offset;
        import_descriptor.set_u32_le(
            import_directory_entry_section_pointer_to_relocations_offset,
            import_directory_entry_section_pointer_to_relocations as u32,
        );
        //   relocation 0: [3] import lookup table rva => points to UNDEF symbol .idata$4
        import_descriptor.write_u32_le(0);
        import_descriptor.write_u32_le(3);
        import_descriptor.write_u16_le(object::pe::IMAGE_REL_AMD64_ADDR32NB);
        //   relocation 1: [2] name rva => points to DLL name section .idata$6
        import_descriptor.write_u32_le(12);
        import_descriptor.write_u32_le(2);
        import_descriptor.write_u16_le(object::pe::IMAGE_REL_AMD64_ADDR32NB);
        //   relocation 2: [4] import address table rva => points to UNDEF symbol .idata$5
        import_descriptor.write_u32_le(16);
        import_descriptor.write_u32_le(4);
        import_descriptor.write_u16_le(object::pe::IMAGE_REL_AMD64_ADDR32NB);

        // [1] section .idata$6 data
        let dll_name_section_pointer_to_raw_data =
            import_descriptor.data.len() - import_descriptor.header_offset;
        import_descriptor.set_u32_le(
            dll_name_section_pointer_to_raw_data_offset,
            dll_name_section_pointer_to_raw_data as u32,
        );
        import_descriptor.write_c_str(dll_name);
        // pad to even offset
        import_descriptor.align(2, 0u8);

        let mut symbol_string_table = vec![];
        // COFF symbol table:
        let pointer_to_symbol_table =
            import_descriptor.data.len() - import_descriptor.header_offset;
        import_descriptor
            .set_u32_le(pointer_to_symbol_table_offset, (pointer_to_symbol_table) as u32);
        //   [0] external __IMPORT_DESCRIPTOR_{dll_basename} => section 1
        //      name:
        // import_descriptor_symbol is definitely longer than 8, so use the long name format.
        symbol_string_table.extend_from_slice(import_descriptor_symbol.as_bytes());
        symbol_string_table.push(0);
        import_descriptor.write_u32_le(0);
        import_descriptor.write_u32_le(0);
        //      value:
        import_descriptor.write_u32_le(0);
        //      section_number:
        import_descriptor.write_u16_le(1);
        //      base_type:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_TYPE_NULL as u8);
        //      complex_type:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_DTYPE_NULL as u8);
        //      storage_class:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_CLASS_EXTERNAL);
        //      number_of_aux_symbols:
        import_descriptor.write_u8(0);
        //   [1] section .idata$2 => section 1
        //      name:
        // ".idata$2" is definitely shorter than 8, so use the short name format.
        import_descriptor.write_pad_len(b".idata$2", 0u8, 8);
        //      value:
        import_descriptor.write_u32_le(0);
        //      section_number:
        import_descriptor.write_u16_le(1);
        //      base_type:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_TYPE_NULL as u8);
        //      complex_type:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_DTYPE_NULL as u8);
        //      storage_class:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_CLASS_SECTION);
        //      number_of_aux_symbols:
        import_descriptor.write_u8(0);
        //   [2] static .idata$6 => section 2
        //      name:
        // ".idata$6" is definitely shorter than 8, so use the short name format.
        import_descriptor.write_pad_len(b".idata$6", 0u8, 8);
        //      value:
        import_descriptor.write_u32_le(0);
        //      section_number:
        import_descriptor.write_u16_le(2);
        //      base_type:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_TYPE_NULL as u8);
        //      complex_type:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_DTYPE_NULL as u8);
        //      storage_class:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_CLASS_STATIC);
        //      number_of_aux_symbols:
        import_descriptor.write_u8(0);
        //   [3] section .idata$4 => undef
        //      name:
        import_descriptor.write_pad_len(b".idata$4", 0u8, 8);
        //      value:
        import_descriptor.write_u32_le(0);
        //      section_number:
        import_descriptor.write_i16_le(object::pe::IMAGE_SYM_UNDEFINED as i16);
        //      base_type:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_TYPE_NULL as u8);
        //      complex_type:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_DTYPE_NULL as u8);
        //      storage_class:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_CLASS_SECTION);
        //      number_of_aux_symbols:
        import_descriptor.write_u8(0);
        //   [4] section .idata$5 => undef
        //      name:
        import_descriptor.write_pad_len(b".idata$5", 0u8, 8);
        //      value:
        import_descriptor.write_u32_le(0);
        //      section_number:
        import_descriptor.write_i16_le(object::pe::IMAGE_SYM_UNDEFINED as i16);
        //      base_type:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_TYPE_NULL as u8);
        //      complex_type:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_DTYPE_NULL as u8);
        //      storage_class:
        import_descriptor.write_u8(object::pe::IMAGE_SYM_CLASS_SECTION);
        //      number_of_aux_symbols:
        import_descriptor.write_u8(0);

        // COFF symbol string table
        import_descriptor.write(&symbol_string_table);

        drop(import_descriptor);

        writer.data.data
    }

    #[derive(Copy, Clone)]
    enum MemberName {
        SymbolTable,     // "/"
        LongNames,       // "//"
        Short([u8; 16]), // "{0}/"
        Long(usize),     // "/{0}"
    }

    struct Data {
        data: Vec<u8>,
    }

    impl Data {
        fn new() -> Self {
            Self { data: vec![] }
        }

        fn len(&self) -> usize {
            self.data.len()
        }

        fn write(&mut self, data: &[u8]) {
            self.data.extend_from_slice(data);
        }

        fn slice(&mut self, offset: usize, len: usize) -> &mut [u8] {
            &mut self.data[offset..offset + len]
        }

        fn write_c_str(&mut self, data: &str) {
            self.data.extend_from_slice(data.as_bytes());
            self.data.push(0);
        }

        fn write_pad_len(&mut self, data: &[u8], pad: u8, len: usize) {
            assert!(data.len() <= len);
            let offset = self.data.len();
            self.data.extend_from_slice(data);
            self.data.resize(offset + len, pad);
        }

        fn write_u8(&mut self, data: u8) {
            self.data.push(data);
        }

        fn write_i16_le(&mut self, data: i16) {
            self.data.extend_from_slice(&data.to_le_bytes());
        }

        fn write_u16_le(&mut self, data: u16) {
            self.data.extend_from_slice(&data.to_le_bytes());
        }

        fn write_u32_be(&mut self, data: u32) {
            self.data.extend_from_slice(&data.to_be_bytes());
        }

        fn write_u32_le(&mut self, data: u32) {
            self.data.extend_from_slice(&data.to_le_bytes());
        }

        fn reserve_bytes(&mut self, count: usize) -> usize {
            let offset = self.data.len();
            self.data.resize(offset + count, 0);
            offset
        }

        fn set_u32_be(&mut self, offset: usize, data: u32) {
            self.data[offset..][..4].copy_from_slice(&data.to_be_bytes());
        }

        fn set_u32_le(&mut self, offset: usize, data: u32) {
            self.data[offset..][..4].copy_from_slice(&data.to_le_bytes());
        }

        fn align(&mut self, alignment: usize, pad: u8) {
            let offset = self.data.len();
            self.data.resize(offset.next_multiple_of(alignment), pad);
        }
    }

    struct Writer {
        data: Data,
        long_names: Option<Vec<u8>>,
    }

    impl Deref for Writer {
        type Target = Data;

        fn deref(&self) -> &Self::Target {
            &self.data
        }
    }

    impl DerefMut for Writer {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.data
        }
    }

    impl Writer {
        fn new() -> Self {
            let long_names = Some(vec![]);
            let mut data = Data::new();
            data.write(b"!<arch>\n");
            Self { data, long_names }
        }

        fn member_name(&mut self, name: &str) -> MemberName {
            let Some(ref mut long_buf) = self.long_names else {
                panic!("already wrote long names member");
            };

            if name.len() < 16 {
                let mut buf = [0u8; 16];
                buf[..name.len()].copy_from_slice(name.as_bytes());
                buf[name.len()] = b'/';
                buf[name.len() + 1..].fill(b' ');
                MemberName::Short(buf)
            } else {
                let name = std::ffi::CString::new(name).expect("names cannot contain \\0");
                let name = name.as_bytes_with_nul();

                // Find the name *including the null terminator* in the existing long names buffer.
                // Note, this could find "bar\0" in "foobar\0", but that seems to be fine according
                // to the spec? It still counts as a null terminated "bar" string.
                let offset = long_buf
                    .windows(name.len())
                    .position(|window| window == name)
                    .unwrap_or_else(|| {
                        // Didn't already have it, so add it to the end.
                        let offset = long_buf.len();
                        long_buf.extend_from_slice(name);
                        offset
                    });
                MemberName::Long(offset)
            }
        }

        fn start_member(&mut self, name: MemberName) -> Member<'_> {
            let header_offset = self.data.len();
            // fill the header with blanks...
            self.data.data.resize(header_offset + Member::HEADER_SIZE - 2, b' ');
            // except for end marker
            self.data.write(b"`\n");

            let mut member = Member::new(&mut self.data, header_offset);
            member.set_name(name);
            // init date, mode to default values as produced by MSVC tools.
            // uid, gid are already defaulted to blank.
            member.set_time_date_stamp(-1);
            member.set_mode(0);
            member
        }

        fn write_long_names(&mut self) {
            let data = self.long_names.take().expect("already wrote long names member");
            let mut member = self.start_member(MemberName::LongNames);
            member.write(&data);
            drop(member);
        }
    }

    struct Member<'a> {
        data: &'a mut Data,
        header_offset: usize,
    }

    impl Deref for Member<'_> {
        type Target = Data;

        fn deref(&self) -> &Self::Target {
            self.data
        }
    }

    impl DerefMut for Member<'_> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            self.data
        }
    }

    impl<'a> Member<'a> {
        const HEADER_SIZE: usize = 60;

        fn new(data: &'a mut Data, header_offset: usize) -> Self {
            Self { data, header_offset }
        }

        fn header_slice(&mut self, offset: usize, len: usize) -> &mut [u8] {
            self.data.slice(self.header_offset + offset, len)
        }

        fn set_name(&mut self, name: MemberName) {
            let mut field = self.header_slice(0, 16);
            match name {
                MemberName::SymbolTable => {
                    field[0..1].copy_from_slice(b"/");
                    field[1..].fill(b' ');
                }
                MemberName::LongNames => {
                    field[0..2].copy_from_slice(b"//");
                    field[2..].fill(b' ');
                }
                MemberName::Short(name) => {
                    field.copy_from_slice(&name);
                    // already includes trailing / and spaces
                }
                MemberName::Long(offset) => {
                    use std::io::Write;
                    field.fill(b' ');
                    write!(field, "/{offset}").expect("writing long name should not fail");
                }
            }
        }

        fn set_time_date_stamp(&mut self, value: i32) {
            self.set_decimal_field(16, 12, value);
        }

        fn set_uid(&mut self, value: i32) {
            self.set_decimal_field(28, 6, value);
        }

        fn set_gid(&mut self, value: i32) {
            self.set_decimal_field(34, 6, value);
        }

        fn set_mode(&mut self, value: i32) {
            use std::io::Write;
            write!(std::io::Cursor::new(self.header_slice(40, 8)), "{value:o}")
                .expect("value too large");
        }

        fn set_decimal_field(&mut self, offset: usize, size: usize, value: i32) {
            use std::io::Write;
            write!(std::io::Cursor::new(self.header_slice(offset, size)), "{value}")
                .expect("value too large");
        }
    }

    impl<'a> Drop for Member<'a> {
        fn drop(&mut self) {
            let data_size = self.data.len() - self.header_offset - Self::HEADER_SIZE;
            assert!(data_size < i32::MAX as usize);
            self.set_decimal_field(48, 10, data_size as i32);
            self.align(2, b'\n');
        }
    }
}
