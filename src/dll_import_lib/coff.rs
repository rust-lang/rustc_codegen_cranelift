// Unfortunately, the object crate doesn't support writing COFF objects.
// This should probably be moved upstream at some point.
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
//   0: string_table_size: u32,         // including this size
//   4: sequence of null-terminated strings

use object::pe::*;
use object::{LittleEndian as LE, U16Bytes, U32Bytes, U16, U32};
use std::ops::{Deref, DerefMut};

use super::data::DataWriter;
use super::string_table::StringTable;

pub(crate) const NULL_IMPORT_DESCRIPTOR_SYMBOL: &str = "__NULL_IMPORT_DESCRIPTOR";

fn u16_aligned(value: u16) -> U16<LE> {
    U16::new(LE, value)
}

fn u32_aligned(value: u32) -> U32<LE> {
    U32::new(LE, value)
}

fn u16_unaligned(value: u16) -> U16Bytes<LE> {
    U16Bytes::new(LE, value)
}

fn u32_unaligned(value: u32) -> U32Bytes<LE> {
    U32Bytes::new(LE, value)
}

pub(crate) fn write_short_import(
    data: &mut DataWriter,
    dll_name: &str,
    name: &&str,
    ordinal_or_hint: Option<u16>,
) {
    data.write_pod(&ImportObjectHeader {
        sig1: u16_aligned(IMAGE_FILE_MACHINE_UNKNOWN),
        sig2: u16_aligned(IMPORT_OBJECT_HDR_SIG2),
        version: u16_aligned(0),
        machine: u16_aligned(IMAGE_FILE_MACHINE_AMD64),
        time_date_stamp: u32_aligned(0),
        size_of_data: u32_aligned((name.len() + 1 + dll_name.len() + 1) as u32),
        ordinal_or_hint: u16_aligned(ordinal_or_hint.unwrap_or_default()),
        name_type: u16_aligned(
            IMPORT_OBJECT_CODE << IMPORT_OBJECT_TYPE_SHIFT
                | IMPORT_OBJECT_NAME << IMPORT_OBJECT_NAME_SHIFT,
        ),
    });
    data.write_c_str(name);
    data.write_c_str(dll_name);
}

pub(crate) fn write_import_descriptor(
    data: &mut DataWriter,
    dll_name: &str,
    import_descriptor_symbol: &str,
    null_thunk_data_symbol: &str,
) {
    // This is a COFF object containing 2 sections:
    //   .idata$2: import directory entry:
    //      20 bytes, all 0 on disk, an Import Directory Table entry
    //      filled out by the linker with relocations.
    //   .idata$6: DLL name:
    //       The null terminated file name of the dll
    // The import directory entry has 3 relocations:
    //    0: [3] import lookup table rva => points to UNDEF symbol .idata$4
    //   12: [2] name rva => points to DLL name section .idata$6
    //   16: [4] import address table rva => points to UNDEF symbol .idata$5
    // The COFF symbol table contains 5 symbols:
    //  [0]: external __IMPORT_DESCRIPTOR_{dll_basename} => section 1
    //  [1]: section .idata$2 => section 1
    //  [2]: static .idata$6 => section 2
    //  [3]: section .idata$4 => undef
    //  [4]: section .idata$5 => undef
    //  [5]: external __NULL_IMPORT_DESCRIPTOR => undef
    //  [6]: external __NULL_THUNK_DATA => undef

    // COFF File header:
    let mut file = CoffFileWriter::new(data, IMAGE_FILE_MACHINE_AMD64);

    // Section table:
    //   [0] .idata$2: import directory entry
    let import_directory_header = file.write_section_header(
        ".idata$2",
        IMAGE_SCN_ALIGN_4BYTES
            | IMAGE_SCN_CNT_INITIALIZED_DATA
            | IMAGE_SCN_MEM_READ
            | IMAGE_SCN_MEM_WRITE,
    );
    //   [1] .idata$6: dll name
    let dll_name_header = file.write_section_header(
        ".idata$6",
        IMAGE_SCN_ALIGN_2BYTES
            | IMAGE_SCN_CNT_INITIALIZED_DATA
            | IMAGE_SCN_MEM_READ
            | IMAGE_SCN_MEM_WRITE,
    );

    // [0] section .idata$2 data
    CoffSectionRawData::new(&mut file, import_directory_header).reserve_bytes(20);

    // [0] section .idata$2 relocations
    let import_descriptor_pointer_to_relocations = file.data.len() - file.offset;

    let header = import_directory_header.get_mut(file.data);
    header.number_of_relocations = u16_aligned(3);

    header.pointer_to_relocations = u32_aligned(import_descriptor_pointer_to_relocations as u32);

    // todo: CoffRelocWriter

    //   relocation 0: [3] import lookup table rva => points to UNDEF symbol .idata$4
    file.data.write_pod(&ImageRelocation {
        virtual_address: u32_unaligned(0),
        symbol_table_index: u32_unaligned(3),
        typ: u16_unaligned(IMAGE_REL_AMD64_ADDR32NB),
    });
    //   relocation 1: [2] name rva => points to DLL name section .idata$6
    file.data.write_pod(&ImageRelocation {
        virtual_address: u32_unaligned(12),
        symbol_table_index: u32_unaligned(2),
        typ: u16_unaligned(IMAGE_REL_AMD64_ADDR32NB),
    });
    //   relocation 2: [4] import address table rva => points to UNDEF symbol .idata$5
    file.data.write_pod(&ImageRelocation {
        virtual_address: u32_unaligned(16),
        symbol_table_index: u32_unaligned(4),
        typ: u16_unaligned(IMAGE_REL_AMD64_ADDR32NB),
    });

    // [1] section .idata$6 data
    CoffSectionRawData::new(&mut file, dll_name_header).write_c_str(dll_name);

    // COFF symbol table:
    let mut symbol_table = file.start_symbol_table();
    symbol_table.add(
        import_descriptor_symbol,
        SymbolOptions {
            section_number: 1,
            storage_class: IMAGE_SYM_CLASS_EXTERNAL,
            ..Default::default()
        },
    );
    symbol_table.add(
        ".idata$2",
        SymbolOptions {
            section_number: 1,
            storage_class: IMAGE_SYM_CLASS_SECTION,
            ..Default::default()
        },
    );
    symbol_table.add(
        ".idata$6",
        SymbolOptions {
            section_number: 2,
            storage_class: IMAGE_SYM_CLASS_STATIC,
            ..Default::default()
        },
    );
    symbol_table.add(
        ".idata$4",
        SymbolOptions { storage_class: IMAGE_SYM_CLASS_SECTION, ..Default::default() },
    );
    symbol_table.add(
        ".idata$5",
        SymbolOptions { storage_class: IMAGE_SYM_CLASS_SECTION, ..Default::default() },
    );
    symbol_table.add(
        NULL_IMPORT_DESCRIPTOR_SYMBOL,
        SymbolOptions { storage_class: IMAGE_SYM_CLASS_EXTERNAL, ..Default::default() },
    );
    symbol_table.add(
        null_thunk_data_symbol,
        SymbolOptions { storage_class: IMAGE_SYM_CLASS_EXTERNAL, ..Default::default() },
    );
}

pub(crate) fn write_null_thunk_data(data: &mut DataWriter, symbol: &str) {
    // This is a COFF file with a two sections with 8 bytes of null data
    let mut file = CoffFileWriter::new(data, IMAGE_FILE_MACHINE_AMD64);

    let import_address_section = file.write_section_header(
        ".idata$5",
        IMAGE_SCN_ALIGN_8BYTES
            | IMAGE_SCN_CNT_INITIALIZED_DATA
            | IMAGE_SCN_MEM_READ
            | IMAGE_SCN_MEM_WRITE,
    );
    let import_lookup_section = file.write_section_header(
        ".idata$4",
        IMAGE_SCN_ALIGN_8BYTES
            | IMAGE_SCN_CNT_INITIALIZED_DATA
            | IMAGE_SCN_MEM_READ
            | IMAGE_SCN_MEM_WRITE,
    );

    CoffSectionRawData::new(&mut file, import_address_section).reserve_bytes(8);

    CoffSectionRawData::new(&mut file, import_lookup_section).reserve_bytes(8);

    file.start_symbol_table().add(
        symbol,
        SymbolOptions {
            section_number: 1,
            storage_class: IMAGE_SYM_CLASS_EXTERNAL,
            ..Default::default()
        },
    );
}

pub(crate) fn write_null_import_descriptor(data: &mut DataWriter) {
    // This is a COFF file with a section with 20 bytes of null data
    let mut file = CoffFileWriter::new(data, IMAGE_FILE_MACHINE_AMD64);
    let header = file.write_section_header(
        ".idata$3",
        IMAGE_SCN_ALIGN_4BYTES
            | IMAGE_SCN_CNT_INITIALIZED_DATA
            | IMAGE_SCN_MEM_READ
            | IMAGE_SCN_MEM_WRITE,
    );
    CoffSectionRawData::new(&mut file, header).reserve_bytes(20);
    file.start_symbol_table().add(
        NULL_IMPORT_DESCRIPTOR_SYMBOL,
        SymbolOptions {
            section_number: 1,
            storage_class: IMAGE_SYM_CLASS_EXTERNAL,
            ..Default::default()
        },
    );
}

struct CoffFileWriter<'data> {
    data: &'data mut DataWriter,
    offset: usize,
    number_of_sections: u16,
    string_table: CoffStringTable,
}

impl<'data> CoffFileWriter<'data> {
    fn new(data: &'data mut DataWriter, machine: u16) -> Self {
        let file_offset = data.len();
        data.write_pod(&ImageFileHeader {
            machine: u16_aligned(machine),
            number_of_sections: u16_aligned(0),
            time_date_stamp: u32_aligned(0),
            pointer_to_symbol_table: u32_aligned(0),
            number_of_symbols: u32_aligned(0),
            size_of_optional_header: u16_aligned(0),
            characteristics: u16_aligned(0),
        });
        let string_table = CoffStringTable::new();
        Self { data, offset: file_offset, number_of_sections: 0, string_table }
    }

    fn file_header_mut(&mut self) -> &mut ImageFileHeader {
        self.data.get_pod_mut(self.offset)
    }

    fn write_section_header(&mut self, name: &str, characteristics: u32) -> CoffSectionHeader {
        self.number_of_sections += 1;
        let offset = self.data.write_pod(&ImageSectionHeader {
            name: self.string_table.get_raw_name(name),
            virtual_size: u32_aligned(0),
            virtual_address: u32_aligned(0),
            size_of_raw_data: u32_aligned(0),    // filled out later
            pointer_to_raw_data: u32_aligned(0), // ditto.
            pointer_to_relocations: u32_aligned(0), // (possibly) ditto.
            pointer_to_linenumbers: u32_aligned(0),
            number_of_relocations: u16_aligned(0),
            number_of_linenumbers: u16_aligned(0),
            characteristics: u32_aligned(characteristics),
        });
        CoffSectionHeader { offset }
    }

    fn start_symbol_table(&mut self) -> CoffSymbolTableWriter<'_, 'data> {
        let offset = self.len();
        self.file_header_mut().pointer_to_symbol_table = u32_aligned((offset - self.offset) as u32);
        CoffSymbolTableWriter { file: self, offset, number_of_symbols: 0 }
    }
}

impl Deref for CoffFileWriter<'_> {
    type Target = DataWriter;

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

impl DerefMut for CoffFileWriter<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data
    }
}

impl Drop for CoffFileWriter<'_> {
    fn drop(&mut self) {
        let number_of_sections = self.number_of_sections;
        let header = self.file_header_mut();
        header.number_of_sections = u16_aligned(number_of_sections);
        self.string_table.write(self.data);
    }
}

struct CoffStringTable(StringTable);

impl CoffStringTable {
    fn new() -> Self {
        Self(StringTable::new())
    }

    fn write(&self, writer: &mut DataWriter) {
        let data = self.0.data();
        writer.write_u32_le(data.len() as u32 + 4);
        writer.write(data);
    }

    pub fn get_raw_name(&mut self, value: &str) -> [u8; 8] {
        let mut result = [0u8; 8];
        if value.len() > 8 {
            // add 4 for the string table length
            let offset = 4 + self.0.find_or_insert(value);
            result[4..].copy_from_slice(&u32::to_le_bytes(offset as u32));
        } else {
            result.copy_from_slice(value.as_bytes())
        }
        result
    }
}

#[derive(Copy, Clone)]
struct CoffSectionHeader {
    offset: usize,
}

impl CoffSectionHeader {
    fn get_mut(self, data: &mut DataWriter) -> &mut ImageSectionHeader {
        data.get_pod_mut(self.offset)
    }
}

struct CoffSectionRawData<'a, 'data> {
    file: &'a mut CoffFileWriter<'data>,
    header: CoffSectionHeader,
    offset: usize,
}

impl<'a, 'data> CoffSectionRawData<'a, 'data> {
    fn new(file: &'a mut CoffFileWriter<'data>, header: CoffSectionHeader) -> Self {
        let offset = file.data.len();
        Self { file, header, offset }
    }
}

impl Deref for CoffSectionRawData<'_, '_> {
    type Target = DataWriter;

    fn deref(&self) -> &Self::Target {
        self.file.data
    }
}

impl DerefMut for CoffSectionRawData<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.file.data
    }
}

impl Drop for CoffSectionRawData<'_, '_> {
    fn drop(&mut self) {
        // Included in size_of_raw_data - not sure if this is correct?
        // Seems to be what MSVC does for the dll_name section.
        self.file.data.align(2, 0u8);
        let end_offset = self.file.len();
        let header = self.header.get_mut(self.file.data);
        let size_of_raw_data = end_offset - self.offset;
        let pointer_to_raw_data = self.offset - self.file.offset;
        header.size_of_raw_data = u32_aligned(size_of_raw_data as u32);
        header.pointer_to_raw_data = u32_aligned(pointer_to_raw_data as u32);
    }
}

#[derive(Default)]
struct SymbolOptions {
    value: u32,
    section_number: i16,
    // IMAGE_SYM_TYPE_*
    base_type: u16,
    // IMAGE_SYM_DTYPE_*
    complex_type: u16,
    storage_class: u8,
    number_of_aux_symbols: u8,
}

struct CoffSymbolTableWriter<'a, 'data> {
    file: &'a mut CoffFileWriter<'data>,
    offset: usize,
    number_of_symbols: u32,
}

impl CoffSymbolTableWriter<'_, '_> {
    fn add(&mut self, name: &str, options: SymbolOptions) {
        let name = self.file.string_table.get_raw_name(name);
        self.file.write_pod(&ImageSymbol {
            name,
            value: u32_unaligned(options.value),
            section_number: u16_unaligned(options.section_number as u16),
            typ: u16_unaligned(options.base_type | options.complex_type << 8),
            storage_class: options.storage_class,
            number_of_aux_symbols: options.number_of_aux_symbols,
        });
        self.number_of_symbols += 1;
    }
}

impl Drop for CoffSymbolTableWriter<'_, '_> {
    fn drop(&mut self) {
        let pointer_to_symbol_table = self.offset - self.file.offset;
        let header = self.file.file_header_mut();
        header.pointer_to_symbol_table = u32_aligned(pointer_to_symbol_table as u32);
        header.number_of_symbols = u32_aligned(self.number_of_symbols);
    }
}
