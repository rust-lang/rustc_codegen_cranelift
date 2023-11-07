// todo: pull out to a proper location. Really should be in `object` crate!
// todo: support ordinals
// todo: support name types (e.g. verbatim+)
// todo: support windows-gnu flavor?
// todo: provide machine
// todo: remove any panics, nice errors

use data::DataWriter;

mod ar;
mod data;
mod string_table;

mod coff;

pub(crate) fn generate(dll_name: &str, import_names: &[&str]) -> Vec<u8> {
    // member count: one for each import_name in argument order, followed by the import
    // descriptor.
    let member_count = 3 + import_names.len();

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
    #[derive(Copy, Clone, Eq, PartialEq)]
    enum SymbolValue {
        // the __IMPORT_DESCRIPTOR_{dll_basename} used to build the final .idata section.
        Descriptor,
        // __NULL_IMPORT_DESCRIPTOR
        NullDescriptor,
        // \x7f{dll_basename}_NULL_THUNK_DATA
        ThunkData,
        // a short import object, specifically for import_names[.0]
        Import(usize),
    }
    impl SymbolValue {
        /// Location in member tables, not *necessarily* the order of the member in the archive.
        fn member_index(self) -> usize {
            match self {
                Self::Descriptor => 0,
                Self::NullDescriptor => 1,
                Self::ThunkData => 2,
                Self::Import(index) => 3 + index,
            }
        }
    }

    // Note we are using the behavior of BTee* that it keeps its keys in sorted order: this
    // is required by the MSVC symbol table so the linker can use binary search.
    let mut symbols = std::collections::BTreeMap::<std::borrow::Cow<'_, str>, SymbolValue>::new();

    let import_descriptor_symbol = format!("__IMPORT_DESCRIPTOR_{dll_basename}");
    symbols.insert(import_descriptor_symbol.as_str().into(), SymbolValue::Descriptor);
    symbols.insert(coff::NULL_IMPORT_DESCRIPTOR_SYMBOL.into(), SymbolValue::NullDescriptor);
    let null_thunk_data_symbol = format!("\x7f{dll_basename}_NULL_THUNK_DATA");
    symbols.insert(null_thunk_data_symbol.as_str().into(), SymbolValue::ThunkData);

    for (index, &name) in import_names.iter().enumerate() {
        symbols.insert(name.into(), SymbolValue::Import(index));
        symbols.insert(format!("__imp_{name}").into(), SymbolValue::Import(index));
    }

    let symbol_count = symbols.len();

    let mut writer = ar::Writer::new();

    // member names are all the dll_name with the MSVC tools.
    let member_name = writer.member_name(dll_name);

    // Standard System-V / GNU symbol table member
    let mut gnu_symbols = writer.start_member(ar::MemberName::SYMBOL_TABLE);
    // member table: one entry per symbol (duplicates allowed for aliasing)
    gnu_symbols.write_u32_be(symbol_count as u32);
    // reserve space for member offsets.
    let gnu_member_table_offset = gnu_symbols.reserve_bytes(symbol_count * 4);
    // symbol string table
    for name in symbols.keys() {
        gnu_symbols.write_c_str(name);
    }
    // done with GNU symbol directory
    drop(gnu_symbols);

    // MSVC tools symbol table member
    let mut ms_symbols = writer.start_member(ar::MemberName::SYMBOL_TABLE);
    // member offset table
    ms_symbols.write_u32_le(member_count as u32);
    let ms_member_table_offset = ms_symbols.reserve_bytes(member_count * 4);
    // symbol table
    ms_symbols.write_u32_le(symbol_count as u32);
    //   member index we assume symbols are in the same order as the member table.
    for &value in symbols.values() {
        let member_index = value.member_index();
        // Yep, it's a 1-based index. Who knows why.
        // cast to u16 should be safe due to assert!() on member_count above.
        ms_symbols.write_u16_le(1 + member_index as u16);
    }
    // string table again (could just copy from legacy string table above?)
    for name in symbols.keys() {
        ms_symbols.write_c_str(name);
    }
    // done with MSVC symbol directory
    drop(ms_symbols);

    writer.write_long_names();
    // can't use writer.member_name() from here

    {
        // import descriptor member
        let mut member = writer.start_member(member_name);

        let symbol_value = SymbolValue::Descriptor;
        // update member offsets
        let member_offset = member.header_offset as u32;
        // Updating GNU symbol table is a bit messy with the aliases
        for (index, value) in symbols.values().enumerate() {
            if symbol_value == *value {
                member.set_u32_be(gnu_member_table_offset + index * 4, member_offset);
            }
        }
        member.set_u32_le(ms_member_table_offset + symbol_value.member_index() * 4, member_offset);

        coff::write_import_descriptor(
            &mut member,
            dll_name,
            &import_descriptor_symbol,
            &null_thunk_data_symbol,
        );
    }

    {
        // null thunk data member
        let mut member = writer.start_member(member_name);

        let symbol_value = SymbolValue::ThunkData;
        // update member offsets
        let member_offset = member.header_offset as u32;
        // Updating GNU symbol table is a bit messy with the aliases
        for (index, value) in symbols.values().enumerate() {
            if symbol_value == *value {
                member.set_u32_be(gnu_member_table_offset + index * 4, member_offset);
            }
        }
        member.set_u32_le(ms_member_table_offset + symbol_value.member_index() * 4, member_offset);

        coff::write_null_thunk_data(&mut member, &null_thunk_data_symbol);
    }

    {
        // null import descriptor member
        let mut member = writer.start_member(member_name);

        let symbol_value = SymbolValue::NullDescriptor;
        // update member offsets
        let member_offset = member.header_offset as u32;
        // Updating GNU symbol table is a bit messy with the aliases
        for (index, value) in symbols.values().enumerate() {
            if symbol_value == *value {
                member.set_u32_be(gnu_member_table_offset + index * 4, member_offset);
            }
        }
        member.set_u32_le(ms_member_table_offset + symbol_value.member_index() * 4, member_offset);

        coff::write_null_import_descriptor(&mut member);
    }

    // short import object members
    for (index, name) in import_names.iter().enumerate() {
        let mut member = writer.start_member(member_name);

        let symbol_value = SymbolValue::Import(index);
        // update member offsets
        let member_offset = member.header_offset as u32;
        // Updating GNU symbol table is a bit messy with the aliases
        for (index, value) in symbols.values().enumerate() {
            if symbol_value == *value {
                member.set_u32_be(gnu_member_table_offset + index * 4, member_offset);
            }
        }
        member.set_u32_le(ms_member_table_offset + symbol_value.member_index() * 4, member_offset);

        coff::write_short_import(&mut member, dll_name, name, None);
    }

    writer.into_data()
}
