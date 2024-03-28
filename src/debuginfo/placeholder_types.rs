use gimli::write::{AttributeValue, DwarfUnit, UnitEntryId};
use rustc_data_structures::fx::FxHashMap;

use super::DebugContext;

pub(crate) struct PlaceholderTypeDebugContext {
    // Placeholders
    u8_type: UnitEntryId,
    placeholder_types: FxHashMap<u64, UnitEntryId>,
}

impl PlaceholderTypeDebugContext {
    pub(super) fn new(dwarf: &mut DwarfUnit) -> Self {
        let u8_type = dwarf.unit.add(dwarf.unit.root(), gimli::DW_TAG_base_type);
        let u8_type_entry = dwarf.unit.get_mut(u8_type);
        u8_type_entry.set(gimli::DW_AT_name, AttributeValue::StringRef(dwarf.strings.add("u8")));
        u8_type_entry.set(gimli::DW_AT_encoding, AttributeValue::Encoding(gimli::DW_ATE_unsigned));
        u8_type_entry.set(gimli::DW_AT_byte_size, AttributeValue::Udata(1));

        PlaceholderTypeDebugContext { u8_type, placeholder_types: FxHashMap::default() }
    }
}

impl DebugContext {
    pub(crate) fn placeholder_type(&mut self, size: u64) -> UnitEntryId {
        *self.placeholder_types.placeholder_types.entry(size).or_insert_with(|| {
            let array_type_id =
                self.dwarf.unit.add(self.dwarf.unit.root(), gimli::DW_TAG_array_type);
            let array_type_entry = self.dwarf.unit.get_mut(array_type_id);
            array_type_entry
                .set(gimli::DW_AT_type, AttributeValue::UnitRef(self.placeholder_types.u8_type));

            let subrange_id = self.dwarf.unit.add(array_type_id, gimli::DW_TAG_subrange_type);
            let subrange_entry = self.dwarf.unit.get_mut(subrange_id);
            subrange_entry.set(gimli::DW_AT_type, AttributeValue::UnitRef(self.array_size_type));
            subrange_entry.set(gimli::DW_AT_lower_bound, AttributeValue::Udata(0));
            subrange_entry.set(gimli::DW_AT_count, AttributeValue::Udata(size));

            array_type_id
        })
    }
}
