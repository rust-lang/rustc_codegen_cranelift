//! https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#archive-library-file-format
//!
//! Windows .lib files are System-V (aka. GNU) flavored ar files with an additional MSVC-specific
//! symbol lookup member after the standard one.
//!
//! An ar archive is the 8 bytes `b"!<arch>\n"` followed by a sequence of 60 byte member headers:
//!
//! ```plaintext
//!   0: name: [u8; 16], // member name, terminated with "/". If it is longer than 15, then
//!                      // use "/n" where "n" is a decimal for the offset in bytes into
//!                      // the longnames ("//") member contents.
//!  16: date: [u8; 12], // ASCII decimal seconds since UNIX epoch - always -1 for MSVC
//!  28: uid: [u8; 6],   // ASCII decimal user id. Always blank for MSVC
//!  34: gid: [u8; 6],   // ditto for group id.
//!  40: mode: [u8; 8],  // ASCII octal UNIX mode. 0 for MSVC
//!  48: size: [u8; 10], // ASCII decimal data size.
//!  58: end: b"`\n",
//! ```
//!
//! then `size` bytes of member payload data. If payload is odd sized, it must be padded to an even
//! offset with `\n`.
//!
//! Standard archives have an initial member with the raw name `/` containing a table with the
//! offsets of the members containing exported symbols, with big-endian encoding:
//!
//! ```plaintext
//!   count: u32_be,            // number of indexed symbols
//!   offsets: [u32_be, count], // file offsets to the header of the member that contains
//!                             // that symbol.
//!   names: *                  // sequence of null terminated symbol names.
//! ```
//!
//! MSVC lib archives then have an additional table member that also has the name `/`, and stores
//! the same information. This uses little-endian encoding, separates the member offset table from
//! the symbol table, and requires symbols to be sorted to allow binary search lookups.
//!
//! ```plaintext
//!   member_count: u32,                   // number of members
//!   member_offsets: [u32; member_count], // file offsets to each member header
//!   symbol_count: u32,                   // number of symbols
//!   symbol_member: [u16; symbol_count],  // *1-based* index of the member that contains
//!                                        // each symbol
//!   symbol_names: *                      // sequence of null terminated symbol names in the same
//!                                        // order as symbol_member.
//! ```
//!
//! Then the standard long names member (`//`), which stores just a sequence of null terminated
//! strings indexed by members using the long name format `/n` as described above. This is not
//! required for MSVC if there are no long names.
//!
//! Then content members follow.
//!
//! The member name doesn't seem to matter, including duplicates, for import libraries MSVC uses
//! the dll name for every member.
//!
//! The short import object has the form:
//!
//! ```plaintext
//!   0: header:
//!      0: sig1: 0u16
//!      2: sig2: 0xFFFFu16
//!      4: version: u16, // normally 0
//!      6: machine: u16, // IMAGE_MACHINE_* value, e.g. 0x8664 for AMD64
//!      8: time_date_stamp: u32, // normally 0
//!     12: size_of_data: u32, // size following the header
//!     16: ordinal_or_hint: u16, // depending on flag
//!     18: object_type: u2, // IMPORT_OBJECT_{CODE,DATA,CONST} = 0, 1, 2
//!         name_type: u3,   // IMPORT_OBJECT_{ORDINAL,NAME,NAME_NO_PREFIX,NAME_UNDECORATE,NAME_EXPORTAS} = 0, 1, 2, 3, 4
//!         reserved: u11,
//!   20: data:  // size_of_data bytes
//!      name: * // import name; null terminated string
//!      dll_name: * // dll name; null terminated string
//! ```

use std::io::Write;
use std::ops::{Deref, DerefMut};

use super::string_table::StringTable;
use super::DataWriter;

#[derive(Copy, Clone)]
pub(crate) struct MemberName(pub [u8; 16]);

impl MemberName {
    pub(crate) const SYMBOL_TABLE: Self = MemberName(*b"/               ");
    pub(crate) const LONG_NAMES: Self = MemberName(*b"//              ");
}

pub(crate) struct Writer {
    data: DataWriter,
    long_names: Option<StringTable>,
}

impl Writer {
    #[allow(clippy::new_without_default)] // Default should probably not write a signature?
    pub(crate) fn new() -> Self {
        let long_names = Some(StringTable::new());
        let mut data = DataWriter::new();
        data.write(b"!<arch>\n");
        Self { data, long_names }
    }

    pub(crate) fn member_name(&mut self, name: &str) -> MemberName {
        let Some(ref mut long_buf) = self.long_names else {
            panic!("already wrote long names member");
        };

        if name.len() < 16 {
            let mut buf = [b' '; 16];
            buf[..name.len()].copy_from_slice(name.as_bytes());
            buf[name.len()] = b'/';
            MemberName(buf)
        } else {
            let offset = long_buf.find_or_insert(name);
            let mut buf = [b' '; 16];
            write!(&mut buf[..], "/{offset}").expect("writing long name should not fail");
            MemberName(buf)
        }
    }

    pub(crate) fn start_member(&mut self, name: MemberName) -> Member<'_> {
        Member::new(&mut self.data, name)
    }

    pub(crate) fn write_long_names(&mut self) {
        let string_table = self.long_names.take().expect("already wrote long names member");
        let mut member = self.start_member(MemberName::LONG_NAMES);
        member.write(string_table.data());
        drop(member);
    }

    pub(crate) fn into_data(self) -> Vec<u8> {
        self.data.into_data()
    }
}

impl Deref for Writer {
    type Target = DataWriter;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl DerefMut for Writer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

pub(crate) struct Member<'data> {
    pub(crate) data: &'data mut DataWriter,
    pub(crate) header_offset: usize,
}

impl<'data> Member<'data> {
    const HEADER_SIZE: usize = std::mem::size_of::<object::archive::Header>();

    fn new(data: &'data mut DataWriter, name: MemberName) -> Self {
        // fill the header MSVC defaults.
        let header_offset = data.write_pod(&object::archive::Header {
            name: name.0,
            date: *b"-1          ",
            uid: [b' '; 6],
            gid: [b' '; 6],
            mode: *b"0       ",
            size: [b' '; 10], // filled out in Drop
            terminator: object::archive::TERMINATOR,
        });

        Self { data, header_offset }
    }

    pub(crate) fn header_mut(&mut self) -> &mut object::archive::Header {
        self.data.get_pod_mut(self.header_offset)
    }

    pub(crate) fn set_name(&mut self, name: MemberName) {
        self.header_mut().name = name.0;
    }

    pub(crate) fn set_time_date_stamp(&mut self, value: i32) -> std::io::Result<()> {
        let header = self.header_mut();
        write!(&mut header.date[..], "{value:<12}")
    }

    pub(crate) fn set_uid(&mut self, value: Option<u32>) -> std::io::Result<()> {
        let header = self.header_mut();
        if let Some(value) = value {
            write!(&mut header.uid[..], "{value:<6}")
        } else {
            header.uid.fill(b' ');
            Ok(())
        }
    }

    pub(crate) fn set_gid(&mut self, value: Option<u32>) -> std::io::Result<()> {
        let header = self.header_mut();
        if let Some(value) = value {
            write!(&mut header.gid[..], "{value:<6}")
        } else {
            header.gid.fill(b' ');
            Ok(())
        }
    }

    pub(crate) fn set_mode(&mut self, value: u16) -> std::io::Result<()> {
        write!(&mut self.header_mut().mode[..], "{value:o<8}")
    }
}

impl Deref for Member<'_> {
    type Target = DataWriter;

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

impl DerefMut for Member<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data
    }
}

impl<'a> Drop for Member<'a> {
    fn drop(&mut self) {
        let data_start = self.header_offset + Self::HEADER_SIZE;
        let data_size = self.data.len() - data_start;
        write!(&mut self.header_mut().size[..], "{data_size}")
            .expect("data size should always fit in 10 bytes");
        self.data.align(2, b'\n');
    }
}
