pub(crate) struct StringTable(Vec<u8>);

impl StringTable {
    pub(crate) fn new() -> Self {
        Self(vec![])
    }

    pub(crate) fn data(&self) -> &[u8] {
        self.0.as_slice()
    }

    pub(crate) fn find_or_insert(&mut self, value: &str) -> usize {
        // Find the name *including the null terminator* in the existing buffer.
        // Note, this could find "bar\0" in "foobar\0", but that should be fine?
        // It still counts as a null terminated "bar" string.
        self.0
            .windows(value.len() + 1)
            .position(|window| {
                &window[..value.len()] == value.as_bytes() && window[value.len()] == b'\0'
            })
            .unwrap_or_else(|| {
                let offset = self.0.len();
                self.0.extend_from_slice(value.as_bytes());
                self.0.push(b'\0');
                offset
            })
    }
}
