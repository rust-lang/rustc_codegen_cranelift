pub(crate) struct DataWriter {
    data: Vec<u8>,
}

impl DataWriter {
    pub(crate) fn new() -> Self {
        Self { data: vec![] }
    }

    pub(crate) fn len(&self) -> usize {
        self.data.len()
    }

    pub(crate) fn get_pod_mut<T>(&mut self, offset: usize) -> &mut T
    where
        T: object::pod::Pod,
    {
        object::from_bytes_mut(&mut self.data[offset..]).expect("invalid POD offset").0
    }

    pub(crate) fn write(&mut self, data: &[u8]) {
        self.data.extend_from_slice(data);
    }

    pub(crate) fn write_pod<T>(&mut self, value: &T) -> usize
    where
        T: object::pod::Pod,
    {
        let offset = self.data.len();
        self.data.extend_from_slice(object::bytes_of(value));
        offset
    }

    pub(crate) fn write_c_str(&mut self, data: &str) {
        self.data.extend_from_slice(data.as_bytes());
        self.data.push(0);
    }

    pub(crate) fn write_u32_le(&mut self, data: u32) {
        self.data.extend_from_slice(&data.to_le_bytes());
    }

    pub(crate) fn reserve_bytes(&mut self, count: usize) -> usize {
        let offset = self.data.len();
        self.data.resize(offset + count, 0);
        offset
    }

    pub(crate) fn align(&mut self, alignment: usize, pad: u8) {
        let offset = self.data.len();
        self.data.resize(offset.next_multiple_of(alignment), pad);
    }

    pub(crate) fn into_data(self) -> Vec<u8> {
        self.data
    }
}
