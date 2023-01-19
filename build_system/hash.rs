#[allow(deprecated)]
use std::hash::SipHasher;
use std::hash::{Hash, Hasher};
use std::path::Path;

#[derive(Copy, Clone, Debug)]
pub(crate) enum IsDirty {
    Clean,
    Dirty,
}

impl IsDirty {
    pub(crate) fn check_file(file: &Path, expected_hash: u64) -> Self {
        let contents = std::fs::read(file).unwrap();

        #[allow(deprecated)]
        let mut hasher = SipHasher::new();
        contents.hash(&mut hasher);
        let actual_hash = hasher.finish();

        if actual_hash == expected_hash { IsDirty::Clean } else { IsDirty::Dirty }
    }
}

pub(crate) struct HashedFile {
    contents: Vec<u8>,
    hash: u64,
}

impl HashedFile {
    pub(crate) fn read_file(file: &Path) -> Self {
        let contents = std::fs::read(file).unwrap();

        #[allow(deprecated)]
        let mut hasher = SipHasher::new();
        contents.hash(&mut hasher);
        let hash = hasher.finish();

        HashedFile { contents, hash }
    }

    pub(crate) fn write_if_changed(&self, target: &Path) -> IsDirty {
        let is_dirty = IsDirty::check_file(target, self.hash);
        if let IsDirty::Dirty = is_dirty {
            std::fs::write(target, &self.contents).unwrap();
        }
        is_dirty
    }
}
