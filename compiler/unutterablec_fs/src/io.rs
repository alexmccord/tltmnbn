use std::{fs::File, path::Path};

use crate::{FileResolver, Result};

pub struct FileSystemIO;

impl FileResolver for FileSystemIO {
    type File = File;

    fn open(&self, path: &Path) -> Result<Self::File> {
        Ok(File::open(path)?)
    }
}
