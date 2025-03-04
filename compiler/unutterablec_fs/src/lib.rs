use std::{io::Read, path::Path};

use thiserror::Error;

mod io;
pub use io::FileSystemIO;

mod virt;
pub use virt::{VirtualFile, VirtualFileSystem};

#[derive(Error, Debug)]
pub enum FileResolverError {
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

impl FileResolverError {
    pub fn as_io(&self) -> Option<&std::io::Error> {
        match self {
            FileResolverError::Io(e) => Some(e),
        }
    }
}

// Just to enable #[derive(PartialEq)]
impl PartialEq for FileResolverError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Io(_), Self::Io(_)) => false,
        }
    }
}

pub type Result<T> = std::result::Result<T, FileResolverError>;

pub trait FileResolver {
    type File: Read;

    fn open(&self, path: &Path) -> Result<Self::File>;

    fn read(&self, path: &Path) -> Result<String> {
        self.read_file(self.open(path)?)
    }

    fn read_file(&self, file: Self::File) -> Result<String> {
        Ok(std::io::read_to_string(file)?)
    }
}
