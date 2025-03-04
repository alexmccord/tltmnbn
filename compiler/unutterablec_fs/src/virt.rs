use std::ffi::OsString;
use std::io::{Cursor, Error, ErrorKind, Read};
use std::path::{Path, PathBuf};

use unutterablec_trie::TrieMap;

use crate::{FileResolver, FileResolverError, Result};

#[derive(Debug, Clone)]
pub struct VirtualFile(Cursor<String>);

impl Read for VirtualFile {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }
}

#[derive(Debug)]
pub struct VirtualFileSystem {
    fs: TrieMap<PathBuf, OsString, String>,
}

impl VirtualFileSystem {
    pub fn new() -> VirtualFileSystem {
        VirtualFileSystem { fs: TrieMap::new() }
    }

    pub fn write(&mut self, path: PathBuf, content: impl Into<String>) -> std::io::Result<()> {
        if self.fs.contains_prefix_key(&path) {
            return Err(Error::from(ErrorKind::IsADirectory));
        }

        self.fs.insert(path, content.into());

        Ok(())
    }
}

impl FileResolver for VirtualFileSystem {
    type File = VirtualFile;

    fn open(&self, path: &Path) -> Result<Self::File> {
        match self.fs.get(path) {
            Some(vf) => Ok(VirtualFile(Cursor::new(vf.clone()))),
            None if self.fs.contains_prefix_key(path) => {
                Err(FileResolverError::Io(Error::from(ErrorKind::IsADirectory)))
            }
            None => Err(FileResolverError::Io(Error::from(ErrorKind::NotFound))),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{io::ErrorKind, path::PathBuf};

    use super::*;

    #[test]
    fn try_reading_a_file() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/foo/bar.txt"), "hello!").unwrap();

        let res = vfs.read(&PathBuf::from("/foo/bar.txt")).unwrap();
        assert!(res == "hello!");
    }

    #[test]
    fn try_reading_a_directory() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/foo/bar.txt"), "hello!").unwrap();

        let res = vfs.read(&PathBuf::from("/foo")).unwrap_err();
        assert!(res
            .as_io()
            .is_some_and(|e| e.kind() == ErrorKind::IsADirectory));
    }
}
