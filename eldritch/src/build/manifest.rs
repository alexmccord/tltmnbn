use std::io;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};
use thiserror::Error;
use toml;

use unutterablec::fs::{FileResolver, FileResolverError};

#[derive(Serialize, Deserialize, Debug)]
pub struct Package {
    pub name: String,
}

impl Package {
    pub fn new(name: String) -> Package {
        Package { name }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ProjectManifest {
    pub package: Package,
}

impl ProjectManifest {
    pub fn new(name: String) -> ProjectManifest {
        ProjectManifest {
            package: Package::new(name),
        }
    }

    pub fn parse<R: AsRef<str>>(value: R) -> Result<ProjectManifest, ManifestError> {
        let manifest = Manifest::parse(value)?;
        match manifest {
            Manifest::Project(p) => Ok(p),
            _ => Err(ManifestError::ManifestTypeMismatch {
                expected: ManifestType::Project,
                actual: manifest.as_type(),
            }),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Workspace {
    pub members: Vec<String>,
}

impl Workspace {
    pub fn new(members: Vec<String>) -> Workspace {
        Workspace { members }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct WorkspaceManifest {
    pub workspace: Workspace,
}

impl WorkspaceManifest {
    pub fn new(names: Vec<String>) -> WorkspaceManifest {
        WorkspaceManifest {
            workspace: Workspace::new(names),
        }
    }

    pub fn parse<R: AsRef<str>>(value: R) -> Result<ProjectManifest, ManifestError> {
        let manifest = Manifest::parse(value)?;
        match manifest {
            Manifest::Project(p) => Ok(p),
            _ => Err(ManifestError::ManifestTypeMismatch {
                expected: ManifestType::Workspace,
                actual: manifest.as_type(),
            }),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum Manifest {
    Project(ProjectManifest),
    Workspace(WorkspaceManifest),
}

#[derive(Error, Debug, PartialEq)]
pub enum ManifestError {
    #[error(transparent)]
    Invalid(#[from] toml::de::Error),
    #[error(transparent)]
    FileResolver(#[from] FileResolverError),
    #[error("Expected {expected} but got {actual}")]
    ManifestTypeMismatch {
        expected: ManifestType,
        actual: ManifestType,
    },
    #[error("Build manifest `eldritch.toml` was nowhere to be found in the ancestral path.")]
    ManifestNotFound,
}

impl ManifestError {
    pub fn as_io(&self) -> Option<&io::Error> {
        self.as_file_resolver().and_then(|e| e.as_io())
    }

    pub fn as_file_resolver(&self) -> Option<&FileResolverError> {
        match self {
            ManifestError::Invalid(_) => None,
            ManifestError::FileResolver(e) => Some(e),
            ManifestError::ManifestTypeMismatch { .. } => None,
            ManifestError::ManifestNotFound => None,
        }
    }
}

#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum ManifestType {
    #[error("project")]
    Project,
    #[error("workspace")]
    Workspace,
}

impl Manifest {
    pub fn parse<R: AsRef<str>>(value: R) -> Result<Manifest, ManifestError> {
        Ok(toml::from_str(value.as_ref())?)
    }

    pub fn as_project(&self) -> Option<&ProjectManifest> {
        match self {
            Manifest::Project(p) => Some(p),
            _ => None,
        }
    }

    pub fn as_workspace(&self) -> Option<&WorkspaceManifest> {
        match self {
            Manifest::Workspace(w) => Some(w),
            _ => None,
        }
    }

    pub fn as_type(&self) -> ManifestType {
        match self {
            Manifest::Project(..) => ManifestType::Project,
            Manifest::Workspace(..) => ManifestType::Workspace,
        }
    }

    pub fn locate_and_open_file<R: FileResolver>(
        resolver: &R,
        search: &Path,
    ) -> Result<(PathBuf, R::File), ManifestError> {
        for dir in search.ancestors() {
            match resolver.open(&dir.join("eldritch.toml")) {
                Ok(file) => return Ok((dir.to_owned(), file)),
                Err(FileResolverError::Io(e)) if e.kind() == io::ErrorKind::NotFound => continue,
                Err(e) => return Err(e.into()),
            }
        }

        Err(ManifestError::ManifestNotFound)
    }
}

impl TryFrom<String> for Manifest {
    type Error = ManifestError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Manifest::parse(value)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use serde::Deserialize;

    use super::*;
    use unutterablec::fs::VirtualFileSystem;

    #[test]
    fn deser_into_project_manifest() {
        let toml = toml::toml! {
            [package]
            name = "something"
        };

        let result = Manifest::deserialize(toml);
        let project = result.as_ref().map(|m| m.as_project()).unwrap();
        assert!(project.is_some_and(|p| p.package.name == "something"));
    }

    #[test]
    fn deser_into_workspace_manifest() {
        let toml = toml::toml! {
            [workspace]
            members = ["something"]
        };

        let result = Manifest::deserialize(toml);
        let workspace = result.as_ref().map(|m| m.as_workspace()).unwrap();
        assert!(workspace.is_some_and(|w| w.workspace.members.len() == 1));
        assert!(workspace.is_some_and(|w| w.workspace.members[0] == "something"));
    }

    #[test]
    fn find_manifest() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/project/eldritch.toml"), "manifest file!")
            .unwrap();
        vfs.write(PathBuf::from("/project/src/foo/bar/baz.l"), "")
            .unwrap();

        let (location, file) =
            Manifest::locate_and_open_file(&vfs, &PathBuf::from("/project/src/foo/bar")).unwrap();

        assert_eq!(location, PathBuf::from("/project"));
        assert_eq!(vfs.read_file(file).unwrap(), "manifest file!");
    }

    #[test]
    fn cannot_find_manifest() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/project/src/foo/bar/baz.l"), "")
            .unwrap();

        let err = Manifest::locate_and_open_file(&vfs, &PathBuf::from("/project/src/foo/bar"))
            .unwrap_err();

        assert_eq!(err, ManifestError::ManifestNotFound);
    }
}
