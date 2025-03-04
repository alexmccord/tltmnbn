use std::path::PathBuf;

use unutterablec::fs::FileResolver;

mod deps;
pub mod manifest;

#[derive(clap::Parser, Debug)]
pub struct Build {
    #[arg(short = 'C')]
    pub dir: Option<PathBuf>,
}

pub fn build<R: FileResolver>(build: Build, resolver: R) {}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use manifest::{ProjectManifest, WorkspaceManifest};
    use unutterablec::fs::VirtualFileSystem;

    fn project(name: &str) -> String {
        toml::to_string(&ProjectManifest::new(name.to_string())).unwrap()
    }

    fn workspace(names: Vec<String>) -> String {
        toml::to_string(&WorkspaceManifest::new(names)).unwrap()
    }

    #[test]
    fn build_project() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/eldritch.toml"), project("foo"))
            .unwrap();
        vfs.write(PathBuf::from("/src/foo.l"), "").unwrap();

        let res = build(Build { dir: None }, vfs);

        // assert!(res.is_ok());
    }

    #[test]
    fn build_project_at_dir_errors() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/eldritch.toml"), project("foo"))
            .unwrap();
        vfs.write(PathBuf::from("/src/foo/bar.l"), "").unwrap();

        // assert!(res == BuildError::PathMustBeALFile(PathBuf::from("/foo")));
    }

    #[test]
    fn build_project_without_manifest() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/src/foo.l"), "").unwrap();

        // assert!(res == ManifestError::ManifestNotFound);
    }

    #[test]
    fn build_project_with_wrong_manifest_type() {
        let mut vfs = VirtualFileSystem::new();
        vfs.write(PathBuf::from("/src/foo.l"), workspace(Vec::new()))
            .unwrap();

        // let mut driver = Driver::new(vfs);
        // driver.submit_task(BuildTask::BuildProject(PathBuf::from("/foo.l")));
        // let res = driver.execute().unwrap_err();

        // assert!(
        //     res == ManifestError::ManifestTypeMismatch {
        //         expected: ManifestType::Project,
        //         actual: ManifestType::Workspace
        //     }
        // );
    }
}
