pub mod module;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::driver::source::module::SourceModule;

#[derive(Debug, Default)]
pub struct SourceGraph {
    source_ids: HashMap<PathBuf, SourceId>,
    source_modules: Vec<SourceModule>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourceId(usize);

impl SourceGraph {
    pub fn new() -> SourceGraph {
        SourceGraph {
            source_ids: HashMap::new(),
            source_modules: Vec::new(),
        }
    }

    pub fn insert_path(&mut self, path: impl Into<PathBuf>) -> SourceId {
        let id = SourceId(self.source_ids.len());
        *self.source_ids.entry(path.into()).or_insert(id)
    }

    pub fn find(&self, path: impl AsRef<Path>) -> Option<SourceId> {
        self.source_ids.get(path.as_ref()).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_sources() {
        let mut source_graph = SourceGraph::new();
        let foo_luau_1 = source_graph.insert_path("~/git/project/src/foo.luau");
        let foo_luau_2 = source_graph.insert_path("~/git/project/src/foo.luau");
        assert_eq!(foo_luau_1, foo_luau_2);

        let bar_luau = source_graph.insert_path("~/git/project/src/bar.luau");
        assert_ne!(foo_luau_1, bar_luau);
    }
}
