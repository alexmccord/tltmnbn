pub mod source;

use std::fs;
use std::io;
use std::path::PathBuf;

use crate::driver::source::SourceGraph;

#[derive(Debug, Default)]
pub struct Driver {
    config: DriverConfig,
    source_graph: SourceGraph,
}

#[derive(Debug, Default, Clone)]
pub struct DriverConfig {
    cwd: PathBuf,
}

impl Driver {
    pub fn new(config: DriverConfig) -> Driver {
        Driver {
            config,
            source_graph: SourceGraph::new(),
        }
    }

    pub fn add_source(&mut self, path: impl Into<PathBuf>) -> io::Result<()> {
        let path = path.into();
        let source_code = fs::read_to_string(&path)?;
        let source_id = self.source_graph.insert_path(path);
        Ok(())
    }
}
