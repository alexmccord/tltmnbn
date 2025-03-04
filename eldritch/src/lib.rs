pub mod build;
pub use build::{build, Build};

#[derive(clap::Parser, Debug)]
pub enum Command {
    Build(Build),
}
