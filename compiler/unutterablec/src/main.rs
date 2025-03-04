use std::path::PathBuf;
use std::process;

use clap::Parser;

use unutterablec;
use unutterablec::fs;

#[derive(clap::Parser)]
pub struct Cli {
    pub input: PathBuf,
    #[arg(long = "output")]
    pub output: PathBuf,
}

fn main() {
    let cli = Cli::parse();

    let options = unutterablec::CompileOptions {
        input: cli.input,
        output: cli.output,
    };

    match unutterablec::compile(options, fs::FileSystemIO) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("{e}");
            process::exit(1);
        }
    }
}
