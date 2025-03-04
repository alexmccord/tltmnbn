use clap::Parser;

use eldritch;
use eldritch::Command;

use unutterablec::fs::FileSystemIO;

fn main() {
    let args = Command::parse();

    match args {
        Command::Build(b) => eldritch::build(b, FileSystemIO),
    }
}
