use clap::Parser;
use compiler::{Compiler, CompilerOutput};
use std::io;
use std::io::Write;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    project_dir: PathBuf,
}

fn main() {
    let cli = Cli::parse();

    let mut compiler = Compiler::new(cli.project_dir, Some(PathBuf::from("dsl/std_lib")));
    let CompilerOutput { stdout, stderr } = compiler.compile();

    io::stdout().write_all(&stdout).unwrap();
    io::stderr().write_all(&stderr).unwrap();
}
