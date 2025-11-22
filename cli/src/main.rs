use clap::Parser;
use compiler::{Compiler, CompilerOutput};
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::{fs, io};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    project_dir: PathBuf,

    #[arg(short, long, default_value = "output.txt")]
    output_file: Option<PathBuf>,
}

fn main() {
    let cli = Cli::parse();

    let mut compiler = Compiler::new(cli.project_dir, Some(PathBuf::from("dsl/std_lib")));
    let CompilerOutput { stdout, stderr } = compiler.compile();

    io::stdout().write_all(&stdout).unwrap();
    io::stderr().write_all(&stderr).unwrap();

    if let Some(output_file) = cli.output_file {
        let mut f = OpenOptions::new()
            .write(true)
            .create(true) // create if missing
            .truncate(true) // overwrite existing contents
            .open(output_file)
            .unwrap();
        f.write_all(&stdout).unwrap();
    }
}
