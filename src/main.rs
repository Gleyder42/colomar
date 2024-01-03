#![feature(result_flattening)]
#![feature(map_try_insert)]

use compiler::Compiler;
use std::io;
use std::io::Write;
use std::path::PathBuf;

fn main() {
    let mut compiler = Compiler::new(PathBuf::from("res\\test\\errors"));
    compiler.set_main_file_path("bad_enum.co");
    let (mut stdout, mut stderr) = compiler.compile();

    io::stdout().write_all(&mut stdout).unwrap();
    io::stderr().write_all(&mut stderr).unwrap();
}
