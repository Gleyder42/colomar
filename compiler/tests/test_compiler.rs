use compiler::Compiler;
use std::io::Write;
use std::path::{Path, PathBuf};
use test_common::assert_patterns;

fn file(name: impl AsRef<Path>) -> PathBuf {
    PathBuf::from("../resources/test/projects").join(name)
}

fn setup(name: impl AsRef<Path>) -> String {
    let path = file(name);
    dbg!(path.exists());
    let mut compiler = Compiler::new(path);

    let (mut stdout, stderr) = compiler.compile();
    std::io::stdout().write_all(&mut stdout).unwrap();
    let lossy_utf8 = String::from_utf8_lossy(&stderr).to_string();
    println!("{lossy_utf8}");
    lossy_utf8
}

#[test]
fn test_compiler_throws_if_file_not_found() {
    let error_output = setup("empty");

    assert_patterns!(&error_output, "cannot", "file", "main.co");
}

#[test]
fn test_compiler_throws_if_no_std() {
    let error_output = setup("only_main");

    assert_patterns!(&error_output, "cannot", "struct", "Player");
}
