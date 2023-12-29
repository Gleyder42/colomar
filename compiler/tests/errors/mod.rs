use crate::assert_patterns;
use compiler::Compiler;
use std::io;
use std::io::Write;
use std::path::PathBuf;

fn test_template(name: &str, assert: impl Fn(&str)) {
    let mut compiler = Compiler::new(PathBuf::from("../res/test/errors"));
    compiler.set_main_file_path(&format!("{name}.co"));
    let (stdout, stderr) = compiler.compile();
    let lossy_utf8 = String::from_utf8_lossy(&stderr).to_string();

    io::stdout().write(&stdout).unwrap();
    io::stderr().write(&stderr).unwrap();

    assert(&lossy_utf8);
}

#[test]
fn test_struct_not_found() {
    test_template("test_struct_not_found", |src| {
        assert_patterns!(&src, "Cannot", "find", "Player", "struct");
    });
}

#[test]
fn test_test_duplicate_ident() {
    test_template("test_duplicate_ident", |src| {
        assert_patterns!(&src, "Duplicated", "ident", "First", "Second");
    });
}

#[test]
fn test_cannot_find_file() {
    test_template("test_cannot_find_file", |src| {
        assert_patterns!(&src, "");
    });
}
