use compiler::Compiler;
use std::io;
use std::io::Write;
use std::path::PathBuf;
use test_common::assert_patterns;

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
    const MISSING_FILE: &str = "test_cannot_find_file";

    test_template(MISSING_FILE, |src| {
        assert_patterns!(&src, "cannot", "file", MISSING_FILE);
    });
}

#[test]
fn test_cannot_find_native_def() {
    const MISSING_EVENT: &str = "CustomEvent";

    test_template("test_cannot_find_native_def", |src| {
        assert_patterns!(
            &src,
            "cannot",
            "find",
            "native",
            "definition",
            MISSING_EVENT
        );
    });
}

#[test]
fn test_bad_ongoing_global() {
    const BAD_NAME: &str = "Zarya";

    test_template("bad_enum.co", |src| {
        assert_patterns!(&src, "caused", "by", "Expected", BAD_NAME);
    })
}
