use compiler::Compiler;
use regex::RegexBuilder;
use std::io;
use std::io::Write;
use std::path::PathBuf;

pub fn contains_many<'a>(patterns: &[&'a str], input: &str) -> Vec<&'a str> {
    patterns
        .into_iter()
        .filter_map(|pattern| {
            let regex = RegexBuilder::new(*pattern)
                .case_insensitive(true)
                .build()
                .unwrap();

            regex.find(input).map(|_| None).unwrap_or(Some(*pattern))
        })
        .collect()
}

macro_rules! assert_patterns {
    ($str:expr, $($pattern:expr),+) => {
        let keywords = vec![$($pattern),+];
        let result = contains_many(&keywords, $str);
        let message = format!("Check that {:?} are present but {:?} were not", &keywords, &result);
        assert!(result.is_empty(), "{}", message);
    };
}

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
