#![feature(pattern)]

use regex::RegexBuilder;
use std::str::pattern::Pattern;

mod errors;

fn contains_many(patterns: Vec<&str>, input: &str) -> bool {
    for pattern in patterns {
        let regex = RegexBuilder::new(pattern)
            .case_insensitive(false)
            .build()
            .unwrap();
        let result = regex.find(input);
        match result {
            Some(_) => continue,
            None => return false,
        }
    }

    true
}

#[macro_export]
macro_rules! assert_patterns {
    ($str:expr, $($pattern:expr),+) => {
        let keywords = vec![$($pattern),+];
        let message = format!("Check that {:?} are present", &keywords);
        let result = $crate::contains_many(keywords, $str);
        assert!(result, "{}", message)
    };
}
