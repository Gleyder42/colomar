#![feature(pattern)]

use regex::RegexBuilder;

mod errors;

mod lexer_errors;
mod parser_errors;

fn contains_many<'a>(patterns: &[&'a str], input: &str) -> Vec<&'a str> {
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

#[macro_export]
macro_rules! assert_patterns {
    ($str:expr, $($pattern:expr),+) => {
        let keywords = vec![$($pattern),+];
        let result = $crate::contains_many(&keywords, $str);
        let message = format!("Check that {:?} are present but {:?} were not", &keywords, &result);
        assert!(result.is_empty(), "{}", message)
    };
}
