use compiler::analysis::interner::InternerDatabase;
use compiler::span::SpanInterner;
use compiler::span::SpanInternerDatabase;
use compiler::span::SpanSourceId;
use compiler::span::StringInternerDatabase;
use regex::RegexBuilder;

#[salsa::database(
    SpanInternerDatabase,
    TestDatabaseHelperDatabase,
    StringInternerDatabase,
    InternerDatabase
)]
#[derive(Default)]
pub struct TestDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestDatabase {}

#[salsa::query_group(TestDatabaseHelperDatabase)]
pub trait TestDatabaseHelper: SpanInterner {
    fn intern_str(&self, name: &'static str) -> SpanSourceId;
}

fn intern_str(db: &dyn TestDatabaseHelper, name: &'static str) -> SpanSourceId {
    db.intern_span_source(name.into())
}

#[macro_export]
macro_rules! assert_patterns {
    ($str:expr, $($pattern:expr),+) => {
        let keywords = vec![$($pattern),+];
        let result = test_common::contains_many(&keywords, $str);
        let message = format!("Check that {:?} are present but {:?} were not", &keywords, &result);
        assert!(result.is_empty(), "{}", message);
    };
}

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
