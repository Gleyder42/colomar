use compiler::span::SpanInterner;
use compiler::span::SpanInternerDatabase;
use compiler::span::SpanSourceId;
use compiler::span::StringInternerDatabase;

#[salsa::database(
    SpanInternerDatabase,
    TestDatabaseHelperDatabase,
    StringInternerDatabase
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
