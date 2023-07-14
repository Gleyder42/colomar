use crate::analysis::decl::DeclDatabase;
use crate::analysis::def::DefDatabase;
use crate::analysis::interner::InternerDatabase;
use crate::codegen::CodegenDatabase;
use crate::loader::WorkshopScriptLoaderDatabase;
use crate::printer::PrinterDatabase;
use crate::SpanInternerDatabase;

#[salsa::database(
    DeclDatabase,
    DefDatabase,
    SpanInternerDatabase,
    InternerDatabase,
    WorkshopScriptLoaderDatabase,
    CodegenDatabase,
    PrinterDatabase
)]
#[derive(Default)]
pub struct CompilerDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for CompilerDatabase {}

#[cfg(test)]
pub mod test {

    use crate::SpanInternerDatabase;
    use crate::{SpanInterner, SpanSourceId};

    #[salsa::database(SpanInternerDatabase, TestDatabaseHelperDatabase)]
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
}
