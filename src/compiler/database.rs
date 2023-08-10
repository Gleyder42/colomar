use crate::compiler::analysis::decl::DeclDatabase;
use crate::compiler::analysis::def::DefDatabase;
use crate::compiler::analysis::interner::InternerDatabase;
use crate::compiler::codegen::CodegenDatabase;
use crate::compiler::loader::WorkshopScriptLoaderDatabase;
use crate::compiler::printer::PrinterDatabase;
use crate::compiler::span::SpanInternerDatabase;

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

#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl $crate::salsa::InternKey for $name {
            fn from_intern_id(v: $crate::salsa::InternId) -> Self {
                $name(v)
            }
            fn as_intern_id(&self) -> $crate::salsa::InternId {
                self.0
            }
        }
    };
}

#[cfg(test)]
pub mod test {

    use crate::compiler::span::SpanInterner;
    use crate::compiler::span::SpanInternerDatabase;
    use crate::compiler::span::SpanSourceId;

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
