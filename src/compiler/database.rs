use crate::compiler::analysis::decl::DeclDatabase;
use crate::compiler::analysis::def::DefDatabase;
use crate::compiler::analysis::interner::InternerDatabase;
use crate::compiler::codegen::CodegenDatabase;
use crate::compiler::loader::WorkshopScriptLoaderDatabase;
use crate::compiler::printer::PrinterDatabase;

#[salsa::database(
    DeclDatabase,
    DefDatabase,
    InternerDatabase,
    WorkshopScriptLoaderDatabase,
    CodegenDatabase,
    PrinterDatabase
)]
#[derive(Default)]
pub struct CompilerDatabase {
    storage: salsa::Storage<Self>,
}

pub mod span {
    use crate::compiler::offset::HierOffset;
    use crate::compiler::{
        HierSpan, PosSpan, SpanInternerContext, SpanInternerDatabase, SpanTable,
    };
    use rustc_hash::FxHashMap;

    #[salsa::database(SpanInternerDatabase)]
    #[derive(Default)]
    pub struct SpanDatabase {
        storage: salsa::Storage<Self>,
        span_map: SpanTable,
    }

    impl salsa::Database for SpanDatabase {}

    impl SpanInternerContext for SpanDatabase {
        fn span_table(&self) -> &SpanTable {
            &self.span_map
        }

        fn as_hier_span(&self, offset: HierOffset, span: PosSpan) -> HierSpan {
            HierSpan::from_pos_span(offset.clone(), self, span)
        }
    }
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
    use crate::compiler::offset::HierOffset;
    use crate::compiler::{
        HierSpan, PosSpan, SpanInternerContext, SpanInternerDatabase, SpanTable,
    };
    use crate::compiler::{SpanInterner, SpanSourceId};
    use rustc_hash::FxHashMap;

    #[salsa::database(SpanInternerDatabase, TestDatabaseHelperDatabase)]
    #[derive(Default)]
    pub struct TestDatabase {
        storage: salsa::Storage<Self>,
    }

    impl SpanInternerContext for TestDatabase {
        fn span_table(&self) -> &SpanTable {
            todo!()
        }

        fn as_hier_span(&self, offset: HierOffset, span: PosSpan) -> HierSpan {
            todo!()
        }
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
