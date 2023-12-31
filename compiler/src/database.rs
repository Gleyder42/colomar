use super::analysis::decl::DeclDatabase;
use super::analysis::def::DefDatabase;
use super::analysis::interner::InternerDatabase;
use super::codegen::CodegenDatabase;
use super::loader::WorkshopScriptLoaderDatabase;
use super::printer::PrinterDatabase;
use super::span::SpanInternerDatabase;
use super::span::StringInternerDatabase;

#[salsa::database(
    DeclDatabase,
    DefDatabase,
    SpanInternerDatabase,
    InternerDatabase,
    WorkshopScriptLoaderDatabase,
    CodegenDatabase,
    PrinterDatabase,
    StringInternerDatabase
)]
#[derive(Default)]
pub struct CompilerDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for CompilerDatabase {}

#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        pub struct $name(salsa::InternId);

        impl salsa::InternKey for $name {
            fn from_intern_id(v: salsa::InternId) -> Self {
                $name(v)
            }
            fn as_intern_id(&self) -> salsa::InternId {
                self.0
            }
        }
    };
}
