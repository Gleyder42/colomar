use decl::DeclDatabase;
use def::DefDatabase;
use interner::InternerDatabase;

pub mod r#enum;
pub mod file;
pub mod interner;
pub mod event;
pub mod arg;
pub mod call;
pub mod namespace;
pub mod r#type;
pub mod im;
pub mod r#struct;
pub mod error;
pub mod rule;
pub mod property;
pub mod function;
pub mod decl;
pub mod def;

#[salsa::database(DeclDatabase, DefDatabase, InternerDatabase)]
#[derive(Default)]
pub struct AnalysisDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for AnalysisDatabase {}

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
