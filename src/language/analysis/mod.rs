use crate::language::error::Trisult;
use crate::language::Ident;
use crate::query_error;
use decl::DeclDatabase;
use def::DefDatabase;
use interner::InternerDatabase;

pub mod arg;
pub mod call;
pub mod decl;
pub mod def;
pub mod r#enum;
pub mod event;
pub mod file;
pub mod function;
pub mod im;
pub mod interner;
pub mod namespace;
pub mod property;
pub mod rule;
pub mod r#struct;
pub mod r#type;

type QueryTrisult<T> = Trisult<T, AnalysisError>;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnalysisError {
    DuplicateIdent { first: Ident, second: Ident },
    CannotFindDefinition(salsa::InternId),
    CannotFindIdent(Ident),
    WrongType,
    NotABool,
    CannotFindPrimitive,
}

impl<T> From<AnalysisError> for Result<T, AnalysisError> {
    fn from(value: AnalysisError) -> Self {
        Err(value)
    }
}

impl<T> From<AnalysisError> for Trisult<T, AnalysisError> {
    fn from(value: AnalysisError) -> Self {
        query_error!(value)
    }
}