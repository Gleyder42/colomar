use decl::DeclDatabase;
use def::DefDatabase;
use interner::InternerDatabase;
use crate::language::error::Trisult;
use crate::language::Ident;
use crate::query_error;

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
pub mod rule;
pub mod property;
pub mod function;
pub mod decl;
pub mod def;

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
    DuplicateIdent {
        first: Ident,
        second: Ident,
    },
    CannotFindDefinition(salsa::InternId),
    CannotFindIdent(Ident),
    WrongType,
    NotABool,
    CannotFindPrimitive,
}

impl<T> Into<Result<T, AnalysisError>> for AnalysisError {
    fn into(self) -> Result<T, AnalysisError> {
        Err(self)
    }
}

impl<T> Into<Trisult<T, AnalysisError>> for AnalysisError {
    fn into(self) -> Trisult<T, AnalysisError> {
        query_error!(self)
    }
}
