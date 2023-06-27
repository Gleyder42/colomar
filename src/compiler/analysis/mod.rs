use crate::compiler::cir::{
    CalledType, CalledTypes, EventDeclarationId, RValue, StructDeclarationId, Type,
};
use crate::compiler::trisult::Trisult;
use crate::compiler::{Ident, Text};
use crate::query_error;
use decl::DeclDatabase;
use def::DefDatabase;
use either::Either;
use interner::InternerDatabase;

use crate::compiler::SpanInternerDatabase;

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

// TODO Move this one mod up
pub type QueryTrisult<T> = Trisult<T, AnalysisError>;

#[salsa::database(DeclDatabase, DefDatabase, SpanInternerDatabase, InternerDatabase)]
#[derive(Default)]
pub struct AnalysisDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for AnalysisDatabase {}

#[cfg(test)]
pub mod test {
    use super::interner::InternerDatabase;
    use crate::compiler::analysis::interner::Interner;
    use crate::compiler::SpanSourceId;

    #[salsa::database(InternerDatabase, TestDatabaseHelperDatabase)]
    #[derive(Default)]
    pub struct TestDatabase {
        storage: salsa::Storage<Self>,
    }

    impl salsa::Database for TestDatabase {}

    #[salsa::query_group(TestDatabaseHelperDatabase)]
    pub trait TestDatabaseHelper: Interner {
        fn intern_str(&self, name: &'static str) -> SpanSourceId;
    }

    fn intern_str(db: &dyn TestDatabaseHelper, name: &'static str) -> SpanSourceId {
        db.intern_span_source(name.into())
    }
}

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
    CannotFindDefinition(Either<StructDeclarationId, EventDeclarationId>),
    CannotFindIdent(Ident),
    NotA(&'static str, RValue, Ident),
    // TODO Dont use either here, make an own type
    WrongType {
        actual: CalledType,
        expected: Either<Type, CalledTypes>,
    },
    CannotFindPrimitiveDeclaration(Text),
    CannotFindNativeDefinition(Text),
    // TODO Add more information
    InvalidNativeDefinition(&'static str),
    NoCaller,
}

impl AnalysisError {
    pub fn error_code(&self) -> u16 {
        match self {
            AnalysisError::DuplicateIdent { .. } => 1,
            AnalysisError::CannotFindDefinition(_) => 2,
            AnalysisError::CannotFindIdent(_) => 3,
            AnalysisError::NotA(_, _, _) => 4,
            AnalysisError::WrongType { .. } => 5,
            AnalysisError::CannotFindPrimitiveDeclaration(_) => 6,
            AnalysisError::CannotFindNativeDefinition(_) => 7,
            AnalysisError::InvalidNativeDefinition(_) => 8,
            AnalysisError::NoCaller => 9,
        }
    }
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
