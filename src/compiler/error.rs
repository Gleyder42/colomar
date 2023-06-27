use either::Either;
use crate::compiler::cir::{CalledType, CalledTypes, EventDeclarationId, RValue, StructDeclarationId, Type};
use crate::compiler::{Ident, Text};
use crate::compiler::trisult::Trisult;
use crate::query_error;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompilerError {
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

impl CompilerError {
    pub fn error_code(&self) -> u16 {
        match self {
            CompilerError::DuplicateIdent { .. } => 1,
            CompilerError::CannotFindDefinition(_) => 2,
            CompilerError::CannotFindIdent(_) => 3,
            CompilerError::NotA(_, _, _) => 4,
            CompilerError::WrongType { .. } => 5,
            CompilerError::CannotFindPrimitiveDeclaration(_) => 6,
            CompilerError::CannotFindNativeDefinition(_) => 7,
            CompilerError::InvalidNativeDefinition(_) => 8,
            CompilerError::NoCaller => 9,
        }
    }
}

impl<T> From<CompilerError> for Result<T, CompilerError> {
    fn from(value: CompilerError) -> Self {
        Err(value)
    }
}

impl<T> From<CompilerError> for Trisult<T, CompilerError> {
    fn from(value: CompilerError) -> Self {
        query_error!(value)
    }
}
