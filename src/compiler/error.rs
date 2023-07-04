use crate::compiler::cir::{
    CalledType, CalledTypes, DeclaredArgumentId, EventDeclarationId, RValue, StructDeclarationId,
    Type,
};
use crate::compiler::trisult::Trisult;
use crate::compiler::{Ident, QueryTrisult, Span, Text};
use crate::query_error;
use either::Either;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompilerError {
    NotImplemented(&'static str, Span),
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
    PlaceholderError(String),
    // TODO Add any information
    WstLexerError,
    // TODO Add any information
    WstParserError,
    MissingArgument {
        missing_arg: DeclaredArgumentId,
        call_site: Span,
    },
    CannotFindNamedArgument(Ident),
    ArgumentOutOfRange(usize, Span),
    DuplicateNamedArgument(Ident),
    CannotMixArguments(Span),
    CannotEvalAsConst,
}

impl CompilerError {
    // TODO Create pattern for error codes
    // Possibly use non ids for error codes?
    pub fn error_code(&self) -> u16 {
        match self {
            CompilerError::NotImplemented(..) => 0,
            CompilerError::DuplicateIdent { .. } => 1,
            CompilerError::CannotFindDefinition(_) => 2,
            CompilerError::CannotFindIdent(_) => 3,
            CompilerError::NotA(_, _, _) => 4,
            CompilerError::WrongType { .. } => 5,
            CompilerError::CannotFindPrimitiveDeclaration(_) => 6,
            CompilerError::CannotFindNativeDefinition(_) => 7,
            CompilerError::InvalidNativeDefinition(_) => 8,
            CompilerError::NoCaller => 9,
            CompilerError::WstLexerError => 10,
            CompilerError::WstParserError => 11,
            CompilerError::PlaceholderError(_) => 12,
            CompilerError::MissingArgument { .. } => 13,
            CompilerError::CannotFindNamedArgument(_) => 14,
            CompilerError::ArgumentOutOfRange(_, _) => 15,
            CompilerError::CannotMixArguments(_) => 16,
            CompilerError::DuplicateNamedArgument(_) => 17,
            CompilerError::CannotEvalAsConst => 18,
        }
    }
}

impl QueryTrisult<()> {
    pub fn assume_or(expr: bool, reason: &'static str, span: Span) -> QueryTrisult<()> {
        if expr {
            QueryTrisult::Ok(())
        } else {
            query_error!(CompilerError::NotImplemented(reason, span))
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
