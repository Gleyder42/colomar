use crate::compiler::analysis::interner::Interner;
use crate::compiler::cir::{
    AValue, CalledType, CalledTypes, DeclArgId, EventDeclId, StructDeclId, Type,
};
use crate::compiler::cst::Path;
use crate::compiler::span::Span;
use crate::compiler::trisult::Trisult;
use crate::compiler::wst::partial::SaturateError;
use crate::compiler::{workshop, Ident, QueryTrisult, StructId, Text};
use crate::query_error;
use chumsky::error::Rich;
use either::Either;
use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompilerError {
    NotImplemented(Cow<'static, str>, Span),
    DuplicateIdent {
        first: Ident,
        second: Ident,
    },
    CannotFindIdent(Ident),
    NotA(&'static str, Ident, Ident),
    // TODO Dont use either here, make an own type
    WrongType {
        actual: CalledType,
        expected: Either<Type, CalledTypes>,
    },
    CannotFindPrimitiveDecl(Text),
    CannotFindNativeDef(String),
    PlaceholderError(SaturateError),
    // TODO Add any information
    WstLexerError(String, Vec<u8>),
    // TODO Add any information
    WstParserError(String, Vec<u8>),
    MissingArg {
        missing_arg: DeclArgId,
        call_site: Span,
    },
    CannotFindNamedArg(Ident),
    ArgOutOfRange(usize, Span),
    DuplicateNamedArg(Ident),
    CannotMixArgs(Span),
    CannotEvalAsConst,
    WrongTypeInBinaryExpr(AValue, AValue),
    CannotFindFile(Path),
    CannotFindStruct(Text),
}

impl CompilerError {
    pub fn main_span(&self) -> Option<Span> {
        match self {
            CompilerError::NotImplemented(_, span) => Some(*span),
            CompilerError::DuplicateIdent { first, .. } => Some(first.span),
            CompilerError::CannotFindIdent(ident) => Some(ident.span),
            CompilerError::NotA(_, ident, _) => Some(ident.span),
            CompilerError::WrongType { actual, .. } => Some(actual.span),
            CompilerError::CannotFindPrimitiveDecl(_) => None,
            CompilerError::CannotFindNativeDef(_) => None,
            CompilerError::PlaceholderError(_) => None,
            CompilerError::WstLexerError(_, _) => None,
            CompilerError::WstParserError(_, _) => None,
            CompilerError::MissingArg { .. } => None,
            CompilerError::CannotFindNamedArg(_) => None,
            CompilerError::ArgOutOfRange(_, _) => None,
            CompilerError::DuplicateNamedArg(_) => None,
            CompilerError::CannotMixArgs(_) => None,
            CompilerError::CannotEvalAsConst => None,
            CompilerError::WrongTypeInBinaryExpr(_, _) => None,
            CompilerError::CannotFindFile(_) => None,
            CompilerError::CannotFindStruct(_) => None,
        }
    }

    pub fn error_code(&self) -> u16 {
        match self {
            CompilerError::NotImplemented(..) => 0,
            CompilerError::DuplicateIdent { .. } => 1,
            CompilerError::CannotFindIdent(_) => 3,
            CompilerError::NotA(_, _, _) => 4,
            CompilerError::WrongType { .. } => 5,
            CompilerError::CannotFindPrimitiveDecl(_) => 6,
            CompilerError::CannotFindNativeDef(_) => 7,
            CompilerError::WstLexerError(_, _) => 10,
            CompilerError::WstParserError(_, _) => 11,
            CompilerError::PlaceholderError(_) => 12,
            CompilerError::MissingArg { .. } => 13,
            CompilerError::CannotFindNamedArg(_) => 14,
            CompilerError::ArgOutOfRange(_, _) => 15,
            CompilerError::CannotMixArgs(_) => 16,
            CompilerError::DuplicateNamedArg(_) => 17,
            CompilerError::CannotEvalAsConst => 18,
            CompilerError::WrongTypeInBinaryExpr(_, _) => 19,
            CompilerError::CannotFindFile(_) => 20,
            CompilerError::CannotFindStruct(_) => 21,
        }
    }
}

impl QueryTrisult<()> {
    pub fn assume_or(expr: bool, reason: &'static str, span: Span) -> QueryTrisult<()> {
        if expr {
            QueryTrisult::Ok(())
        } else {
            query_error!(CompilerError::NotImplemented(reason.into(), span))
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
