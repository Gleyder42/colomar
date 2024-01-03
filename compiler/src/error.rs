use super::cir::{AValue, CalledType, CalledTypes, DeclArgId, Type};
use super::cst::Path;
use super::span::Span;
use super::trisult::Trisult;
use super::wst::partial::SaturateError;
use super::{workshop, wst, Ident, OwnedRich, PartialQueryTrisult, QueryTrisult, TextId};
use crate::query_error;
use either::Either;
use smallvec::{smallvec, SmallVec};
use std::borrow::Cow;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PartialCompilerError {
    CannotFindPrimitiveDecl(TextId),
    CannotFindNativeDef(String),
    PlaceholderError(SaturateError),
    CannotFindStruct(TextId),
    WstLexerError(Rc<str>, Vec<OwnedRich<char, wst::Span>>),
    WstParserError(Rc<str>, Vec<OwnedRich<workshop::lexer::Token, wst::Span>>),
    CompilerErrors(Vec<CompilerError>),
}

impl PartialCompilerError {
    /// Use a SmallVec as return type, because most of the time one partial error corresponds to one CompilerError.
    /// Using a SmallVec avoids allocating many 1 length vectors.
    fn into_compiler_error(self, error_cause: ErrorCause) -> SmallVec<[CompilerError; 1]> {
        use CompilerError as C;
        use PartialCompilerError as P;

        let error = match self {
            P::CompilerErrors(errors) => return SmallVec::from_vec(errors),
            P::CannotFindPrimitiveDecl(text_id) => C::CannotFindPrimitiveDecl(text_id, error_cause),
            P::CannotFindNativeDef(string) => C::CannotFindNativeDef(string, error_cause),
            P::PlaceholderError(error) => C::PlaceholderError(error, error_cause),
            P::CannotFindStruct(text_id) => C::CannotFindStruct(text_id, error_cause),
            P::WstParserError(source, errors) => C::WstParserError(source, errors, error_cause),
            P::WstLexerError(source, errors) => C::WstLexerError(source, errors, error_cause),
        };
        smallvec![error]
    }
}

fn complete_partial_errors(
    errors: Vec<PartialCompilerError>,
    error_cause: ErrorCause,
) -> Vec<CompilerError> {
    errors
        .into_iter()
        .map(|partial_error| partial_error.into_compiler_error(error_cause.clone()))
        .flatten()
        .collect()
}

impl<T> PartialQueryTrisult<T> {
    pub fn complete_with_span(self, span: Span) -> QueryTrisult<T> {
        self.complete_with_cause(ErrorCause::Span(span))
    }

    pub fn complete_with_message(self, message: impl Into<Cow<'static, str>>) -> QueryTrisult<T> {
        self.complete_with_cause(ErrorCause::Message(message.into()))
    }

    fn complete_with_cause(self, error_cause: ErrorCause) -> QueryTrisult<T> {
        self.map_error(|errors| complete_partial_errors(errors, error_cause))
    }
}

impl<T> QueryTrisult<T> {
    pub fn partial_errors(self) -> PartialQueryTrisult<T> {
        self.map_error(|errors| vec![PartialCompilerError::CompilerErrors(errors)])
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ErrorCause {
    Span(Span),
    Message(Cow<'static, str>),
}

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

    WstLexerError(Rc<str>, Vec<OwnedRich<char, wst::Span>>, ErrorCause),
    WstParserError(
        Rc<str>,
        Vec<OwnedRich<workshop::lexer::Token, wst::Span>>,
        ErrorCause,
    ),
    CannotFindPrimitiveDecl(TextId, ErrorCause),
    CannotFindNativeDef(String, ErrorCause),
    PlaceholderError(SaturateError, ErrorCause),
    CannotFindStruct(TextId, ErrorCause),
}

impl CompilerError {
    pub fn main_span(&self) -> Option<Span> {
        match self {
            CompilerError::NotImplemented(_, span) => Some(*span),
            CompilerError::DuplicateIdent { first, .. } => Some(first.span),
            CompilerError::CannotFindIdent(ident) => Some(ident.span),
            CompilerError::NotA(_, _, ident) => Some(ident.span),
            CompilerError::WrongType { actual, .. } => Some(actual.span),
            CompilerError::CannotFindPrimitiveDecl(_, _) => None,
            CompilerError::CannotFindNativeDef(_, _) => None,
            CompilerError::PlaceholderError(_, _) => None,
            CompilerError::WstParserError(_, _, _) => None,
            CompilerError::WstLexerError(_, _, _) => None,
            CompilerError::MissingArg { call_site, .. } => Some(*call_site),
            CompilerError::CannotFindNamedArg(ident) => Some(ident.span),
            CompilerError::ArgOutOfRange(_, span) => Some(*span),
            CompilerError::DuplicateNamedArg(ident) => Some(ident.span),
            CompilerError::CannotMixArgs(span) => Some(*span),
            CompilerError::CannotEvalAsConst => {
                todo!()
            }
            CompilerError::WrongTypeInBinaryExpr(left, _) => Some(left.span()),
            CompilerError::CannotFindFile(path) => Some(path.span),
            CompilerError::CannotFindStruct(_, _) => None,
        }
    }

    pub fn error_code(&self) -> u16 {
        match self {
            CompilerError::NotImplemented(..) => 0,
            CompilerError::DuplicateIdent { .. } => 1,
            CompilerError::CannotFindIdent(_) => 3,
            CompilerError::NotA(_, _, _) => 4,
            CompilerError::WrongType { .. } => 5,
            CompilerError::CannotFindPrimitiveDecl(_, _) => 6,
            CompilerError::CannotFindNativeDef(_, _) => 7,
            CompilerError::WstLexerError(_, _, _) => 10,
            CompilerError::WstParserError(_, _, _) => 11,
            CompilerError::PlaceholderError(_, _) => 12,
            CompilerError::MissingArg { .. } => 13,
            CompilerError::CannotFindNamedArg(_) => 14,
            CompilerError::ArgOutOfRange(_, _) => 15,
            CompilerError::CannotMixArgs(_) => 16,
            CompilerError::DuplicateNamedArg(_) => 17,
            CompilerError::CannotEvalAsConst => 18,
            CompilerError::WrongTypeInBinaryExpr(_, _) => 19,
            CompilerError::CannotFindFile(_) => 20,
            CompilerError::CannotFindStruct(_, _) => 21,
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
