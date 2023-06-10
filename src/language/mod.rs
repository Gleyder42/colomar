use std::ops::Range;
use crate::language::ast::SpannedBool;
use smol_str::SmolStr;

pub mod analysis;
pub mod ast;
pub mod error;
pub mod im;
pub mod lexer;
pub mod parser;
pub mod interner;

pub type SpanSource = usize;

// pub mod converter;
pub type Span = (SpanSource, Range<usize>);


pub type ImmutableString = SmolStr;

const CONDITIONS_LEN: usize = 6;
const ACTIONS_LEN: usize = 8;
const DECLARED_ARGUMENTS_LEN: usize = 4;
const PROPERTY_DECLS_LEN: usize = 4;
const FUNCTIONS_DECLS_LEN: usize = 6;
const ENUM_CONSTANTS_LEN: usize = 8;

const CALLED_ARGUMENTS_LEN: usize = DECLARED_ARGUMENTS_LEN;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    pub value: ImmutableString,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Spanned { value, span }
    }

    pub fn ignore_value(option: Option<T>, span: Span) -> SpannedBool {
        option.map(|_| Spanned::new((), span))
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T, I: IntoIterator<Item = T>> IntoIterator for Spanned<I> {
    type Item = T;
    type IntoIter = <I as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.value.into_iter()
    }
}

impl<T> Spanned<T> {
    pub fn inner_into<U: From<T>>(self) -> Spanned<U> {
        Spanned {
            value: self.value.into(),
            span: self.span,
        }
    }
}
