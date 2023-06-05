use crate::language::ast::SpannedBool;
use smol_str::SmolStr;

pub mod analysis;
pub mod ast;
pub mod error;
pub mod im;
pub mod lexer;
pub mod parser;

// pub mod converter;
pub type Span = std::ops::Range<usize>;
pub type ImmutableString = SmolStr;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    pub value: ImmutableString,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: crate::Span) -> Self {
        Spanned { value, span }
    }

    pub fn ignore_value(option: Option<T>, span: crate::Span) -> SpannedBool {
        option.map(|_| Spanned::new((), span))
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: crate::Span,
}

impl<T, I: IntoIterator<Item = T>> IntoIterator for Spanned<I> {
    type Item = T;
    type IntoIter = <I as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.value.into_iter()
    }
}
