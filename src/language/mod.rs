use std::rc::Rc;
use derivative::Derivative;
use crate::language::ast::SpannedBool;

pub mod parser;
pub mod lexer;
pub mod ast;
pub mod im;
pub mod analysis;

// pub mod converter;
pub type Span = std::ops::Range<usize>;
pub type ImmutableString = Rc<String>;

#[derive(Derivative, Debug, Hash, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Ident {
    pub value: ImmutableString,
    pub span: Span
}

impl<T> Spanned<T> {

    pub fn new(value: T, span: crate::Span) -> Self {
        Spanned { value, span }
    }

    pub fn ignore_value(option: Option<T>, span: crate::Span) -> SpannedBool {
        option.map(|_| Spanned::new((), span))
    }
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: crate::Span
}

impl<T, I: IntoIterator<Item=T>> IntoIterator for Spanned<I> {
    type Item = T;
    type IntoIter = <I as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.value.into_iter()
    }
}
