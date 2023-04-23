use derivative::Derivative;

pub mod parser;
pub mod lexer;
pub mod ast;
pub mod im;

// pub mod converter;
mod query;

pub type Span = std::ops::Range<usize>;

#[derive(Derivative, Debug, Hash, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Ident {
    pub value: String,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}