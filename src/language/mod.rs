pub mod parser;
pub mod lexer;
pub mod ast;
pub mod imt;

pub mod converter;

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    pub value: String,
    pub span: Span
}