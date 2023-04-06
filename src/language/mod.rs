pub mod parser;
pub mod lexer;
pub mod ast;
pub mod im;

pub mod converter;

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    pub value: String,
    pub span: Span
}

impl Ident {

    fn new(value: String, span: Span) -> Ident {
        Ident { value, span }
    }
}