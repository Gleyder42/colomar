use derivative::Derivative;

pub mod parser;
pub mod lexer;
pub mod ast;
pub mod im;

pub mod converter;

pub type Span = std::ops::Range<usize>;

#[derive(Derivative, Debug, Hash, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Ident {
    pub value: String,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

#[cfg(test)]
impl Ident {

    fn new(value: impl Into<String>, span: Span) -> Ident {
        Ident { value: value.into(), span }
    }
}