use crate::language::imt::Named;

pub mod parser;
pub mod lexer;
pub mod ast;
pub mod imt;

mod validator;
pub mod converter;

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident(pub String, pub Span);

impl<'a> Named<'a> for Ident {

    fn name(&'a self) -> &'a str {
        &self.0
    }
}
