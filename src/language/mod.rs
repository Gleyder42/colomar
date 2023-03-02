pub mod parser;
pub mod lexer;
pub mod ast;

mod validator;
mod compiler;
mod im;

pub type Span = std::ops::Range<usize>;
