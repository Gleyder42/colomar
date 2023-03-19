pub mod parser;
pub mod lexer;
pub mod ast;
pub mod im;

mod validator;

pub type Span = std::ops::Range<usize>;
