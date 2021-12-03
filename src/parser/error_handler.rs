use crate::lexer::Token;
use crate::parser::parser::TokenIter;

pub struct ErrorRecovery;

impl ErrorRecovery {
    pub fn new() -> ErrorRecovery {
        ErrorRecovery {}
    }

    pub fn recover_struct_declaration(&self, iter: TokenIter, token: Option<Token>) -> Token {
        todo!()
    }
}