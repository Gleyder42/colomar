use chumsky::prelude::*;
use crate::language::workshop::lexer::Token;
use crate::language::workshop::tree::Ident;

type ParserError = Simple<Token>;

fn ident() -> impl Parser<Token, Ident, Error = ParserError> {
    filter_map(|span, token| match token {
        Token::Ident(text) => Ok(Ident(text)),
        _ => Err(ParserError::expected_input_found(
            span,
            Vec::new(),
            Some(token),
        )),
    })
}