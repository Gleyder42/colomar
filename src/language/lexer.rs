extern crate core;

use std::fmt::Debug;
use chumsky::prelude::*;
use std::string::String;

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Token {
    Rule,
    Cond,
    Ident(String),
    String(String),
    Num(String),
    Ctrl(char)
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Token::Num);

    let string = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::String);

    let ctrl = one_of("(){},.:;".chars())
        .map(|c| Token::Ctrl(c));

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "rule" => Token::Rule,
        "cond" => Token::Cond,
        _ => Token::Ident(ident),
    });

    let token = num
        .or(string)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;
    use crate::language::lexer::{lexer, Token};
    use crate::test_assert::assert_into_iter;

    #[test]
    fn test_number_lexer() {
        let code = "1 5 1.2 123.321";

        let actual = lexer().parse(code).unwrap();
        let expected = vec![
            Token::Num("1".to_string()),
            Token::Num("5".to_string()),
            Token::Num("1.2".to_string()),
            Token::Num("123.321".to_string())
        ];

        assert_into_iter(actual.into_iter().map(|i| i.0),expected);
    }

    #[test]
    fn test_string_lexer() {
        let code = "\"Hello\" \"Hello World\"";

        let actual = lexer().parse(code).unwrap();
        let expected = vec![
            Token::String("Hello".to_string()),
            Token::String("Hello World".to_string()),
        ];

        assert_into_iter(actual.into_iter().map(|i| i.0),expected);
    }

    #[test]
    fn test_keyword_lexer() {
        let code = "rule cond";

        let actual = lexer().parse(code).unwrap();
        let expected = vec![
            Token::Rule,
            Token::Cond,
        ];

        assert_into_iter(actual.into_iter().map(|i| i.0),expected);
    }
}