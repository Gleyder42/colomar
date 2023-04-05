extern crate core;

use std::fmt::{Debug, Display, Formatter};
use chumsky::prelude::*;
use std::string::String;
use crate::language::Span;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Token {
    ContextAssigment,
    Rule,
    Event,
    Cond,
    Workshop,
    Enum,
    By,
    Ident(String),
    String(String),
    Num(String),
    Ctrl(char)
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::ContextAssigment => write!(f, "ContextAssigment"),
            Token::Rule => write!(f, "rule"),
            Token::Event => write!(f, "event"),
            Token::Cond => write!(f, "cond"),
            Token::Workshop => write!(f, "workshop"),
            Token::Enum => write!(f, "enum"),
            Token::By => write!(f, "by"),
            Token::Ident(string) => write!(f, "Ident {string}"),
            Token::String(string) => write!(f, "String {string}"),
            Token::Num(string) => write!(f, "Num {string}"),
            Token::Ctrl(ctrl) => write!(f, "Ctrl {ctrl}"),
        }
    }
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

    let ctrl = one_of("(){},.:;|=")
        .map(|c| Token::Ctrl(c));

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "rule" => Token::Rule,
        "cond" => Token::Cond,
        "workshop" => Token::Workshop,
        "event" => Token::Event,
        "enum" => Token::Enum,
        "by" => Token::By,
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
    use crate::test_assert::assert_vec;

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

        assert_vec(&actual.into_iter().map(|i| i.0).collect(), &expected);
    }

    #[test]
    fn test_string_lexer() {
        let code = "\"Hello\" \"Hello World\"";

        let actual = lexer().parse(code).unwrap();
        let expected = vec![
            Token::String("Hello".to_string()),
            Token::String("Hello World".to_string()),
        ];

        assert_vec(&actual.into_iter().map(|i| i.0).collect(), &expected);
    }

    #[test]
    fn test_keyword_lexer() {
        let code = "rule cond workshop event enum by";

        let actual = lexer().parse(code).unwrap();
        let expected = vec![
            Token::Rule,
            Token::Cond,
            Token::Workshop,
            Token::Event,
            Token::Enum,
            Token::By
        ];

        assert_vec(&actual.into_iter().map(|i| i.0).collect(), &expected);
    }
}