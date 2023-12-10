extern crate core;

use crate::compiler::span::{Span, SpanLocation, SpanSourceId, StringId, StringInterner};
use chumsky::prelude::*;

use std::fmt::{Debug, Display, Formatter};
use std::string::String;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Token {
    Rule,
    Event,
    Cond,
    Native,
    Enum,
    By,
    Partial,
    Struct,
    GetVar,
    SetVar,
    Val,
    Var,
    Fn,
    Type,
    Import,
    Pub,
    Ident(StringId),
    String(StringId),
    Num(StringId),
    Ctrl(char),
    Dctrl([char; 2]), // Delete this maybe?
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Rule => write!(f, "rule"),
            Token::Event => write!(f, "event"),
            Token::Cond => write!(f, "cond"),
            Token::Native => write!(f, "native"),
            Token::Enum => write!(f, "enum"),
            Token::By => write!(f, "by"),
            Token::Struct => write!(f, "struct"),
            Token::Partial => write!(f, "partial"),
            Token::GetVar => write!(f, "getvar"),
            Token::SetVar => write!(f, "setvar"),
            Token::Fn => write!(f, "fn"),
            Token::Type => write!(f, "type"),
            Token::Val => write!(f, "val"),
            Token::Var => write!(f, "var"),
            Token::Pub => write!(f, "pub"),
            Token::Import => write!(f, "import"),
            Token::Ident(string) => write!(f, "{string:?}"),
            Token::String(string) => write!(f, "{string:?}"),
            Token::Num(string) => write!(f, "{string:?}"),
            Token::Ctrl(ctrl) => write!(f, "{ctrl}"),
            Token::Dctrl(ctrl) => write!(f, "{}{}", ctrl[0], ctrl[1]),
        }
    }
}

pub type LexerExtra<'a> = extra::Err<Simple<'a, char>>;

pub fn lexer<'src>(
    span_source_id: SpanSourceId,
    string_interner: &'src (impl StringInterner + ?Sized),
) -> impl Parser<'src, &'src str, Vec<(Token, Span)>, LexerExtra<'src>> {
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .slice()
        .map(|it: &str| Token::Num(string_interner.intern_string(it.to_owned())));

    let string = just('"')
        .ignore_then(any().filter(|c| *c != '"').repeated().collect::<String>())
        .then_ignore(just('"'))
        .map(|it| Token::String(string_interner.intern_string(it)));

    let ctrl = one_of("(){},.:|=;+-/*&|!").map(Token::Ctrl);

    let ident = text::ident().map(|ident: &str| match ident {
        "rule" => Token::Rule,
        "cond" => Token::Cond,
        "native" => Token::Native,
        "event" => Token::Event,
        "enum" => Token::Enum,
        "by" => Token::By,
        "partial" => Token::Partial,
        "struct" => Token::Struct,
        "getvar" => Token::GetVar,
        "setvar" => Token::SetVar,
        "pub" => Token::Pub,
        "fn" => Token::Fn,
        "type" => Token::Type,
        "val" => Token::Val,
        "var" => Token::Var,
        "import" => Token::Import,
        _ => Token::Ident(string_interner.intern_string(ident.to_owned())),
    });

    let token = choice((ident, num, string, ctrl));

    token
        .padded()
        .map_with_span(move |tok, span| {
            (
                tok,
                Span {
                    location: SpanLocation::from(span),
                    source: span_source_id,
                },
            )
        })
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::database::test::TestDatabase;
    use crate::compiler::database::test::TestDatabaseHelper;
    use crate::test_assert::assert_vec;
    use chumsky::Parser;

    #[test]
    #[should_panic]
    fn test_end_is_consumed() {
        let interner = TestDatabase::default();
        let span_source_id = interner.intern_str("test_end_is_consumed");

        let code = "hello test νρσ";
        let _ = lexer(span_source_id, &interner).parse(code).unwrap();
    }

    #[test]
    fn test_number_lexer() {
        let code = "1 5 1.2 123.321";
        let interner = TestDatabase::default();
        let span_source_id = interner.intern_str("test_end_is_consumed");

        let actual = lexer(span_source_id, &interner).parse(code).unwrap();
        let expected = vec![
            Token::Num(interner.intern_string("1".to_string())),
            Token::Num(interner.intern_string("5".to_string())),
            Token::Num(interner.intern_string("1.2".to_string())),
            Token::Num(interner.intern_string("123.321".to_string())),
        ];

        assert_vec(
            &actual.into_iter().map(|i| i.0).collect::<Vec<_>>(),
            &expected,
        );
    }

    #[test]
    fn test_string_lexer() {
        let code = "\"Hello\" \"Hello World\"";
        let interner = TestDatabase::default();
        let span_source_id = interner.intern_str("test_end_is_consumed");

        let actual = lexer(span_source_id, &interner).parse(code).unwrap();
        let expected = vec![
            Token::String(interner.intern_string("Hello".to_string())),
            Token::String(interner.intern_string("Hello World".to_string())),
        ];

        assert_vec(
            &actual.into_iter().map(|i| i.0).collect::<Vec<_>>(),
            &expected,
        );
    }

    #[test]
    fn test_keyword_lexer() {
        let code =
            "rule cond native event enum by partial struct getvar setvar val var fn type pub";
        let interner = TestDatabase::default();
        let span_source_id = interner.intern_str("test_end_is_consumed");

        let actual = lexer(span_source_id, &interner).parse(code).unwrap();
        let expected = vec![
            Token::Rule,
            Token::Cond,
            Token::Native,
            Token::Event,
            Token::Enum,
            Token::By,
            Token::Partial,
            Token::Struct,
            Token::GetVar,
            Token::SetVar,
            Token::Val,
            Token::Var,
            Token::Fn,
            Token::Type,
            Token::Pub,
        ];

        assert_vec(
            &actual.into_iter().map(|i| i.0).collect::<Vec<_>>(),
            &expected,
        );
    }
}
