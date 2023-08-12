extern crate core;

use crate::compiler::span::{InnerSpan, OffsetTable, SimpleSpanLocation, Span, SpanSourceId};
use crate::compiler::{span, Text};
use chumsky::prelude::*;

use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;
use std::string::String;

#[derive(Debug)]
pub struct UndecidedSpan(Vec<(Token, Span)>);

impl UndecidedSpan {
    pub fn into_relative_span(mut self) -> (Vec<(Token, Span)>, OffsetTable) {
        let mut references: Vec<_> = self
            .0
            .iter_mut()
            .map(|(_, span)| &mut span.location)
            .collect();
        const LEVEL: u16 = 1;
        let table = span::encode_in_place(LEVEL, &mut references);
        (self.0, table)
    }

    pub fn into_simple_spans(self) -> Vec<(Token, Span)> {
        self.0
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Token {
    Rule,
    Event,
    Cond,
    Native,
    Enum,
    By,
    Open, // Rename to partial
    Struct,
    GetVar,
    SetVar,
    Val,
    Var,
    Fn,
    Type,
    Ident(Text),
    String(Text),
    Num(Text),
    Ctrl(char),
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
            Token::Open => write!(f, "open"),
            Token::GetVar => write!(f, "getvar"),
            Token::SetVar => write!(f, "setvar"),
            Token::Fn => write!(f, "fn"),
            Token::Type => write!(f, "type"),
            Token::Val => write!(f, "val"),
            Token::Var => write!(f, "var"),
            Token::Ident(string) => write!(f, "{string}"),
            Token::String(string) => write!(f, "{string}"),
            Token::Num(string) => write!(f, "{string}"),
            Token::Ctrl(ctrl) => write!(f, "{ctrl}"),
        }
    }
}

pub fn lexer(
    span_source_id: SpanSourceId,
) -> impl Parser<char, UndecidedSpan, Error = Simple<char>> {
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Text::new)
        .map(Token::Num);

    let string = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Text::new)
        .map(Token::String);

    let ctrl = one_of("(){},.:|=;").map(Token::Ctrl);

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "rule" => Token::Rule,
        "cond" => Token::Cond,
        "native" => Token::Native,
        "event" => Token::Event,
        "enum" => Token::Enum,
        "by" => Token::By,
        "open" => Token::Open,
        "struct" => Token::Struct,
        "getvar" => Token::GetVar,
        "setvar" => Token::SetVar,
        "fn" => Token::Fn,
        "type" => Token::Type,
        "val" => Token::Val,
        "var" => Token::Var,
        _ => Token::Ident(Text::new(ident)),
    });

    let token = choice((ident, num, string, ctrl)).recover_with(skip_then_retry_until([]));

    token
        .padded()
        .map_with_span(move |tok, span| {
            (
                tok,
                Span::new(span_source_id, SimpleSpanLocation::from(span)),
            )
        })
        .repeated()
        .map(UndecidedSpan)
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
        let _ = lexer(span_source_id).parse(code).unwrap();
    }

    #[test]
    fn test_number_lexer() {
        let code = "1 5 1.2 123.321";
        let interner = TestDatabase::default();
        let span_source_id = interner.intern_str("test_end_is_consumed");

        let actual = lexer(span_source_id)
            .parse(code)
            .unwrap()
            .into_simple_spans();
        let expected = vec![
            Token::Num("1".to_string().into()),
            Token::Num("5".to_string().into()),
            Token::Num("1.2".to_string().into()),
            Token::Num("123.321".to_string().into()),
        ];

        assert_vec(&actual.into_iter().map(|i| i.0).collect(), &expected);
    }

    #[test]
    fn test_string_lexer() {
        let code = "\"Hello\" \"Hello World\"";
        let interner = TestDatabase::default();
        let span_source_id = interner.intern_str("test_end_is_consumed");

        let actual = lexer(span_source_id)
            .parse(code)
            .unwrap()
            .into_simple_spans();
        let expected = vec![
            Token::String("Hello".to_string().into()),
            Token::String("Hello World".to_string().into()),
        ];

        assert_vec(&actual.into_iter().map(|i| i.0).collect(), &expected);
    }

    #[test]
    fn test_keyword_lexer() {
        let code = "rule cond native event enum by open struct getvar setvar val var fn type";
        let interner = TestDatabase::default();
        let span_source_id = interner.intern_str("test_end_is_consumed");

        let actual = lexer(span_source_id)
            .parse(code)
            .unwrap()
            .into_simple_spans();
        let expected = vec![
            Token::Rule,
            Token::Cond,
            Token::Native,
            Token::Event,
            Token::Enum,
            Token::By,
            Token::Open,
            Token::Struct,
            Token::GetVar,
            Token::SetVar,
            Token::Val,
            Token::Var,
            Token::Fn,
            Token::Type,
        ];

        assert_vec(&actual.into_iter().map(|i| i.0).collect(), &expected);
    }
}
