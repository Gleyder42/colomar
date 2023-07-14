extern crate core;

use crate::{CheapRange, Span, SpanSourceId, Text};
use chumsky::prelude::*;
use chumsky::text::Character;
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
    // Rename to partial
    Open,
    Struct,
    GetVar,
    Val,
    Fn,
    NewLine,
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
            Token::Fn => write!(f, "fn"),
            Token::Type => write!(f, "type"),
            Token::Val => write!(f, "val"),
            Token::NewLine => write!(f, "newline"),
            Token::Ident(string) => write!(f, "{string}"),
            Token::String(string) => write!(f, "{string}"),
            Token::Num(string) => write!(f, "{string}"),
            Token::Ctrl(ctrl) => write!(f, "{ctrl}"),
        }
    }
}

pub fn lexer(
    span_source_id: SpanSourceId,
) -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
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

    let ctrl = one_of("(){},.:|=").map(Token::Ctrl);

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
        "fn" => Token::Fn,
        "type" => Token::Type,
        "val" => Token::Val,
        _ => Token::Ident(Text::new(ident)),
    });

    let newline = text::newline().map(|_| Token::NewLine);

    let token = choice((num, newline, string, ctrl, ident)).recover_with(skip_then_retry_until([]));

    let whitespace = filter(|c: &char| c.is_whitespace() && !NEWLINE_CHARS.contains(c))
        .ignored()
        .repeated();

    token
        .padded_by(whitespace)
        .map_with_span(move |tok, span| (tok, Span::new(span_source_id, CheapRange::from(span))))
        .repeated()
        .then_ignore(end())
}

const NEWLINE_CHARS: [char; 7] = [
    '\n',       // line feed
    '\r',       // Carriage return
    '\x0B',     // Vertical tab
    '\x0C',     // Form feed
    '\u{0085}', // Next line
    '\u{2028}', // Line separator
    '\u{2029}', // Paragraph separator
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::database::test::TestDatabase;
    use crate::database::test::TestDatabaseHelper;
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

        let actual = lexer(span_source_id).parse(code).unwrap();
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

        let actual = lexer(span_source_id).parse(code).unwrap();
        let expected = vec![
            Token::String("Hello".to_string().into()),
            Token::String("Hello World".to_string().into()),
        ];

        assert_vec(&actual.into_iter().map(|i| i.0).collect(), &expected);
    }

    #[test]
    fn test_newline() {
        let code = "\n \n\n rule\n \rrule";
        let interner = TestDatabase::default();
        let span_source_id = interner.intern_str("test_end_is_consumed");

        let actual = lexer(span_source_id).parse(code).unwrap();
        let expected = vec![
            Token::NewLine,
            Token::NewLine,
            Token::NewLine,
            Token::Rule,
            Token::NewLine,
            Token::NewLine,
            Token::Rule,
        ];

        assert_vec(&actual.into_iter().map(|i| i.0).collect(), &expected);
    }

    #[test]
    fn test_keyword_lexer() {
        let code = "rule cond native event enum by open struct getvar val fn type";
        let interner = TestDatabase::default();
        let span_source_id = interner.intern_str("test_end_is_consumed");

        let actual = lexer(span_source_id).parse(code).unwrap();
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
            Token::Val,
            Token::Fn,
            Token::Type,
        ];

        assert_vec(&actual.into_iter().map(|i| i.0).collect(), &expected);
    }
}
