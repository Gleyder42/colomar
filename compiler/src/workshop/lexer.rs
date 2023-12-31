use super::super::wst::partial::Placeholder;
use crate::analysis::interner::Interner;
use crate::span::Span;
use crate::{parser_alias, wst, InternedName};
use chumsky::input::{SpannedInput, Stream};
use chumsky::prelude::*;
use chumsky::text::Char;
use smol_str::SmolStr;
use std::fmt::{Display, Formatter};

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Token {
    Ident(SmolStr),
    String(SmolStr),
    Number(SmolStr),
    Placeholder(SmolStr),
    Ctrl(char),
}

impl InternedName for Token {
    fn name(&self, _: &dyn Interner) -> String {
        match self {
            Token::Ident(text)
            | Token::String(text)
            | Token::Number(text)
            | Token::Placeholder(text) => text.to_string(),
            Token::Ctrl(c) => c.to_string(),
        }
    }
}

impl From<Placeholder> for Token {
    fn from(value: Placeholder) -> Self {
        Token::Placeholder(value.0)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(text) => write!(f, "Ident {}", text),
            Token::String(text) => write!(f, "String {}", text),
            Token::Number(text) => write!(f, "Number {}", text),
            Token::Ctrl(ctrl) => write!(f, "{}", ctrl),
            Token::Placeholder(placeholder) => write!(f, "{}", placeholder),
        }
    }
}

const PLACEHOLDER_DELIMITER: char = '$';

pub type ParserError<'a> = extra::Err<Rich<'a, char, wst::Span>>;

pub type ParserInput<'a> =
    SpannedInput<char, wst::Span, Stream<std::vec::IntoIter<(char, wst::Span)>>>;

parser_alias!(PParser, ParserInput<'a>, ParserError<'a>);

pub fn input_from_str(wscript: &str) -> ParserInput {
    let eoi = wst::Span {
        start: wscript.len(),
        end: wscript.len() + 1,
    };
    let iter = wscript
        .char_indices()
        .map(|(index, c)| {
            (
                c,
                wst::Span {
                    start: index,
                    end: index + 1,
                },
            )
        })
        .collect::<Vec<_>>()
        .into_iter();

    Stream::from_iter(iter).spanned(eoi)
}

fn placeholder<'src>() -> impl PParser<'src, Token> {
    any()
        .filter(|c: &char| *c == PLACEHOLDER_DELIMITER)
        .then(
            any()
                .filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_')
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .then(any().filter(|c: &char| *c == PLACEHOLDER_DELIMITER))
        .map(|((first, mut mid), last)| {
            let mut combined = Vec::with_capacity(mid.len() + 2);
            combined.push(first);
            combined.append(&mut mid);
            combined.push(last);
            Token::Placeholder(combined.into_iter().collect())
        })
}

fn ident<'src>() -> impl PParser<'src, Token> {
    let valid_chars = any::<ParserInput, _>()
        .filter(|c| c.is_ascii_alphanumeric() || c.is_inline_whitespace() || *c == '-')
        .repeated()
        .collect::<Vec<_>>();

    any::<ParserInput, _>()
        .filter(|c| c.is_ascii_alphabetic())
        .then(valid_chars)
        .map(|(initial, mut following)| {
            let mut combined = Vec::with_capacity(following.len() + 1);
            combined.push(initial);
            combined.append(&mut following);
            Token::Ident(combined.into_iter().collect())
        })
}

pub fn lexer<'src>() -> impl PParser<'src, Vec<(Token, wst::Span)>> {
    let ctrl = one_of("(),").map(Token::Ctrl);

    // TODO Add recover_with(skip_then_retry_until([]))
    let token = choice((ident(), placeholder(), ctrl));

    token
        .padded()
        .map_with_span(|token, span| (token, span))
        .repeated()
        .collect::<Vec<_>>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_iterator;
    use chumsky::prelude::end;

    #[test]
    fn test_placeholders() {
        let placeholders = ["$hello$", "$world$", "$t$", "$10$", "$a_b$"];

        for code in placeholders {
            let actual = placeholder()
                .then_ignore(end())
                .parse(input_from_str(code))
                .into_result()
                .expect(&format!("Cannot parse {code}"));
            assert_eq!(actual, Token::Placeholder(code.into()))
        }
    }

    #[test]
    fn test_wrong_placeholders() {
        let placeholders = ["test", "$$", "$$$", "  ", ""];

        for code in placeholders {
            let actual = placeholder()
                .then_ignore(end())
                .parse(input_from_str(code))
                .into_result();

            assert!(!actual.is_ok(), "'{}' was parsed, but should fail", code)
        }
    }

    #[test]
    fn test_idents() {
        let idents = [
            "Ongoing - Global",
            "Ongoing - EachPlayer",
            "Is Game In Progress",
            "Event Player",
            "Wait",
            "Team 1",
            "Ability 2",
        ];

        for code in idents {
            let actual = ident()
                .then_ignore(end())
                .parse(input_from_str(code))
                .into_result()
                .expect(&format!("Cannot parse {code}"));
            assert_eq!(actual, Token::Ident(code.into()))
        }
    }

    #[test]
    fn test_wrong_idents() {
        let idents = ["- Global", " - Global", " ", "     ", "1.10", "20"];

        for code in idents {
            let actual = ident()
                .then_ignore(end())
                .parse(input_from_str(code))
                .into_result();

            assert!(!actual.is_ok(), "'{}' was parsed, but should fail", code)
        }
    }

    #[test]
    fn test_lexer() {
        let code = "Is Reloading(Event Player, Event Player)";
        let actual: Vec<Token> = lexer()
            .then_ignore(end())
            .parse(input_from_str(code))
            .into_result()
            .expect(&format!("Cannot parse {code}"))
            .into_iter()
            .map(|it| it.0)
            .collect();

        let expected = [
            Token::Ident(SmolStr::new("Is Reloading")),
            Token::Ctrl('('),
            Token::Ident(SmolStr::new("Event Player")),
            Token::Ctrl(','),
            Token::Ident(SmolStr::new("Event Player")),
            Token::Ctrl(')'),
        ];

        assert_iterator!(actual, expected);
    }
}
