use crate::language::Text;
use chumsky::error::Simple;
use chumsky::prelude::*;
use chumsky::text::Character;
use chumsky::{text, Parser};
use std::fmt::{Display, Formatter};

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Token {
    Ident(Text),
    String(Text),
    Number(Text),
    Ctrl(char),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(text) => write!(f, "Ident {}", text),
            Token::String(text) => write!(f, "String {}", text),
            Token::Number(text) => write!(f, "Number {}", text),
            Token::Ctrl(ctrl) => write!(f, "{}", ctrl)
        }
    }
}

fn string() -> impl Parser<char, Token, Error = Simple<char>> {
    just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Text::new)
        .map(Token::String)
}

fn ident() -> impl Parser<char, Token, Error = Simple<char>> {
    let valid_chars = filter::<char, _, _>(|c| {
        c.is_ascii_alphanumeric() || c.is_inline_whitespace() || *c == '-'
    })
    .repeated();

    filter::<char, _, _>(|c| c.is_ascii_alphabetic())
        .then(valid_chars)
        .map(|(initial, mut following)| {
            let mut combined = Vec::with_capacity(following.len() + 1);
            combined.push(initial);
            combined.append(&mut following);
            let text: Text = combined.into_iter().collect();
            Token::Ident(text)
        })
}

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let ctrl = one_of("(),").map(Token::Ctrl);

    let token = choice((ident(), ctrl)).recover_with(skip_then_retry_until([]));

    token
        .padded()
        .repeated()
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::prelude::end;
    use crate::assert_iterator;

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
                .parse(code)
                .expect(&format!("Cannot parse {code}"));
            assert_eq!(actual, Token::Ident(code.into()))
        }
    }

    #[test]
    fn test_wrong_idents() {
        let idents = ["- Global", " - Global", " ", "     ", "1.10", "20"];

        for code in idents {
            let actual = ident().then_ignore(end()).parse(code);

            assert!(!actual.is_ok(), "'{}' was parsed, but should fail", code)
        }
    }

    #[test]
    fn test_lexer() {
        let code = "Is Reloading(Event Player, Event Player)";
        let actual = lexer()
            .then_ignore(end())
            .parse(code)
            .expect(&format!("Cannot parse {code}"));

        let expected = [
            Token::Ident(Text::new("Is Reloading")),
            Token::Ctrl('('),
            Token::Ident(Text::new("Event Player")),
            Token::Ctrl(','),
            Token::Ident(Text::new("Event Player")),
            Token::Ctrl(')'),
        ];

        assert_iterator!(actual, expected);
    }
}
