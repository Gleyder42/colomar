use crate::compiler::Text;
use chumsky::error::Simple;
use chumsky::prelude::*;
use chumsky::text::Character;
use chumsky::Parser;
use std::fmt::{Display, Formatter};

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Token {
    Ident(Text),
    String(Text),
    Number(Text),
    Placeholder(Text),
    Ctrl(char),
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

fn string() -> impl Parser<char, Token, Error = Simple<char>> {
    just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Text::new)
        .map(Token::String)
}

fn placeholder() -> impl Parser<char, Token, Error = Simple<char>> {
    filter::<char, _, _>(|c| *c == '%')
        .then(
            filter::<char, _, _>(|c| c.is_ascii_alphanumeric() || *c == '_')
                .repeated()
                .at_least(1),
        )
        .then(filter::<char, _, _>(|c| *c == '%'))
        .map(|((first, mut mid), last)| {
            let mut combined = Vec::with_capacity(mid.len() + 2);
            combined.push(first);
            combined.append(&mut mid);
            combined.push(last);
            Token::Placeholder(combined.into_iter().collect())
        })
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

    let token = choice((ident(), placeholder(), ctrl)).recover_with(skip_then_retry_until([]));

    token.padded().repeated()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_iterator;
    use chumsky::prelude::end;

    #[test]
    fn test_placeholders() {
        let placeholders = ["%hello%", "%world%", "%t%", "%10%", "%a_b%"];

        for code in placeholders {
            let actual = placeholder()
                .then_ignore(end())
                .parse(code)
                .expect(&format!("Cannot parse {code}"));
            assert_eq!(actual, Token::Placeholder(code.into()))
        }
    }

    #[test]
    fn test_wrong_placeholders() {
        let placeholders = ["test", "%%", "%%%", "  ", ""];

        for code in placeholders {
            let actual = placeholder().then_ignore(end()).parse(code);

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
