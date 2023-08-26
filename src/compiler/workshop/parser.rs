use crate::compiler::workshop::lexer::Token;
use crate::compiler::wst::partial::Placeholder;
use crate::compiler::wst::{partial, Ident};
use chumsky::error::Error;

use crate::compiler::Text;
use chumsky::prelude::*;
use chumsky::util::Maybe;

pub type ParserExtra<'a> = extra::Err<Rich<'a, Token>>;
pub type ParserInput<'a> = &'a [Token];

#[derive(Debug, Clone)]
enum Name {
    Ident(Ident),
    Placeholder(Placeholder),
}

fn ident<'src>() -> impl Parser<'src, &'src [Token], Ident, ParserExtra<'src>> + Clone {
    any().try_map(|token, span| match token {
        Token::Ident(text) => Ok(Ident(text)),
        _ => {
            let expected = vec![Some(Maybe::from(Token::Ident(Text::from("ident"))))];
            let found = Some(Maybe::from(token));
            let error = <Rich<_, _> as Error<ParserInput>>::expected_found(expected, found, span);
            Err(error)
        }
    })
}

fn placeholder<'src>() -> impl Parser<'src, &'src [Token], Placeholder, ParserExtra<'src>> + Clone {
    any().try_map(|token, span| match token {
        Token::Placeholder(text) => Ok(Placeholder(text)),
        _ => {
            let expected = vec![Some(Maybe::from(Token::Placeholder(Text::from(
                "placeholder",
            ))))];
            let found = Some(Maybe::from(token));
            let error = <Rich<_, _> as Error<ParserInput>>::expected_found(expected, found, span);
            Err(error)
        }
    })
}

pub fn call<'src>() -> impl Parser<'src, &'src [Token], partial::Call, ParserExtra<'src>> {
    recursive::<_, partial::Call, _, _, _>(|call| {
        let args = call
            .separated_by(just(Token::Ctrl(',')))
            .at_least(1)
            .collect::<Vec<_>>()
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

        choice((
            ident().map(Name::Ident),
            placeholder().map(Name::Placeholder),
        ))
        .then(args.or_not())
        .try_map(|(ident, args), span| match (ident, args) {
            (Name::Ident(ident), Some(args)) => Ok(partial::Call::Function(partial::Function {
                name: ident,
                args,
            })),
            (Name::Ident(ident), None) => Ok(partial::Call::Ident(ident)),
            (Name::Placeholder(placeholder), None) => Ok(partial::Call::Placeholder(placeholder)),
            (Name::Placeholder(placeholder), Some(_)) => {
                let expected = vec![Some(Maybe::from(Token::Placeholder(Text::from(
                    "placeholder",
                ))))];
                let found = Some(Maybe::from(Token::from(placeholder)));
                let error =
                    <Rich<_, _> as Error<ParserInput>>::expected_found(expected, found, span);
                Err(error)
            }
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::workshop::lexer::lexer;
    use crate::compiler::Text;

    #[test]
    fn test_element_parser() {
        let code = "Small Message(Event Player, Is Reloading(Event Player))";
        let tokens = lexer().then_ignore(end()).parse(code).unwrap();
        let actual_element = call().then_ignore(end()).parse((&tokens)).unwrap();

        let expected_element = partial::Call::Function(partial::Function {
            name: Ident(Text::new("Small Message")),
            args: vec![
                partial::Call::Ident(Ident(Text::new("Event Player"))),
                partial::Call::Function(partial::Function {
                    name: Ident(Text::new("Is Reloading")),
                    args: vec![partial::Call::Ident(Ident(Text::new("Event Player")))],
                }),
            ],
        });

        assert_eq!(actual_element, expected_element);
    }

    #[test]
    fn test_placeholder() {
        let code = "Set Damage Dealt($caller$, $value$)";
        let tokens = lexer().then_ignore(end()).parse(code).unwrap();
        let actual_element = call().then_ignore(end()).parse(&tokens).unwrap();

        let expected_element = partial::Call::Function(partial::Function {
            name: Ident(Text::new("Set Damage Dealt")),
            args: vec![
                partial::Call::Placeholder(Placeholder(Text::new("$caller$"))),
                partial::Call::Placeholder(Placeholder(Text::new("$value$"))),
            ],
        });

        assert_eq!(actual_element, expected_element);
    }
}
