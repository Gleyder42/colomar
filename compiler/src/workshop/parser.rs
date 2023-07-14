use crate::workshop::lexer::Token;
use crate::wst::partial::Placeholder;
use crate::wst::{partial, Ident};

use chumsky::prelude::*;

pub type ParserError = Simple<Token>;

enum Name {
    Ident(Ident),
    Placeholder(Placeholder),
}

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

fn placeholder() -> impl Parser<Token, Placeholder, Error = ParserError> {
    filter_map(|span, token| match token {
        Token::Placeholder(text) => Ok(Placeholder(text)),
        _ => Err(ParserError::expected_input_found(
            span,
            Vec::new(),
            Some(token),
        )),
    })
}

pub fn call() -> impl Parser<Token, partial::Call, Error = ParserError> {
    recursive::<_, partial::Call, _, _, _>(|call| {
        let args = call
            .separated_by(just(Token::Ctrl(',')))
            .at_least(1)
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
            (Name::Placeholder(placeholder), Some(_)) => Err(ParserError::expected_input_found(
                span,
                Vec::new(),
                Some(Token::Placeholder(placeholder.0)),
            )),
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::workshop::lexer::lexer;
    use crate::Text;

    #[test]
    fn test_element_parser() {
        let code = "Small Message(Event Player, Is Reloading(Event Player))";
        let tokens = lexer().then_ignore(end()).parse(code).unwrap();
        let actual_element = call().then_ignore(end()).parse(tokens).unwrap();

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
}
