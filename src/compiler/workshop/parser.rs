use crate::compiler::workshop::lexer::Token;
use crate::compiler::wst::{partial, Ident};
use chumsky::prelude::*;

pub type ParserError = Simple<Token>;

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

pub fn call() -> impl Parser<Token, partial::Call, Error = ParserError> {
    recursive::<_, partial::Call, _, _, _>(|call| {
        let args = call
            .separated_by(just(Token::Ctrl(',')))
            .at_least(1)
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

        ident()
            .then(args.or_not())
            .map(|(ident, args)| match (ident, args) {
                (ident, Some(args)) => {
                    partial::Call::Function(partial::Function { name: ident, args })
                }
                (ident, None) => partial::Call::Ident(ident),
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
