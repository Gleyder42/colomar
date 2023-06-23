use chumsky::prelude::*;
use crate::language::workshop::lexer::Token;
use crate::language::workshop::tree::{Element, Function, Ident};

type ParserError = Simple<Token>;

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

fn element() -> impl Parser<Token, Element, Error = ParserError> {
    recursive::<_, Element, _, _, _>(|call| {
        let args = call
            .separated_by(just(Token::Ctrl(',')))
            .at_least(1)
            .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

        ident().then(args.or_not())
            .map(|(ident, args)| {
                match (ident, args) {
                    (ident, Some(args)) => Element::Function(Function { name: ident, args}),
                    (ident, None) => Element::Ident(ident)
                }
            })
    })
}

#[cfg(test)]
mod tests {
    use crate::language::Text;
    use crate::language::workshop::lexer::lexer;
    use super::*;

    #[test]
    fn test_element_parser() {
        let code = "Small Message(Event Player, Is Reloading(Event Player))";
        let tokens = lexer().then_ignore(end()).parse(code).unwrap();
        let actual_element = element().then_ignore(end()).parse(tokens).unwrap();

        let expected_element = Element::Function(Function {
            name: Ident(Text::new("Small Message")),
            args: vec![
                Element::Ident(Ident(Text::new("Event Player"))),
                Element::Function(Function {
                    name: Ident(Text::new("Is Reloading")),
                    args: vec![Element::Ident(Ident(Text::new("Event Player")))]
                })
            ]
        });

        assert_eq!(actual_element, expected_element);
    }
}