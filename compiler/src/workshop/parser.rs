use super::super::workshop::lexer::Token;
use super::super::wst::partial::Placeholder;
use super::super::wst::{partial, Ident};
use chumsky::error::Error;
use chumsky::input::{SpannedInput, Stream};

use crate::{parser_alias, span};
use chumsky::prelude::*;
use chumsky::util::Maybe;
use smol_str::SmolStr;

#[derive(Debug, Clone)]
enum Name {
    Ident(Ident),
    Placeholder(Placeholder),
}

pub type ParserExtra<'a> = extra::Err<Rich<'a, Token, span::Span>>;
pub type ParserInput =
    SpannedInput<Token, span::Span, Stream<std::vec::IntoIter<(Token, span::Span)>>>;

parser_alias!(PParser, ParserInput, ParserExtra<'a>);

fn ident<'src>() -> impl PParser<'src, Ident> + Clone {
    select! {
        Token::Ident(text) => Ident(text)
    }
}

fn placeholder<'src>() -> impl PParser<'src, Placeholder> + Clone {
    select! {
        Token::Placeholder(text) => Placeholder(text)
    }
}

pub fn call<'src>() -> impl PParser<'src, partial::Call> {
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
                let expected = vec![Some(Maybe::from(Token::Placeholder(SmolStr::from(
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
    use crate::span::{Offset, SpanSourceId};
    use crate::workshop::lexer::{input_from_str, lexer};
    use salsa::InternId;
    use salsa::InternKey;

    #[test]
    fn test_element_parser() {
        let id = SpanSourceId::from_intern_id(InternId::from(1_u32));
        let code = "Small Message(Event Player, Is Reloading(Event Player))";
        let tokens = lexer()
            .then_ignore(end())
            .parse(input_from_str(code, id))
            .unwrap();

        let eoi = span::Span {
            context: id,
            offset: Offset::from(tokens.len()..tokens.len() + 1),
        };
        let stream = Stream::from_iter(tokens.into_iter()).spanned(eoi);

        let actual_element = call().then_ignore(end()).parse(stream).unwrap();

        let expected_element = partial::Call::Function(partial::Function {
            name: Ident::from("Small Message"),
            args: vec![
                partial::Call::Ident(Ident::from("Event Player")),
                partial::Call::Function(partial::Function {
                    name: Ident::from("Is Reloading"),
                    args: vec![partial::Call::Ident(Ident::from("Event Player"))],
                }),
            ],
        });

        assert_eq!(actual_element, expected_element);
    }

    #[test]
    fn test_placeholder() {
        let id = SpanSourceId::from_intern_id(InternId::from(1_u32));
        let code = "Set Damage Dealt($caller$, $value$)";
        let tokens = lexer()
            .then_ignore(end())
            .parse(input_from_str(code, id))
            .unwrap();

        let eoi = span::Span {
            context: id,
            offset: Offset::from(tokens.len()..tokens.len() + 1),
        };
        let stream = Stream::from_iter(tokens.into_iter()).spanned(eoi);
        let actual_element = call().then_ignore(end()).parse(stream).unwrap();

        let expected_element = partial::Call::Function(partial::Function {
            name: Ident::from("Set Damage Dealt"),
            args: vec![
                partial::Call::Placeholder(Placeholder::from("caller")),
                partial::Call::Placeholder(Placeholder::from("value")),
            ],
        });

        assert_eq!(actual_element, expected_element);
    }
}
