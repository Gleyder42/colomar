extern crate core;

use std::fmt::{Debug};
use chumsky::prelude::*;
use std::string::String;
use crate::language::lexer::Token;
use crate::language::ast::*;

type IdentParser = impl Parser<Token, Ident, Error=Simple<Token>> + Clone;
type IdentChainParser = impl Parser<Token, Box<Call>, Error=Simple<Token>> + Clone;
type ArgsParser = impl Parser<Token, Vec<Box<Call>>, Error=Simple<Token>> + Clone;
type EventParser = impl Parser<Token, Event, Error=Simple<Token>> + Clone;
type EnumParser = impl Parser<Token, Enum, Error=Simple<Token>> + Clone;
type BlockParser = impl Parser<Token, Block, Error=Simple<Token>> + Clone;
type RuleParser = impl Parser<Token, Rule, Error=Simple<Token>> + Clone;

pub fn ident_parser() -> IdentParser {
    filter_map(|span, token| match token {
        Token::Ident(ident) => Ok(Ident(ident.clone(), span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    })
}

fn event_parser(
    ident: IdentParser,
    ident_chain: IdentChainParser,
    args: ArgsParser,
) -> EventParser {
    let declare_args = ident.clone()
        .then_ignore(just(Token::Ctrl(':')))
        .then(ident.clone().separated_by(just(Token::Ctrl('|'))))
        .then(just(Token::Ctrl('=')).ignore_then(ident_chain).or_not())
        .map_with_span(|((name, types), default_value), span | DeclaredArgument { name, types, default_value, span })
        .separated_by(just(Token::Ctrl(',')))
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

    just(Token::Workshop)
        .or_not()
        .map(|o| o.is_some())
        .then_ignore(just(Token::Event))
        .then(ident.clone())
        .then(declare_args)
        .then(just(Token::By).ignore_then(ident).then(args).or_not())
        .then_ignore(just(Token::Ctrl('{')))
        .then_ignore(just(Token::Ctrl('}')))
        .validate(|it, span, emit| {
            if it.0.0.0 && it.1.is_some() {
                emit(Simple::custom(span, "Workshop functions cannot have a by clause"));
            }
            it
        })
        .map_with_span(|(((_, event), decl_args), by), span| Event {
            event,
            by,
            args: decl_args,
            span
        })
}

fn enum_parser(
    ident: IdentParser
) -> EnumParser {
    let constants = ident.clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing();

    just(Token::Workshop)
        .or_not()
        .map(|o| o.is_some())
        .then_ignore(just(Token::Enum))
        .then(ident.clone())
        .then(constants.delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))))
        .map_with_span(|((is_native, name), constants), span| Enum { name, is_workshop: is_native, constants, span })
}

fn ident_chain_parser(
    ident: IdentParser
) -> (
    IdentChainParser,
    ArgsParser
) {
    let mut ident_chain = Recursive::<_, Box<Call>, _>::declare();
    let args = ident_chain
        .clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .labelled("function args");

    ident_chain.define(
        ident.then(args.clone().or_not())
            .separated_by(just(Token::Ctrl('.')))
            .at_least(1)
            .map_with_span(|o, span| o.into_iter().rfold::<Option<Box<Call>>, _>(None, |acc, element| {
                let call = match element {
                    (name, Some(args)) => Call::Fn { name, args, next: acc, span: span.clone() },
                    (name, None) => Call::Var { name, next: acc }
                };
                return Some(Box::new(call));
            }).expect("Cannot have call chain with no calls")));

    (ident_chain, args)
}

fn block_parser(
    ident_chain: IdentChainParser
) -> BlockParser {
    let cond = just(Token::Cond)
        .ignore_then(ident_chain.clone())
        .then_ignore(just(Token::Ctrl(';')))
        .map(|o| o);

    just(Token::Ctrl('{'))
        .ignore_then(cond.repeated())
        .then(ident_chain.clone()
            .then_ignore(just(Token::Ctrl(';')))
            .map(|o| o as Action)
            .repeated()
        )
        .then_ignore(just(Token::Ctrl('}')))
        .map_with_span(|o, span| Block { actions: o.0, conditions: o.1, span })
}

pub fn rule_parser(
    ident: IdentParser,
    block: BlockParser,
    args: ArgsParser,
) -> RuleParser {
    let rule_name = filter_map(|span, token| match token {
        Token::String(string) => Ok((string.clone(), span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    just(Token::Rule)
        .ignore_then(rule_name)
        .then(ident)
        .then(args.clone())
        .then(block)
        .map_with_span(|(((rule_name, ident), args), block), span| Rule {
            conditions: block.conditions,
            actions: block.actions,
            name: rule_name,
            event: ident,
            args,
            span
        })
}

pub fn parser() -> impl Parser<Token, Ast, Error=Simple<Token>> {
    let ident = ident_parser();
    let (ident_chain, args) = ident_chain_parser(ident.clone());
    let block = block_parser(ident_chain.clone());

    let rule_parser = rule_parser(ident.clone(), block.clone(), args.clone())
        .map(Root::Rule);
    let event_parser = event_parser(ident.clone(), ident_chain.clone(), args)
        .map(Root::Event);
    let enum_parser = enum_parser(ident.clone())
        .map(Root::Enum);

    choice((rule_parser, event_parser, enum_parser))
        .repeated()
        .then_ignore(end())
        .map(|p| Ast(p))
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, Stream};
    use std::fs::{read_to_string};
    use once_cell::sync::Lazy;
    use crate::language::ast::Ident;
    use crate::language::lexer::lexer;
    use crate::language::parser::{Call, DeclaredArgument, Enum, Event, parser, Rule, Root, Ast};
    use crate::test_assert::{assert_vec};

    static RULE_HEADER: Lazy<String> = Lazy::new(|| read_to_string("resources/tests/snippets/rule_header.colo").unwrap());
    static EVENT_DECL_HEADER: Lazy<String> = Lazy::new(|| read_to_string("resources/tests/snippets/rule_decl.colo").unwrap());
    static ENUM: Lazy<String> = Lazy::new(|| read_to_string("resources/tests/snippets/enum.colo").unwrap());

    fn read(file: &Lazy<String>) -> Ast {
        let tokens = lexer().parse(file.as_str()).unwrap();
        let stream = Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter());
        parser().parse(stream).unwrap()
    }

    #[test]
    fn test_enum() {
        let actual_enums: Vec<_> = read(&ENUM).0.into_iter()
            .filter_map(|decl| match decl {
                Root::Enum(my_enum) => Some(my_enum),
                _ => None
            }).collect();

        let expected: Vec<Enum> = vec![
            Enum {
                is_workshop: true,
                name: Ident("Hero".to_string(), 14..18),
                constants: vec![
                    Ident("Reaper".to_string(), 26..32),
                    Ident("Tracer".to_string(), 39..45),
                    Ident("Mercy".to_string(), 52..57),
                ],
                span: 0..60
            },
            Enum {
                is_workshop: false,
                name: Ident("MyEnum".to_string(), 69..75),
                constants: vec![
                    Ident("First".to_string(), 83..88),
                    Ident("Second".to_string(), 90..96),
                ],
                span: 64..99
            },
        ];

        actual_enums.into_iter()
            .zip(expected)
            .for_each(|(actual, expected)| {
                assert_eq!(actual.name, expected.name);
                assert_eq!(actual.is_workshop, expected.is_workshop);
                assert_eq!(actual.span, expected.span);
                assert_vec(&actual.constants, &expected.constants);
            })
    }

    #[test]
    fn test_event_decl() {
        let actual_events: Vec<_> = read(&EVENT_DECL_HEADER).0.into_iter()
            .filter_map(|o| match o {
                Root::Event(event) => Some(event),
                _ => None,
            })
            .collect();

        let expected: Vec<Event> = vec![
            Event {
                event: Ident("OngoingEachPlayer".to_string(), 15..32),
                by: None,
                span: 0..71,
                args: vec![
                    DeclaredArgument {
                        name: Ident("team".to_string(), 33..37),
                        types: vec![Ident("Team".to_string(), 39..43)],
                        default_value: None,
                        span: 33..43
                    },
                    DeclaredArgument {
                        name: Ident(
                            "heroSlot".to_string(),
                            45..53,
                        ),
                        types: vec![Ident(
                                    "Hero".to_string(),
                                    55..59,
                                ), Ident(
                            "Slot".to_string(),
                            62..66,
                        )],
                        default_value: None,
                        span: 45..66,
                    },
                ],
            }
        ];

        assert_eq!(actual_events.len(), expected.len(),
                   "Test if actual rules length is equal to expected length");

        actual_events.into_iter()
            .zip(expected)
            .for_each(|(actual, expected)| {
                assert_eq!(actual.event, expected.event);
                assert_eq!(actual.by, expected.by);
                assert_eq!(actual.span, expected.span);
                assert_vec(&actual.args, &expected.args);
            })
    }

    #[test]
    fn test_rule_header() {
        let actual_rules = read(&RULE_HEADER);

        let expected: Vec<Rule> = vec![
            Rule {
                name: ("Heal on Kill".to_string(), 5..19),
                event: Ident("OngoingPlayer".to_string(), 20..33),
                args: Vec::new(),
                conditions: Vec::new(),
                actions: Vec::new(),
                span: 0..39
            },
            Rule {
                name: ("Test".to_string(), 48..54),
                event: Ident("MyEvent".to_string(), 55..62),
                args: vec![
                    Call::new_var(Ident("Hello".to_string(), 63..68)),
                    Call::new_var(Ident("World".to_string(), 70..75)),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
                span: 43..80
            },
            Rule {
                name: ("Heal on Kill".to_string(), 89..103),
                event: Ident("PlayerDealtFinalBlow".to_string(), 104..124),
                args: vec![
                    Call::new_var(Ident("Team1".to_string(), 125..130)),
                    Call::new_var(Ident("Slot1".to_string(), 132..137)),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
                span: 84..142
            },
            Rule {
                name: ("Heal on Kill".to_string(), 151..165),
                event: Ident("PlayerDealtFinalBlow".to_string(), 166..186),
                args: vec![
                    Call::new_fn(Ident("Team1".to_string(), 187..192), 187..194),
                    Call::new_fn(Ident("Slot1".to_string(), 196..201), 196..203),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
                span: 146..208
            },
            Rule {
                name: ("Do something".to_string(), 217..231),
                span: 212..271,
                event: Ident("HelloWorld".to_string(), 232..242),
                args: vec![
                    Call::new_fn_args(
                        Ident("test".to_string(), 243..247),
                        243..254,
                        vec![Call::new_var(Ident("Team1".to_string(), 248..253))]
                    ),
                    Call::new_fn_args(
                        Ident("foo".to_string(), 256..259),
                        256..266,
                        vec![Call::new_var(Ident("Slot1".to_string(), 260..265))]
                    ),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
            },
            Rule {
                name: ("Complex".to_string(), 280..289),
                span: 275..345,
                event: Ident("HelloWorld".to_string(), 290..300),
                args: vec![
                    Call::new_var_next(
                        Ident("foo".to_string(), 301..304),
                        Call::new_fn_args_next(
                            Ident("bar".to_string(), 305..308),
                            301..320,
                            vec![Call::new_var(Ident("hello".to_string(), 309..314))],
                            Call::new_var(Ident("nice".to_string(), 316..320)),
                        ),
                    ),
                    Call::new_fn_args_next(
                        Ident("fn".to_string(), 322..324),
                        322..340,
                        vec![
                            Call::new_var(Ident("e".to_string(), 325..326)),
                            Call::new_var(Ident("o".to_string(), 328..329)),
                        ],
                        Call::new_fn_args(
                            Ident("foo".to_string(), 331..334),
                            322..340,
                            vec![
                                Call::new_var(Ident("x".to_string(), 335..336)),
                                Call::new_var(Ident("p".to_string(), 338..339, )),
                            ],
                        ),
                    ),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
            },
        ];

        assert_eq!(actual_rules.0.len(), expected.len(),
                   "Test if actual rules length is equal to expected length");

        actual_rules.0.into_iter()
            .zip(expected)
            .for_each(|(actual, expected)| {
                match actual {
                    Root::Rule(actual) => {
                        assert_eq!(actual.name, expected.name,
                                   "Test if {:?} is equal to {:?}", actual.name, expected.name);
                        assert_eq!(actual.event, expected.event,
                                   "Test if {:?} is equal to {:?}", actual.event, expected.event);
                        assert_eq!(actual.span, expected.span);
                        assert_vec(&actual.args, &expected.args);
                        assert_vec(&actual.conditions, &expected.conditions);
                        assert_vec(&actual.actions, &expected.actions);
                    }
                    _ => assert!(false, "{:?} and {:?} do not have the same type", actual, expected)
                }
            })
    }
}