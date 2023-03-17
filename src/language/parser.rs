extern crate core;

use std::fmt::{Debug};
use chumsky::prelude::*;
use std::string::String;
use crate::language::lexer::Token;
use crate::language::ast::*;

type IdentParser = impl Parser<Token, String, Error=Simple<Token>> + Clone;
type IdentChainParser = impl Parser<Token, Box<Call>, Error=Simple<Token>> + Clone;
type ArgsParser = impl Parser<Token, Vec<Box<Call>>, Error=Simple<Token>> + Clone;
type EventParser = impl Parser<Token, Event, Error=Simple<Token>> + Clone;
type EnumParser = impl Parser<Token, Enum, Error=Simple<Token>> + Clone;
type BlockParser = impl Parser<Token, Block, Error=Simple<Token>> + Clone;
type RuleParser = impl Parser<Token, Rule, Error=Simple<Token>> + Clone;

pub fn ident_parser() -> IdentParser {
    filter_map(|span, token| match token {
        Token::Ident(ident) => Ok(ident.clone()),
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
        .map(|((name, types), default_value)| DeclaredArgument { name, types, default_value })
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
        .map(|(((_, event), decl_args), by)| Event {
            event,
            by,
            args: decl_args,
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
        .map(|((is_native, name), constants)| Enum { name, is_workshop: is_native, constants })
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
            .map(|o| o.into_iter().rfold::<Option<Box<Call>>, _>(None, |acc, element| {
                let call = match element {
                    (name, Some(args)) => Call::Fn { name, args, next: acc },
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
        .map(|o| Block { actions: o.0, conditions: o.1 })
}

pub fn rule_parser(
    ident: IdentParser,
    block: BlockParser,
    args: ArgsParser,
) -> RuleParser {
    let rule_name = filter_map(|span, token| match token {
        Token::String(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    just(Token::Rule)
        .ignore_then(rule_name)
        .then(ident)
        .then(args.clone())
        .then(block)
        .map(|(((rule_name, ident), args), block)| Rule {
            conditions: block.conditions,
            actions: block.actions,
            name: rule_name,
            event: ident,
            args,
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
        .map_with_span(|it, span| (it, span))
        .repeated()
        .then_ignore(end())
        .map(|p| Ast(p))
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, Stream};
    use std::fs::{read_to_string};
    use once_cell::sync::Lazy;
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
                name: "Hero".to_string(),
                constants: vec![
                    "Reaper".to_string(),
                    "Tracer".to_string(),
                    "Mercy".to_string(),
                ],
            },
            Enum {
                is_workshop: false,
                name: "MyEnum".to_string(),
                constants: vec![
                    "First".to_string(),
                    "Second".to_string(),
                ],
            },
        ];

        actual_enums.into_iter()
            .zip(expected)
            .for_each(|(actual, expected)| {
                assert_eq!(actual.name, expected.name);
                assert_eq!(actual.is_workshop, expected.is_workshop);
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
                event: "OngoingEachPlayer".to_string(),
                by: None,
                args: vec![
                    DeclaredArgument {
                        name: "team".to_string(),
                        types: vec!["Team".to_string()],
                        default_value: None,
                    },
                    DeclaredArgument {
                        name: "heroSlot".to_string(),
                        types: vec!["Hero".to_string(), "Slot".to_string()],
                        default_value: None,
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
                assert_vec(&actual.args, &expected.args);
            })
    }

    #[test]
    fn test_rule_header() {
        let actual_rules = read(&RULE_HEADER);

        let expected: Vec<Rule> = vec![
            Rule {
                name: "Heal on Kill".to_string(),
                event: "OngoingPlayer".to_string(),
                args: Vec::new(),
                conditions: Vec::new(),
                actions: Vec::new(),
            },
            Rule {
                name: "Test".to_string(),
                event: "MyEvent".to_string(),
                args: vec![
                    Call::new_var("Hello"),
                    Call::new_var("World"),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
            },
            Rule {
                name: "Heal on Kill".to_string(),
                event: "PlayerDealtFinalBlow".to_string(),
                args: vec![
                    Call::new_var("Team1"),
                    Call::new_var("Slot1"),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
            },
            Rule {
                name: "Heal on Kill".to_string(),
                event: "PlayerDealtFinalBlow".to_string(),
                args: vec![
                    Call::new_fn("Team1"),
                    Call::new_fn("Slot1"),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
            },
            Rule {
                name: "Do something".to_string(),
                event: "HelloWorld".to_string(),
                args: vec![
                    Call::new_fn_args("test", vec![Call::new_var("Team1")]),
                    Call::new_fn_args("foo", vec![Call::new_var("Slot1")]),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
            },
            Rule {
                name: "Complex".to_string(),
                event: "HelloWorld".to_string(),
                args: vec![
                    Call::new_var_next(
                        "foo",
                        Call::new_fn_args_next(
                            "bar",
                            vec![Call::new_var("hello")],
                            Call::new_var("nice"),
                        ),
                    ),
                    Call::new_fn_args_next(
                        "fn",
                        vec![
                            Call::new_var("e"),
                            Call::new_var("o"),
                        ],
                        Call::new_fn_args(
                            "foo",
                            vec![
                                Call::new_var("x"),
                                Call::new_var("p"),
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
                        assert_vec(&actual.args, &expected.args);
                        assert_vec(&actual.conditions, &expected.conditions);
                        assert_vec(&actual.actions, &expected.actions);
                    }
                    _ => assert!(false, "{:?} and {:?} do not have the same type", actual, expected)
                }
            })
    }
}