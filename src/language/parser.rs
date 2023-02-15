extern crate core;

use std::fmt::{Debug};
use std::process::id;
use chumsky::prelude::*;
use std::string::String;
use crate::language::lexer::Token;

pub type Action = Box<Call>;
pub type Condition = Box<Call>;
pub type CallArgs = Vec<Box<Call>>;


#[derive(Debug)]
pub enum TopLevelDecl {
    Event(Event),
    Rule(Rule),
    Enum(Enum)
}

#[derive(Debug)]
pub struct Event {
    event: String,
    by: Option<(String, Vec<Box<Call>>)>,
    args: Vec<DeclaredArgument>
}

#[derive(Debug)]
pub struct Enum {
    is_native: bool,
    name: String,
    constants: Vec<String>
}

impl Event {

    pub fn is_native(&self) -> bool {
        self.by.is_none()
    }
}

#[derive(Debug, PartialEq)]
struct DeclaredArgument {
    name: String,
    types: Vec<String>,
    default_value: Option<Box<Call>>
}

#[derive(Debug)]
pub struct Rule {
    pub name: String,
    pub event: String,
    pub args: Vec<Box<Call>>,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>,
}

#[derive(Debug)]
pub struct Block {
    pub actions: Vec<Action>,
    pub conditions: Vec<Condition>,
}

#[derive(Debug, PartialEq)]
pub enum Call {
    Fn {
        name: String,
        args: CallArgs,
        next: Option<Box<Call>>,
    },
    Var {
        name: String,
        next: Option<Box<Call>>,
    },
}

impl Call {
    pub fn new_var(name: impl Into<String>) -> Box<Self> {
        Box::new(Call::Var { name: name.into(), next: None })
    }

    pub fn new_var_next(name: impl Into<String>, next: Box<Call>) -> Box<Self> {
        Box::new(Call::Var { name: name.into(), next: Some(next) })
    }

    pub fn new_fn(name: impl Into<String>) -> Box<Self> {
        Box::new(Call::Fn { name: name.into(), args: Vec::new(), next: None })
    }

    pub fn new_fn_next(name: impl Into<String>, next: Box<Call>) -> Box<Self> {
        Box::new(Call::Fn { name: name.into(), args: Vec::new(), next: Some(next) })
    }

    pub fn new_fn_args(name: impl Into<String>, args: CallArgs) -> Box<Self> {
        Box::new(Call::Fn { name: name.into(), args, next: None })
    }

    pub fn new_fn_args_next(name: impl Into<String>, args: CallArgs, next: Box<Call>) -> Box<Self> {
        Box::new(Call::Fn { name: name.into(), args, next: Some(next) })
    }
}

impl CallName for Call {
    fn name(&self) -> &String {
        match self {
            Call::Fn { name, args: _, next: _ } => &name,
            Call::Var { name, next: _ } => &name
        }
    }
}

pub trait CallName {
    fn name(&self) -> &String;
}

pub fn ident_parser() -> impl Parser<Token, String, Error=Simple<Token>>  + Clone {
    filter_map(|span, token| match token {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    })
}

fn event_parser(
    ident: impl Parser<Token, String, Error=Simple<Token>> + Clone + 'static,
    ident_chain: impl Parser<Token, Box<Call>, Error=Simple<Token>> + Clone + 'static,
    args: impl Parser<Token, Vec<Box<Call>>, Error=Simple<Token>> + Clone + 'static
) -> impl Parser<Token, Event, Error=Simple<Token>> + Clone {
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
        .map(|(((is_native, event), decl_args), by) | Event {
            event, by, args: decl_args
        })
}

fn enum_parser(
    ident: impl Parser<Token, String, Error=Simple<Token>> + Clone + 'static
) -> impl Parser<Token, Enum, Error=Simple<Token>> + Clone {
    let constants = ident.clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing();

    just(Token::Workshop)
        .or_not()
        .map(|o| o.is_some())
        .then_ignore(just(Token::Enum))
        .then(ident.clone())
        .then(constants.delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))))
        .map(|((is_native, name), constants)| Enum { name, is_native, constants })
}

fn ident_chain_parser(
    ident: impl Parser<Token, String, Error=Simple<Token>> + 'static
) -> (
    impl Parser<Token, Box<Call>, Error=Simple<Token>> + Clone,
    impl Parser<Token, Vec<Box<Call>>, Error=Simple<Token>> + Clone
) {
    let mut ident_chain = Recursive::<_, Box<Call>, _>::declare();
    let args = ident_chain
        .clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(just(Token::Ctrl('('),), just(Token::Ctrl(')')))
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
    ident_chain: impl Parser<Token, Box<Call>, Error=Simple<Token>> + Clone + 'static
) -> impl Parser<Token, Block, Error=Simple<Token>> + Clone + 'static {
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
    ident: impl Parser<Token, String, Error=Simple<Token>> + Clone + 'static,
    block: impl Parser<Token, Block, Error=Simple<Token>> + Clone+ 'static,
    args: impl Parser<Token, Vec<Box<Call>>, Error=Simple<Token>> + Clone + 'static
) -> impl Parser<Token, Rule, Error=Simple<Token>> + Clone {
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

pub fn parser() -> impl Parser<Token, Vec<TopLevelDecl>, Error=Simple<Token>> {
    let ident = ident_parser();
    let (ident_chain, args) = ident_chain_parser(ident.clone());
    let block = block_parser(ident_chain.clone());

    let rule_parser = rule_parser(ident.clone(), block.clone(), args.clone())
        .map(TopLevelDecl::Rule);
    let event_parser = event_parser(ident.clone(), ident_chain.clone(), args)
        .map(TopLevelDecl::Event);
    let enum_parser = enum_parser(ident.clone())
        .map(TopLevelDecl::Enum);

    choice((rule_parser, event_parser, enum_parser))
        .repeated()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, Stream};
    use std::fs::{read_to_string};
    use once_cell::sync::Lazy;
    use crate::language::lexer::lexer;
    use crate::language::parser::{Call, DeclaredArgument, Enum, Event, parser, Rule, TopLevelDecl};
    use crate::test_assert::{assert_vec, compare_vec};

    static RULE_HEADER: Lazy<String> = Lazy::new(|| read_to_string("snippets/rule_header.colo").unwrap());
    static EVENT_DECL_HEADER: Lazy<String> = Lazy::new(|| read_to_string("snippets/rule_decl.colo").unwrap());
    static ENUM: Lazy<String> = Lazy::new(|| read_to_string("snippets/enum.colo").unwrap());

    fn read(file: &Lazy<String>) -> Vec<TopLevelDecl> {
        let tokens = lexer().parse(file.as_str()).unwrap();
        let stream = Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter());
        parser().parse(stream).unwrap()
    }

    #[test]
    fn test_enum() {
        let actual_enums: Vec<_> = read(&ENUM).into_iter()
            .filter_map(|decl| match decl {
                TopLevelDecl::Enum(myEnum) => Some(myEnum),
                _ => None
            }).collect();

        let expected: Vec<Enum> = vec![
            Enum {
                is_native: true,
                name: "Hero".to_string(),
                constants: vec![
                    "Reaper".to_string(),
                    "Tracer".to_string(),
                    "Mercy".to_string()
                ]
            },
            Enum {
                is_native: false,
                name: "MyEnum".to_string(),
                constants: vec![
                    "First".to_string(),
                    "Second".to_string()
                ]
            }
        ];

        actual_enums.into_iter()
            .zip(expected)
            .for_each(|(actual, expected)| {
                assert_eq!(actual.name, expected.name);
                assert_eq!(actual.is_native, expected.is_native);
                assert_vec(&actual.constants, &expected.constants);
            })
    }

    #[test]
    fn test_event_decl() {
        let actual_events: Vec<_> = read(&EVENT_DECL_HEADER).into_iter()
            .filter_map(|o| match o {
                TopLevelDecl::Event(event) => Some(event),
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
                        default_value : None
                    },
                    DeclaredArgument {
                        name: "heroSlot".to_string(),
                        types: vec!["Hero".to_string(), "Slot".to_string()],
                        default_value: None
                    }
                ]
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
                            Call::new_var("nice")
                        )
                    ),
                    Call::new_fn_args_next(
                        "fn",
                        vec![
                            Call::new_var("e"),
                            Call::new_var("o")
                        ],
                        Call::new_fn_args(
                            "foo",
                            vec![
                                Call::new_var("x"),
                                Call::new_var("p")
                            ]
                        )
                    )
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
            },
        ];

        assert_eq!(actual_rules.len(), expected.len(),
                   "Test if actual rules length is equal to expected length");

        actual_rules.into_iter()
            .zip(expected)
            .for_each(|(actual, expected)| {
                match actual {
                    TopLevelDecl::Rule(actual) => {
                        assert_eq!(actual.name, expected.name,
                                   "Test if {:?} is equal to {:?}", actual.name, expected.name);
                        assert_eq!(actual.event, expected.event,
                                   "Test if {:?} is equal to {:?}", actual.event, expected.event);
                        assert!(compare_vec(&actual.args, &expected.args),
                                "Test if {:?} is equal to {:?}", actual.args, expected.args);
                        assert!(compare_vec(&actual.conditions, &expected.conditions),
                                "Test if {:?} is equal to {:?}", actual.conditions, expected.conditions);
                        assert!(compare_vec(&actual.actions, &expected.actions),
                                "Test if {:?} is equal to {:?}", actual.actions, expected.actions);
                    },
                    _ => assert!(false, "{:?} and {:?} do not have the same type", actual, expected)
                }

            })
    }
}