extern crate core;

use std::fmt::{Debug};
use chumsky::prelude::*;
use std::string::String;
use crate::language::lexer::Token;

pub type Action = Box<Call>;
pub type Condition = Box<Call>;
pub type Args = Vec<Box<Call>>;

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
        args: Args,
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

    pub fn new_fn_args(name: impl Into<String>, args: Args) -> Box<Self> {
        Box::new(Call::Fn { name: name.into(), args, next: None })
    }

    pub fn new_fn_args_next(name: impl Into<String>, args: Args, next: Box<Call>) -> Box<Self> {
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

pub fn rule_parser() -> impl Parser<Token, Vec<Rule>, Error=Simple<Token>> {
    let rule_name = filter_map(|span, token| match token {
        Token::String(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    let ident = filter_map(|span, token| match token {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    let mut ident_chain = Recursive::<_, Box<Call>, _>::declare();

    let args = ident_chain
        .clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(Token::Ctrl('('), Token::Ctrl(')'))
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

    let cond = just(Token::Cond)
        .ignore_then(ident_chain.clone())
        .then_ignore(just(Token::Ctrl(';')))
        .map(|o| o);

    let block = just(Token::Ctrl('{'))
        .ignore_then(cond.repeated())
        .then(ident_chain.clone()
            .then_ignore(just(Token::Ctrl(';')))
            .map(|o| o as Action)
            .repeated()
        )
        .then_ignore(just(Token::Ctrl('}')))
        .map(|o| Block { actions: o.0, conditions: o.1 });

    let rule = just(Token::Rule)
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
        });

    rule.repeated()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, Stream};
    use std::fs::{read_to_string};
    use once_cell::sync::Lazy;
    use crate::language::lexer::lexer;
    use crate::language::parser::{Call, Rule, rule_parser};
    use crate::test_assert::compare_vec;

    static RULE_HEADER: Lazy<String> = Lazy::new(|| read_to_string("snippets/rule_header.colo").unwrap());


    #[test]
    fn test_rule_header() {
        let tokens = lexer().parse(RULE_HEADER.as_str()).unwrap();
        let stream = Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter());
        let rules = rule_parser().parse(stream).unwrap();

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

        assert_eq!(rules.len(), expected.len(),
                   "Test if actual rules length is equal to expected length");

        rules.into_iter()
            .zip(expected)
            .for_each(|(actual, expected)| {
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
            })
    }
}