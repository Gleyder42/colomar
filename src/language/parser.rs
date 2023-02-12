extern crate core;

use std::fmt::{Debug};
use chumsky::prelude::*;
use std::string::String;
use crate::language::lexer::Token;

pub type Action = CallChain;
pub type Condition = CallChain;

#[derive(Debug)]
pub struct Rule {
    pub name: String,
    pub event: String,
    pub args: Vec<String>,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>
}

#[derive(Debug)]
pub struct Block {
    pub actions: Vec<Action>,
    pub conditions: Vec<Condition>
}

#[derive(Debug)]
pub struct CallChain {
    pub idents: Vec<Call>
}

impl PartialEq for CallChain {

    fn eq(&self, other: &Self) -> bool {
        compare_vec(&self.idents, &other.idents)
    }
}

#[derive(Debug, PartialEq)]
pub enum Call {
    Fn(String, Vec<String>),
    Var(String)
}

pub fn rule_parser() -> impl Parser<Token, Vec<Rule>, Error = Simple<Token>> {
    let rule_name = filter_map(|span, token| match token {
        Token::String(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    let ident = filter_map(|span, token| match token {
        Token::Ident(ident) => Ok(ident.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    let args = ident
        .clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(Token::Ctrl('('), Token::Ctrl(')'))
        .labelled("function args");

    let ident_chain = ident
        .clone()
        .then(args.clone().repeated().at_most(1).map(|mut o| o.pop()))
        .separated_by(just(Token::Ctrl('.')))
        .map(|o| o.into_iter().map(|i| match i {
            (name, Some(args)) => Call::Fn(name, args),
            (name, None) => Call::Var(name)
        }).collect::<Vec<Call>>());

    let cond = just(Token::Cond)
        .ignore_then(ident_chain.clone())
        .then_ignore(just(Token::Ctrl(';')))
        .map(|o| Condition { idents: o });

    let block = just(Token::Ctrl('{'))
        .ignore_then(cond.repeated())
        .then(ident_chain.clone()
            .then_ignore(just(Token::Ctrl(';')))
            .map(|o| Action { idents: o })
            .repeated()
        )
        .then_ignore(just(Token::Ctrl('}')))
        .map(|o| Block { actions: o.0, conditions: o.1 });

    let rule = just(Token::Rule)
        .ignore_then(rule_name)
        .then(ident)
        .then(args.clone())
        .then(block)
        .map(|(((rule_name, ident),args), block)| Rule {
            conditions: block.conditions,
            actions: block.actions,
            name: rule_name,
            event: ident,
            args
        });

    rule.repeated()
        .then_ignore(end())
}

fn compare_vec<T: PartialEq>(a: &Vec<T>, b: &Vec<T>) -> bool {
    let matching = a.iter().zip(b.iter()).filter(|&(a, b)| a == b).count();
    matching == a.len() && matching == b.len()
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, Stream};
    use std::fs::{read_to_string};
    use once_cell::sync::Lazy;
    use crate::language::lexer::lexer;
    use crate::language::parser::{compare_vec, Rule, rule_parser};

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
                actions: Vec::new()
            },
            Rule {
                name: "Test".to_string(),
                event: "MyEvent".to_string(),
                args: vec!["Hello".to_string(), "World".to_string()],
                conditions: Vec::new(),
                actions: Vec::new()
            },
            Rule {
                name: "Heal on Kill".to_string(),
                event: "PlayerDealtFinalBlow".to_string(),
                args: vec!["Team1".to_string(), "Slot1".to_string()],
                conditions: Vec::new(),
                actions: Vec::new()
            }
        ];

        assert_eq!(rules.len(), expected.len());

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