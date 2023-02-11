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

#[derive(Debug)]
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