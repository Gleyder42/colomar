extern crate core;

use std::borrow::Borrow;
use std::fmt::{Debug, Formatter, write};
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};
use chumsky::prelude::*;
use chumsky::text::Character;
use std::string::String;
use chumsky::chain::Chain;
use chumsky::combinator::To;
use chumsky::Stream;

pub type Span = std::ops::Range<usize>;

const BASE_10: u32 = 10;

pub mod workshop;

fn main() {
    let source = fs::read_to_string("dsl/rule.colo")
        .expect("Cannot read from file");

    println!("{:?}", source.chars());

    let (tokens, _) = lexer().parse_recovery_verbose(source.as_str());

    if let Some(tokens) = tokens {
        let (rule, err) = rule_parser()
            .parse_recovery(Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter()));
        println!("{:?}", err);
        if let Some(rule) = rule {
            let mut printer = workshop::WorkshopPrinter::new();
            printer.print_rule(rule);
            print!("{}", printer.output());
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
enum Token {
    Rule,
    Cond,
    Ident(String),
    String(String),
    Num(String),
    Ctrl(char)
}

#[derive(Debug)]
pub struct Rule {
    pub name: String,
    pub event: String,
    pub args: Vec<String>,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>
}

fn rule_parser() -> impl Parser<Token, Vec<Rule>, Error = Simple<Token>> {
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

pub type Action = CallChain;
pub type Condition = CallChain;

#[derive(Debug)]
struct Block {
    actions: Vec<Action>,
    conditions: Vec<Condition>
}

#[derive(Debug)]
pub struct CallChain {
    idents: Vec<Call>
}

#[derive(Debug)]
enum Call {
    Fn(String, Vec<String>),
    Var(String)
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let num = text::int(BASE_10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Token::Num);

    let string = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::String);

    let ctrl = one_of("(){},.:;".chars())
        .map(|c| Token::Ctrl(c));

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "rule" => Token::Rule,
        "cond" => Token::Cond,
        _ => Token::Ident(ident),
    });

    let token = num
        .or(string)
        .or(ctrl)
        .or(ident)
        .map(|o| {
            println!("Test {:?}", o);
            o
        })
        .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
}