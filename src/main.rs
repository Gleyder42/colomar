extern crate core;

use std::fs;
use chumsky::prelude::*;
use crate::language::lexer::lexer;

pub type Span = std::ops::Range<usize>;

pub mod workshop;
pub mod language;
mod test_assert;

fn main() {
    let source = fs::read_to_string("dsl/rule.colo")
        .expect("Cannot read from file");

    println!("{:?}", source.chars());

    let (tokens, _) = lexer().parse_recovery_verbose(source.as_str());

    // if let Some(tokens) = tokens {
    //     let (rule, err) = rule_parser()
    //         .parse_recovery(Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter()));
    //     println!("{:?}", err);
    //     if let Some(rule) = rule {
    //         let mut printer = WorkshopPrinter::new();
    //         printer.print_rule(rule);
    //         print!("{}", printer.output());
    //     }
    // }
}