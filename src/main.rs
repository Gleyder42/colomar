extern crate core;

use std::fs;
use chumsky::prelude::*;
use chumsky::Stream;
use crate::language::lexer::lexer;
use crate::language::parser::parser;

pub type Span = std::ops::Range<usize>;

pub mod workshop;
pub mod language;
mod test_assert;
mod intermediate;

fn main() {
    let source = fs::read_to_string("dsl/example/test.colo")
        .expect("Cannot read from file");

    let (tokens, _) = lexer().parse_recovery_verbose(source.as_str());

    if let Some(tokens) = tokens {
        let (rule, err) = parser()
            .parse_recovery(Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter()));
        println!("{:#?}", rule);
        println!("{:?}", err);
    }
}