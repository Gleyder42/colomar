#![feature(type_alias_impl_trait)]
#![feature(box_patterns)]

extern crate core;

use std::fs;
use std::io::{Read};
use std::ops::Range;
use std::path::Path;
use chumsky::prelude::*;
use chumsky::Stream;
use crate::language::lexer::{lexer};
use crate::language::parser::parser;

pub type Span = Range<usize>;

pub mod workshop;
pub mod language;
pub mod test_assert;
mod compiler;

fn main() {
    let filename = "milestone_one.colo";
    let filepath = format!("dsl/example/{filename}");
    let path = Path::new(&filepath);
    let mut file = fs::File::open(path).expect("Cannot read from file");

    let mut source = String::new();
    file.read_to_string(&mut source).expect("Cannot read file content");

    let (tokens, _lexer_errors) = lexer().parse_recovery(source.as_str());

    let (ast, _parser_errors) = if let Some(tokens) = tokens {
        let stream = Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter());
        parser().parse_recovery(stream)
    } else {
        (None, Vec::new())
    };

    if let Some(ast) = ast {
        println!("{:#?}", ast);
    }
}
