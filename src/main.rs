#![feature(type_alias_impl_trait)]
#![feature(box_patterns)]

extern crate core;

use std::fmt::{Display};
use std::fs;
use std::hash::Hash;
use std::io::{Read};
use std::ops::Range;
use std::path::Path;
use chumsky::prelude::*;
use chumsky::Stream;
use crate::language::lexer::{lexer, Token};
use crate::language::parser::parser;

pub type Span = Range<usize>;

use ariadne::{Color, Label, Report, ReportBuilder, ReportKind, sources};
use crate::compiler::compile;
use crate::language::converter::{convert, ConverterError};

pub mod workshop;
pub mod language;
pub mod test_assert;
mod compiler;

enum CompilerError {
    Lexer(Simple<char>),
    Parser(Simple<Token>),
    Converter(ConverterError),
}

fn main() {
    let filename = "milestone_one.colo";
    let filepath = format!("dsl/example/{filename}");
    let path = Path::new(&filepath);
    let mut file = fs::File::open(path).expect("Cannot read from file");

    let mut source = String::new();
    file.read_to_string(&mut source).expect("Cannot read file content");

    let (tokens, lexer_errors) = lexer().parse_recovery(source.as_str());

    let (ast, parser_errors) = if let Some(tokens) = tokens {
        println!("{:#?}", tokens);
        let stream = Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter());
        parser().parse_recovery(stream)
    } else {
        (None, Vec::new())
    };

    let (im, converter_errors) = if let Some(ast) = ast {
        //println!("{ast:#?}");
        convert(ast)
    } else {
        (None, Vec::new())
    };

    let workshop_tree = if let Some(im) = im {
        let wt = compile(im);
        Some(wt)
    } else {
        None
    };

    let mut compiler_errors: Vec<CompilerError> = Vec::new();
    compiler_errors.append(&mut lexer_errors.into_iter().map(CompilerError::Lexer).collect());
    compiler_errors.append(&mut parser_errors.into_iter().map(CompilerError::Parser).collect());
    compiler_errors.append(&mut converter_errors.into_iter().map(CompilerError::Converter).collect());

    match workshop_tree {
        Some(wt) if compiler_errors.is_empty() => {
            println!("{}", wt)
        }
        _ => {
            print_errors(compiler_errors, filename, source.as_str());
        }
    }
}

fn print_errors(compiler_errors: Vec<CompilerError>, src_id: &'static str, source: &str) {
    for compiler_error in compiler_errors {
        let builder = match compiler_error {
            CompilerError::Lexer(error) => print_simple_error(error, "Lexer", src_id),
            CompilerError::Parser(error) => print_simple_error(error, "Parser", src_id),
            CompilerError::Converter(error) => print_converter_error(error, src_id)
        };

        builder
            .finish()
            .print(sources(vec![
                (src_id, source)
            ]))
            .unwrap();
    }
}

fn print_converter_error(error: ConverterError, src_id: &str) -> ReportBuilder<(&str, Span)> {
    match error {
        ConverterError::MismatchedTypes { message, ident_message, ident_span } => {
            Report::build(ReportKind::Error, src_id, ident_span.start)
                .with_message(message)
                .with_label(Label::new((src_id, ident_span))
                    .with_message(ident_message)
                    .with_color(Color::Red))
        }
        ConverterError::CannotResolveIdent(message, span) => {
            Report::build(ReportKind::Error, src_id, span.start)
                .with_message(message)
                .with_label(Label::new((src_id, span)).with_color(Color::Red))
        },
        ConverterError::DuplicateIdent { message, first_defined_span, second_defined_span } => {
            Report::build(ReportKind::Error, src_id, first_defined_span.start)
                .with_message(message)
                .with_label(Label::new((src_id, first_defined_span)).with_message("First here").with_color(Color::Red))
                .with_label(Label::new((src_id, second_defined_span)).with_message("Second here").with_color(Color::Red))
        },
        ConverterError::ResolvedIdentWrongType { message, help, called_span, referenced_span } => {
            Report::build(ReportKind::Error, src_id, called_span.start)
                .with_message(message)
                .with_label(Label::new((src_id, called_span))
                    .with_message("Used here").with_color(Color::Blue)
                )
                .with_label(Label::new((src_id, referenced_span))
                    .with_message("Found type").with_color(Color::Magenta)
                )
                .with_help(help)
        }
    }
}

fn print_simple_error<'a, T: Display + Hash + Eq>(error: Simple<T>, code: &'static str, src_id: &'a str) -> ReportBuilder<(&'a str, Span)> {
    let builder = Report::build(ReportKind::Error, src_id, error.span().start)
        .with_code(code)
        .with_message(error.to_string())
        .with_label(Label::new((src_id, error.span())).with_color(Color::Red));
    builder
}