#![feature(type_alias_impl_trait)]
#![feature(box_patterns)]

extern crate core;

use std::fs;
use std::io::Read;
use std::ops::Range;
use std::path::Path;
use chumsky::prelude::*;
use chumsky::Stream;
use crate::language::lexer::{lexer};
use crate::language::parser::parser;

pub type Span = Range<usize>;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source, sources};
use chumsky::error::SimpleReason;
use crate::compiler::compile;
use crate::language::converter::ConverterError;

pub mod workshop;
pub mod language;
pub mod test_assert;
mod compiler;

fn main() {
    let filename = "milestone_one.colo";
    let filepath = format!("dsl/example/{filename}");
    let path = Path::new(&filepath);
    let mut file = fs::File::open(path).unwrap();

    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();

    let (tokens, lexer_errors) = lexer().parse_recovery(source.as_str());
    //println!("{:?}", source.chars().collect::<Vec<_>>());
    let tokens = if let Some(tokens) = tokens {
        println!("{:#?}", tokens.iter().map(|it| &it.0).collect::<Vec<_>>());

        tokens
    } else {
        for error in lexer_errors {
            let report = Report::build(ReportKind::Error, filename, error.span().start);

            let found = error.found()
                .map(|it| it.to_string())
                .unwrap_or("No input".to_string());

            let expected = error.expected()
                .map(|it| match it {
                    Some(char) => char.to_string(),
                    None => "No input".to_string()
                }).collect::<Vec<String>>()
                .join(", ");

            let report = match error.reason() {
                SimpleReason::Unclosed { span, delimiter } => {
                    report
                        .with_code(50)
                        .with_message(format!("Unclosed delimiter {delimiter}, expected {expected} but found {found}"))
                        .with_label(Label::new((filename, span.clone())).with_color(Color::Red))
                },
                SimpleReason::Unexpected => {
                    report
                        .with_code(51)
                        .with_message(format!("Unexpected input {found}, but expected {expected}"))
                        .with_label(Label::new((filename, error.span())))
                },
                SimpleReason::Custom(message) => {
                    report
                        .with_code(52)
                        .with_message(message)
                        .with_label(Label::new((filename, error.span())))
                }
            };

            report
                .finish()
                .print(sources(vec![
                    (filename, source.as_str())
                ]))
                .unwrap();
        }
        return;
    };

    let (ast, errors) = parser().parse_recovery(Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter()));
    //println!("{:#?}", ast);
    //println!("{:#?}", errors);

    if let Some(ast) = ast {
        let (im, im_errors) = language::converter::convert(ast);


        //println!("{:#?}", im);
        //println!("{:#?}", im_errors);

        if im_errors.is_empty() {
            let tree = compile(im);
            println!("{}", tree);
        }

        im_errors.into_iter()
            .for_each(|it| {
                let _ = match it {
                    ConverterError::CannotResolveIdent(message, span) => {
                        Report::build(ReportKind::Error, "test2.colo", span.start)
                            .with_code(10)
                            .with_message(message)
                            .with_label(
                                Label::new(("test2.colo", span.clone()))
                                    .with_color(Color::Red)
                            )
                            .finish()
                            .print(sources(vec![
                                ("test2.colo", source.as_str())
                            ]))
                            .unwrap();
                    },
                    ConverterError::ResolvedIdentWrongType { message, help, called_span, referenced_span } => {
                        Report::build(ReportKind::Error, "test2.colo", called_span.start)
                            .with_code(2)
                            .with_message(message)
                            .with_label(
                                Label::new(("test2.colo", called_span.clone()))
                                    .with_message("Used here")
                            )
                            .with_label(
                                Label::new(("test2.colo", referenced_span.clone()))
                                    .with_message(help)
                            )
                            .finish()
                            .print(sources(vec![
                                ("test2.colo", source.as_str())
                            ]))
                            .unwrap();
                    }
                };
            });
    }

    for error in errors.into_iter().map(|e| e.map(|it| format!("{}", it))) {
        let report = Report::build(ReportKind::Error, (), error.span().start);
        let report = match error.reason() {
            SimpleReason::Unclosed { span, delimiter } => report
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                )
                .with_label(
                    Label::new(error.span())
                        .with_message(format!(
                            "Must be closed before this {}",
                            error.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            SimpleReason::Unexpected => report
                .with_message(format!(
                    "{}, expected {}",
                    if error.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpected end of input"
                    },
                    if error.expected().len() == 0 {
                        "end of input".to_string()
                    } else {
                        error.expected()
                            .flatten()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ))
                .with_label(
                    Label::new(error.span())
                        .with_message(format!(
                            "Unexpected token {}",
                            error.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                Label::new(error.span())
                    .with_message(format!("{}", msg.fg(Color::Red)))
                    .with_color(Color::Red),
            ),
        };

        report.finish().print(Source::from(&source)).unwrap();
    }
}
