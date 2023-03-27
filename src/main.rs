#![feature(type_alias_impl_trait)]

extern crate core;

use std::fs;
use std::io::Read;
use std::ops::Range;
use std::path::Path;
use chumsky::prelude::*;
use chumsky::Stream;
use crate::language::lexer::{lexer, Token};
use crate::language::parser::parser;

pub type Span = Range<usize>;
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportBuilder, ReportKind, Source};
use chumsky::error::SimpleReason;

pub mod workshop;
pub mod language;
pub mod test_assert;
mod compiler;
mod multimap;

fn main() {
    let path = Path::new("dsl/example/test2.colo");
    let mut file = fs::File::open(path).unwrap();

    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();

    let (tokens, _) = lexer().parse_recovery(source.as_str());

    if let Some(tokens) = tokens {
        let (ast, errors) = parser().parse_recovery(Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter()));
        println!("{:#?}", ast);
        println!("{:#?}", errors);

        if let Some(ast) = ast {
            let im = language::converter::convert(ast);

            println!("{:#?}", im);
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
}
