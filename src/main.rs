#![feature(result_flattening)]
#![feature(map_try_insert)]

extern crate core;
extern crate salsa;

use crate::language::analysis::interner::Interner;
use crate::language::analysis::{AnalysisDatabase, AnalysisError};
use crate::language::im::{DeclaredArgument, FunctionDecl, PropertyDecl, Root, StructDeclaration};
use crate::language::lexer::lexer;
use crate::language::parser::parser;
use crate::language::{im, FatSpan, Span, SpanSourceId};
use ariadne::{sources, Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use chumsky::Stream;
use either::Either;
use language::error::Trisult;
use std::collections::HashSet;
use std::fs;
use std::io::Read;
use std::ops::Range;
use std::path::Path;

use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::def::DefQuery;

mod compiler;
pub mod language;
pub mod test_assert;
pub mod workshop;

fn main() {
    let filename = "milestone_one.colo";
    let filepath = format!("dsl/example/{filename}");
    let path = Path::new(&filepath);
    let mut file = fs::File::open(path).expect("Cannot read from file");

    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Cannot read file content");

    let mut db = AnalysisDatabase::default();
    let span_source_id: SpanSourceId = db.intern_span_source(path.to_string_lossy().into());

    let (tokens, lexer_errors) = lexer(span_source_id).parse_recovery(source.as_str());
    println!("{:#?}", lexer_errors);

    let (ast, parser_errors) = if let Some(tokens) = tokens {
        let eoi = Span::new(span_source_id, tokens.len()..tokens.len() + 1);
        let stream = Stream::from_iter(eoi, tokens.into_iter());
        parser().parse_recovery(stream)
    } else {
        (None, Vec::new())
    };
    println!("{:#?}", parser_errors);

    if let Some(ast) = ast {
        println!("{:#?}", ast);

        db.set_input_content(ast);

        let im: Trisult<im::Im, AnalysisError> = db.query_im();

        let output = im.to_option();

        println!("{:#?}", output.1);

        if let Some(im) = output.0 {
            for root in im {
                match root {
                    Root::Rule(rule) => {
                        println!("{:#?}", rule);
                    }
                    Root::Event(event) => {
                        let decl = db.lookup_intern_event_decl(event.declaration);

                        let vec = event
                            .definition
                            .arguments
                            .into_iter()
                            .map(|it| db.lookup_intern_decl_arg(it))
                            .collect::<Vec<DeclaredArgument>>();

                        println!(
                            "Event\nDecl: {:#?}\nDef: {:#?}, Proprs: {:#?}",
                            decl, vec, event.definition.properties
                        );
                    }
                    Root::Enum(r#enum) => {
                        let decl = db.lookup_intern_enum_decl(r#enum.declaration);

                        let constants: Vec<_> = r#enum
                            .definition
                            .constants
                            .into_iter()
                            .map(|it| db.lookup_intern_enum_constant(it))
                            .collect();

                        println!(
                            "Enum\nDecl: {:#?}\nDef: {:#?}\nSpan: {:#?}",
                            decl, constants, r#enum.span
                        );
                    }
                    Root::Struct(r#struct) => {
                        let struct_decl: StructDeclaration =
                            db.lookup_intern_struct_decl(r#struct.decl);

                        let properties = r#struct
                            .def
                            .properties
                            .into_iter()
                            .map(|it| db.lookup_intern_property_decl(it))
                            .collect::<Vec<PropertyDecl>>();

                        let functions = r#struct
                            .def
                            .functions
                            .into_iter()
                            .map(|it| db.lookup_intern_function_decl(it))
                            .collect::<Vec<FunctionDecl>>();

                        println!(
                            "Struct {:#?}, Properties: {:#?}, Functions: {:#?}",
                            struct_decl, properties, functions
                        );
                    }
                };
                println!("===")
            }
        }

        println!("{:#?}", output.1);
        // Due the nature of demand-driven compilation, the queries return duplicate errors,
        // if an erroneous query is queried multiple times.
        // However, the errors only seem to be duplicate, because they just miss context information
        // which would distinct them.
        // Filtering here while having the all errors in the result wastes space and computing power.
        // The question is if this is negligible.
        let original_len = output.1.len();
        let unique_errors = output.1.into_iter().collect::<HashSet<_>>();
        let new_len = unique_errors.len();
        for analysis_error in unique_errors {
            let error_code = analysis_error.error_code();
            const ERROR_KIND: ReportKind = ReportKind::Error;
            const COMPILER_ERROR: ReportKind =
                ReportKind::Custom("Compiler Error", Color::RGB(219, 13, 17));

            match analysis_error {
                AnalysisError::DuplicateIdent { first, second } => {
                    let first_span = FatSpan::from_span(&db, first.span);
                    let second_span = FatSpan::from_span(&db, second.span);

                    Report::<FatSpan>::build(
                        ERROR_KIND,
                        first_span.source.clone(),
                        first_span.location.start,
                    )
                    .with_code(error_code)
                    .with_message(format!(
                        "{} is already defined in the current scope",
                        second.value.fg(Color::Cyan)
                    ))
                    .with_label(
                        Label::new(first_span.clone())
                            .with_color(Color::Cyan)
                            .with_message("First defined here"),
                    )
                    .with_label(
                        Label::new(second_span.clone())
                            .with_color(Color::Cyan)
                            .with_message("Second defined here"),
                    )
                    .finish()
                    .eprint(sources(vec![
                        (first_span.source.clone(), &source),
                        (second_span.source.clone(), &source),
                    ]))
                    .unwrap();
                }
                AnalysisError::CannotFindDefinition(_def) => {
                    todo!()
                }
                AnalysisError::CannotFindIdent(ident) => {
                    let span = FatSpan::from_span(&db, ident.span);

                    Report::build(ERROR_KIND, span.source.clone(), span.location.start)
                        .with_code(error_code)
                        .with_message(format!(
                            "Cannot find {} in the current scope",
                            ident.value.fg(Color::Cyan)
                        ))
                        .with_label(
                            Label::new(span.clone())
                                .with_color(Color::Cyan)
                                .with_message("This"),
                        )
                        .finish()
                        .eprint(sources(vec![(span.source.clone(), &source)]))
                        .unwrap();
                }
                AnalysisError::NotA(type_name, actual_rvalue, occurrence) => {
                    let occurrence_span = FatSpan::from_span(&db, occurrence.span.clone());

                    Report::build(
                        ERROR_KIND,
                        occurrence_span.source.clone(),
                        occurrence_span.location.start,
                    )
                    .with_code(error_code)
                    .with_message(format!(
                        "{} is not a {}",
                        actual_rvalue.name(&db).value,
                        type_name
                    ))
                    .with_label(
                        Label::new(occurrence_span.clone())
                            .with_message(format!("Is of type {}", actual_rvalue.name(&db).value)),
                    )
                    .finish()
                    .eprint(sources(vec![(occurrence_span.source.clone(), &source)]))
                    .unwrap();
                }
                AnalysisError::WrongType { actual, expected } => {
                    let actual_span = FatSpan::from_span(&db, actual.span.clone());
                    let report_builder = Report::build(
                        ERROR_KIND,
                        actual_span.source.clone(),
                        actual_span.location.start,
                    )
                    .with_code(error_code)
                    .with_message("Wrong types")
                    .with_label(
                        Label::new(actual_span.clone())
                            .with_color(Color::Blue)
                            .with_message(format!(
                                "Returned type is {}",
                                actual.r#type.name(&db).fg(Color::Cyan)
                            )),
                    );

                    let report_builder = match expected {
                        Either::Left(r#type) => report_builder.with_label(
                            Label::new(actual_span.clone()).with_message(format!(
                                "Expected to return type {}",
                                r#type.name(&db).fg(Color::Cyan)
                            )),
                        ),
                        Either::Right(called_type) => {
                            let called_type_span =
                                FatSpan::from_span(&db, called_type.span.clone());

                            report_builder.with_label(
                                Label::new(called_type_span.clone())
                                    .with_color(Color::Blue)
                                    .with_message(format!(
                                        "Declared type is {}",
                                        called_type.name(&db).fg(Color::Cyan)
                                    )),
                            )
                        }
                    };

                    report_builder
                        .finish()
                        .eprint(sources(vec![
                            (actual_span.source.clone(), &source),
                            // TODO Add expected span
                        ]))
                        .unwrap();
                }
                AnalysisError::CannotFindPrimitiveDeclaration(name) => {
                    /// We use ariadne to print the compiler error, even though we have no span
                    /// nor source message.
                    /// If want to have a consistent error reporting so I use ariadne instead of
                    /// manuel error printing.
                    /// However this requires to have a span of some type.
                    type DummyType = Range<usize>;

                    Report::<DummyType>::build(COMPILER_ERROR, (), 0)
                        .with_code(error_code)
                        .with_message(format!("Cannot find {} primitive", name.fg(Color::Cyan)))
                        .finish()
                        .print(Source::from(""))
                        .unwrap();
                }
                AnalysisError::CannotFindNativeDefinition(_) => {
                    todo!()
                }
                AnalysisError::InvalidNativeDefinition(_) => {
                    todo!()
                }
            }
        }
        println!(
            "Reduced errors from {} to {}. \nReduced size by {}",
            original_len,
            new_len,
            100.0 - (new_len as f32 / original_len as f32) * 100.0
        );
    }
}
