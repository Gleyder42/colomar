use crate::compiler::cir::{CalledType, CalledTypes, Type};
use crate::compiler::database::CompilerDatabase;
use crate::compiler::error::CompilerError;
use crate::compiler::span::{FatSpan, Span, SpanSourceId};
use crate::compiler::{Ident, Text};
use ariadne::{sources, Color, Fmt, Label, Report, ReportKind, Source};
use either::Either;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::ops::Range;

pub fn new_print_errors(
    source_map: &HashMap<SpanSourceId, String>,
    db: &mut CompilerDatabase,
    unique_errors: HashSet<CompilerError>,
) {
    for error in unique_errors {
        let report = match error {
            CompilerError::NotImplemented(_, _) => {}
            CompilerError::DuplicateIdent { .. } => {}
            CompilerError::CannotFindIdent(_) => {}
            CompilerError::NotA(_, _, _) => {}
            CompilerError::WrongType { .. } => {}
            CompilerError::CannotFindPrimitiveDecl(_) => {}
            CompilerError::CannotFindNativeDef(_) => {}
            CompilerError::PlaceholderError(_) => {}
            CompilerError::WstParserError(_, _) => {}
            CompilerError::WstLexerError(_, _) => {}
            CompilerError::MissingArg { .. } => {}
            CompilerError::CannotFindNamedArg(_) => {}
            CompilerError::ArgOutOfRange(_, _) => {}
            CompilerError::DuplicateNamedArg(_) => {}
            CompilerError::CannotMixArgs(_) => {}
            CompilerError::CannotEvalAsConst => {}
            CompilerError::WrongTypeInBinaryExpr(_, _) => {}
            CompilerError::CannotFindFile(_) => {}
            CompilerError::CannotFindStruct(_) => {}
        };
    }
}

const ERROR_KIND: ReportKind = ReportKind::Error;

const COMPILER_ERROR: ReportKind = ReportKind::Custom("Compiler Error", Color::RGB(219, 13, 17));

pub fn print_errors(
    source_map: &HashMap<SpanSourceId, String>,
    db: &mut CompilerDatabase,
    unique_errors: HashSet<CompilerError>,
) {
    for analysis_error in unique_errors {
        let error_code = analysis_error.error_code();

        match analysis_error {
            CompilerError::DuplicateIdent { first, second } => {
                print_duplicate_error(source_map, db, error_code, &first, &second);
            }
            CompilerError::CannotFindIdent(ident) => {
                print_cannot_find_ident(source_map, db, error_code, &ident);
            }
            CompilerError::NotA(type_name, actual_rvalue, occurrence) => {
                print_not_a(
                    source_map,
                    db,
                    error_code,
                    type_name,
                    actual_rvalue,
                    &occurrence,
                );
            }
            CompilerError::WrongType { actual, expected } => {
                print_wrong_type(source_map, db, error_code, &actual, expected);
            }
            CompilerError::CannotFindPrimitiveDecl(name) => {
                print_cannot_find_primitive_decl(db, error_code, name)
            }
            CompilerError::CannotFindNativeDef(_) => {}
            CompilerError::NotImplemented(reason, span) => {
                print_not_implemented(source_map, db, error_code, reason, span);
            }
            CompilerError::PlaceholderError(_) => {}
            CompilerError::WstLexerError(source, error)
            | CompilerError::WstParserError(source, error) => print_wst_error(error),
            CompilerError::MissingArg { .. } => {}
            CompilerError::CannotFindNamedArg(_) => {}
            CompilerError::ArgOutOfRange(_, _) => {}
            CompilerError::DuplicateNamedArg(_) => {}
            CompilerError::CannotMixArgs(_) => {}
            CompilerError::CannotEvalAsConst => {}
            CompilerError::WrongTypeInBinaryExpr(_, _) => {}
            CompilerError::CannotFindFile(_) => {}
            CompilerError::CannotFindStruct(_) => {}
        }
    }
}

fn print_cannot_find_primitive_decl(db: &mut CompilerDatabase, error_code: u16, name: Text) {
    /// We use ariadne to print the compiler error, even though we have no span
    /// nor source message.
    /// If want to have a consistent error reporting so I use ariadne instead of
    /// manuel error printing.
    /// However this requires to have a span of some type.
    type DummyType = Range<usize>;

    Report::<DummyType>::build(COMPILER_ERROR, (), 0)
        .with_code(error_code)
        .with_message(format!(
            "Cannot find {} primitive",
            name.name(db).fg(Color::Cyan)
        ))
        .finish()
        .print(Source::from(""))
        .unwrap();
}

fn print_wst_error(buf: Vec<u8>) {
    std::io::stdout().write(&buf).unwrap();
}

fn print_not_implemented(
    source_map: &HashMap<SpanSourceId, String>,
    db: &mut CompilerDatabase,
    error_code: u16,
    reason: Cow<str>,
    span: Span,
) {
    let source = source_map.get(&span.source).unwrap();
    let span = FatSpan::from_span(db, span);

    Report::build(
        COMPILER_ERROR,
        span.source.clone(),
        span.location.start() as usize,
    )
    .with_code(error_code)
    .with_message("Not yet implemented")
    .with_label(
        Label::new(span.clone())
            .with_color(Color::Red)
            .with_message(reason),
    )
    .finish()
    .eprint(sources(vec![(span.source.clone(), &source)]))
    .unwrap();
}

fn print_wrong_type(
    source_map: &HashMap<SpanSourceId, String>,
    db: &mut CompilerDatabase,
    error_code: u16,
    actual: &CalledType,
    expected: Either<Type, CalledTypes>,
) {
    let actual_span = FatSpan::from_span(db, actual.span);
    let actual_source = source_map.get(&actual.span.source).unwrap();

    let report_builder = Report::build(
        ERROR_KIND,
        actual_span.source.clone(),
        actual_span.location.start() as usize,
    )
    .with_code(error_code)
    .with_message("Wrong types")
    .with_label(
        Label::new(actual_span.clone())
            .with_color(Color::Blue)
            .with_message(format!(
                "Returned type is {}",
                actual.r#type.name(db).fg(Color::Cyan)
            )),
    );

    let report_builder = match expected {
        Either::Left(r#type) => {
            report_builder.with_label(Label::new(actual_span.clone()).with_message(format!(
                "Expected to return type {}",
                r#type.name(db).fg(Color::Cyan)
            )))
        }
        Either::Right(called_type) => {
            let called_type_span = FatSpan::from_span(db, called_type.span);

            report_builder.with_label(
                Label::new(called_type_span.clone())
                    .with_color(Color::Blue)
                    .with_message(format!(
                        "Declared type is {}",
                        called_type.name(db).fg(Color::Cyan)
                    )),
            )
        }
    };

    report_builder
        .finish()
        .eprint(sources(vec![
            (actual_span.source.clone(), &actual_source),
            // TODO Add expected span
        ]))
        .unwrap();
}

fn print_not_a(
    source_map: &HashMap<SpanSourceId, String>,
    db: &mut CompilerDatabase,
    error_code: u16,
    type_name: &str,
    actual_rvalue: Ident,
    occurrence: &Ident,
) {
    let occurrence_span = FatSpan::from_span(db, occurrence.span);
    let source = source_map.get(&occurrence.span.source).unwrap();

    Report::build(
        ERROR_KIND,
        occurrence_span.source.clone(),
        occurrence_span.location.start() as usize,
    )
    .with_code(error_code)
    .with_message(format!(
        "{} is not a {}",
        actual_rvalue.value.name(db),
        type_name
    ))
    .with_label(
        Label::new(occurrence_span.clone())
            .with_message(format!("Is of type {}", actual_rvalue.value.name(db))),
    )
    .finish()
    .eprint(sources(vec![(occurrence_span.source.clone(), &source)]))
    .unwrap();
}

fn print_cannot_find_ident(
    source_map: &HashMap<SpanSourceId, String>,
    db: &mut CompilerDatabase,
    error_code: u16,
    ident: &Ident,
) {
    let span = FatSpan::from_span(db, ident.span);
    let source = source_map.get(&ident.span.source).unwrap();

    Report::build(
        ERROR_KIND,
        span.source.clone(),
        span.location.start() as usize,
    )
    .with_code(error_code)
    .with_message(format!(
        "Cannot find {} in the current scope",
        ident.value.name(db).fg(Color::Cyan)
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

fn print_duplicate_error(
    source_map: &HashMap<SpanSourceId, String>,
    db: &mut CompilerDatabase,
    error_code: u16,
    first: &Ident,
    second: &Ident,
) {
    let first_span = FatSpan::from_span(db, first.span);
    let second_span = FatSpan::from_span(db, second.span);

    let first_source = source_map.get(&first.span.source).unwrap();
    let second_source = source_map.get(&second.span.source).unwrap();

    Report::<FatSpan>::build(
        ERROR_KIND,
        first_span.source.clone(),
        first_span.location.start() as usize,
    )
    .with_code(error_code)
    .with_message(format!(
        "{} is already defined in the current scope",
        second.value.name(db).fg(Color::Cyan)
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
        (first_span.source.clone(), first_source),
        (second_span.source.clone(), second_source),
    ]))
    .unwrap();
}
