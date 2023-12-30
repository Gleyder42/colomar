use super::cir::{CalledType, CalledTypes, Type};
use super::database::CompilerDatabase;
use super::error::CompilerError;
use super::span::{CopyRange, FatSpan, Span, SpanInterner, SpanSource, SpanSourceId};
use super::{Ident, TextId};
use crate::cst::{Path, PathName};
use crate::source_cache::{EmptyLookupSource, LookupSourceCache, SourceCache};
use ariadne::{sources, Color, Fmt, Label, ReportBuilder, ReportKind, Source};
use chumsky::error::Rich;
use either::Either;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Display;
use std::io::{Cursor, Write};
use std::path::PathBuf;
use std::process::Output;

pub type Cache<'a> = LookupSourceCache<'a>;

type Report<'a> = ReportBuilder<'a, Span>;

mod ind {
    use ariadne::Color;

    pub const UNKNOWN: Color = Color::Red;
    pub const NAME: Color = Color::Cyan;
}

pub fn new_print_errors(
    unique_errors: HashSet<CompilerError>,
    db: &CompilerDatabase,
    mut source_cache: Cache,
    dummy_report_values: &DummyReportValues,
    output: &mut Cursor<Vec<u8>>,
) {
    for error in unique_errors {
        let report: Report = match error.main_span() {
            Some(main_span) => ariadne::Report::build(
                ReportKind::Error,
                main_span.source,
                main_span.location.start as usize,
            ),
            None => ariadne::Report::<Span>::build(
                COMPILER_ERROR,
                dummy_report_values.0.source,
                dummy_report_values.0.location.start as usize,
            ),
        }
        .with_code(error.error_code());

        let report: Report = match error {
            CompilerError::NotImplemented(name, span) => {
                report_not_implemented_error(name, span, report)
            }
            CompilerError::DuplicateIdent { first, second } => {
                report_duplicate_ident_error(first, second, report, source_cache.interner)
            }
            CompilerError::CannotFindIdent(ident) => {
                report_cannot_find_ident_error(ident, report, source_cache.interner)
            }
            CompilerError::NotA(name, actual, expected) => {
                report_not_a_error(name, actual, expected, report, source_cache.interner)
            }
            CompilerError::WrongType { expected, actual } => {
                report_wrong_type_error(expected, actual, report, source_cache.interner)
            }
            CompilerError::CannotFindPrimitiveDecl(_) => {
                todo!()
            }
            CompilerError::CannotFindNativeDef(_) => {
                todo!()
            }
            CompilerError::PlaceholderError(_) => {
                todo!()
            }
            CompilerError::WstParserError(_) => {
                todo!()
            }
            CompilerError::WstLexerError(_) => {
                todo!()
            }
            CompilerError::MissingArg { .. } => {
                todo!()
            }
            CompilerError::CannotFindNamedArg(_) => {
                todo!()
            }
            CompilerError::ArgOutOfRange(_, _) => {
                todo!()
            }
            CompilerError::DuplicateNamedArg(_) => {
                todo!()
            }
            CompilerError::CannotMixArgs(_) => {
                todo!()
            }
            CompilerError::CannotEvalAsConst => {
                todo!()
            }
            CompilerError::WrongTypeInBinaryExpr(_, _) => {
                todo!()
            }
            CompilerError::CannotFindFile(path) => {
                report_cannot_find_file(path, report, source_cache.interner)
            }
            CompilerError::CannotFindStruct(name) => {
                report_cannot_find_struct_error(name, report, source_cache.interner)
            }
        };

        report
            .finish()
            .write(&mut source_cache, output.get_mut())
            .expect("Printing should never fail");
    }
}

fn report_cannot_find_file<'a>(
    path: Path,
    report: Report<'a>,
    db: &CompilerDatabase,
) -> Report<'a> {
    report
        .with_message(format!(
            "Cannot find file {}",
            path.name.name(db).fg(ind::NAME)
        ))
        .with_label(Label::new(path.span).with_color(ind::NAME))
}

fn report_cannot_find_struct_error<'a>(
    name: TextId,
    report: Report<'a>,
    db: &CompilerDatabase,
) -> Report<'a> {
    report.with_message(format!(
        "Cannot find {} struct",
        name.name(db).fg(ind::UNKNOWN)
    ))
}

fn report_wrong_type_error<'a>(
    expected: Either<Type, CalledTypes>,
    actual: CalledType,
    report: Report<'a>,
    db: &CompilerDatabase,
) -> Report<'a> {
    let report = report.with_message("Wrong type");

    let report = match expected {
        Either::Left(r#type) => report,
        Either::Right(called_types) => report.with_label(
            Label::new(called_types.span)
                .with_message("Expected")
                .with_color(Color::Red),
        ),
    };

    report.with_label(
        Label::new(actual.span)
            .with_message(format!("Actual type is {}", actual.r#type.name(db)))
            .with_color(Color::Blue),
    )
}

fn report_not_a_error<'a>(
    name: &'a str,
    expected: Ident,
    actual: Ident,
    report: Report<'a>,
    db: &CompilerDatabase,
) -> Report<'a> {
    report
        .with_message(format!("{} is not {}", name, expected.value.name(db)))
        .with_label(
            Label::new(actual.span)
                .with_message(format!("Found {} here", actual.value.name(db)))
                .with_color(Color::Magenta),
        )
        .with_label(
            Label::new(expected.span).with_message(format!("Expected {}", expected.value.name(db))),
        )
}

fn report_cannot_find_ident_error<'a>(
    ident: Ident,
    report: Report<'a>,
    db: &CompilerDatabase,
) -> Report<'a> {
    report
        .with_message(format!("Cannot find {}", ident.value.name(db)))
        .with_label(Label::new(ident.span).with_color(Color::Red))
}

fn report_duplicate_ident_error<'a>(
    first: Ident,
    second: Ident,
    report: Report<'a>,
    db: &CompilerDatabase,
) -> Report<'a> {
    report
        .with_message(format!(
            "Duplicated ident {}",
            first.value.name(db).fg(ind::NAME)
        ))
        .with_label(
            Label::new(first.span)
                .with_message("First defined here")
                .with_color(ind::NAME),
        )
        .with_label(
            Label::new(second.span)
                .with_message("Second defined here")
                .with_color(Color::Blue),
        )
}

fn report_not_implemented_error<'a>(
    name: Cow<'a, str>,
    span: Span,
    report: Report<'a>,
) -> Report<'a> {
    report
        .with_message(format!("{} is not implemented", name))
        .with_label(Label::new(span).with_color(Color::Magenta))
}

const ERROR_KIND: ReportKind = ReportKind::Error;

const COMPILER_ERROR: ReportKind =
    ReportKind::Custom("Internal Compiler Error", Color::RGB(219, 13, 17));

pub struct DummyReportValues(Span, EmptyLookupSource);

impl DummyReportValues {
    pub fn new(db: &impl SpanInterner) -> Self {
        let dummy_span = Span {
            location: CopyRange { start: 0, end: 0 },
            source: db.intern_span_source(PathBuf::from("DummySpanSource")),
        };
        DummyReportValues(dummy_span, EmptyLookupSource::default())
    }
}

pub fn print_cannot_find_primitive_decl(db: &CompilerDatabase, error_code: u16, name: TextId) {
    /// We use ariadne to print the compiler error, even though we have no span
    /// nor source message.
    /// If want to have a consistent error reporting so I use ariadne instead of
    /// manuel error printing.
    /// However this requires to have a span of some type.
    let dummy_span = Span {
        location: CopyRange { start: 0, end: 0 },
        source: db.intern_span_source(PathBuf::from("DummySpanSource")),
    };

    ariadne::Report::<Span>::build(
        COMPILER_ERROR,
        dummy_span.source,
        dummy_span.location.start as usize,
    )
    .with_code(error_code)
    .with_message(format!(
        "Cannot find {} primitive",
        name.name(db).fg(Color::Cyan)
    ))
    .finish()
    .print(EmptyLookupSource::default())
    .unwrap();
}

fn print_wst_error(buf: Vec<u8>) {
    std::io::stdout().write(&buf).unwrap();
}
