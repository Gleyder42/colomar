use super::cir::{CalledType, CalledTypes, Type};
use super::database::CompilerDatabase;
use super::error::{CompilerError, ErrorCause};
use super::span::{CopyRange, Span, SpanInterner, SpanSourceId, FAKE_SPAN_SOURCE_NAME};
use super::{Ident, InternedName, OwnedRich, Text, TextId};
use crate::analysis::interner::Interner;
use crate::cst::Path;
use crate::source_cache::{EmptyLookupSource, LookupSourceCache};
use ariadne::{Color, Fmt, Label, ReportBuilder, ReportKind};
use chumsky::error::{Rich, RichPattern, RichReason};
use either::Either;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Debug;
use std::io::Cursor;
use std::path::PathBuf;

pub type Cache<'a> = LookupSourceCache<'a>;

type Report<'a> = ReportBuilder<'a, Span>;

mod ind {
    use ariadne::Color;

    pub const UNKNOWN: Color = Color::Red;
    pub const NAME: Color = Color::Cyan;
}

const PRINTING_ERROR_MESSAGE: &'static str = "Error while printing. This is an error";

pub fn new_print_errors(
    unique_errors: HashSet<CompilerError>,
    db: &CompilerDatabase,
    mut source_cache: Cache,
    dummy_report_values: &mut DummyReportValues,
    output: &mut Cursor<Vec<u8>>,
) {
    for error in unique_errors {
        let main_span = error.main_span();
        let report: Report = match main_span {
            Some(main_span) => ariadne::Report::build(
                ReportKind::Error,
                main_span.context,
                main_span.offset.start as usize,
            ),
            None => ariadne::Report::<Span>::build(
                COMPILER_ERROR,
                dummy_report_values.0.context,
                dummy_report_values.0.offset.start as usize,
            ),
        }
        .with_code(error.error_code());

        let report: Report = match error {
            CompilerError::NotImplemented(name, span) => {
                report_not_implemented_error(name, span, report)
            }
            CompilerError::DuplicateIdent { first, second } => {
                report_duplicate_ident_error(first, second, report, db)
            }
            CompilerError::CannotFindIdent(ident) => {
                report_cannot_find_ident_error(ident, report, db)
            }
            CompilerError::NotA(name, actual, expected) => {
                report_not_a_error(name, actual, expected, report, db)
            }
            CompilerError::WrongType { expected, actual } => {
                report_wrong_type_error(expected, actual, report, db)
            }
            CompilerError::CannotFindPrimitiveDecl(text_id, cause) => {
                report_cannot_find_primitive_decl_error(text_id, cause, report, db)
            }
            CompilerError::CannotFindNativeDef(def, cause) => {
                report_cannot_find_native_def_error(def, cause, report)
            }
            CompilerError::PlaceholderError(_, _) => {
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
            CompilerError::CannotFindFile(path) => report_cannot_find_file(path, report, db),
            CompilerError::CannotFindStruct(name, cause) => {
                report_cannot_find_struct_error(name, cause, report, db)
            }
            CompilerError::WstParserError(path, selection, source, errors, cause) => {
                report_wst_error(
                    db,
                    &mut source_cache,
                    report,
                    path,
                    selection,
                    &source,
                    errors,
                    cause,
                )
            }
            CompilerError::WstLexerError(path, selection, source, errors, cause) => {
                report_wst_error(
                    db,
                    &mut source_cache,
                    report,
                    path,
                    selection,
                    &source,
                    errors,
                    cause,
                )
            }
        };

        report
            .finish()
            .write(&mut source_cache, output.get_mut())
            .expect(PRINTING_ERROR_MESSAGE);
    }
}

fn is_fake_span(db: &CompilerDatabase, span: Span) -> bool {
    db.lookup_intern_span_source(span.context) == FAKE_SPAN_SOURCE_NAME.as_os_str()
}

fn report_wst_error<'a, T: Debug + InternedName>(
    db: &CompilerDatabase,
    source_cache: &mut Cache,
    report: Report<'a>,
    path: PathBuf,
    selection: Text,
    source: &str,
    errors: Vec<OwnedRich<T, Span>>,
    cause: ErrorCause,
) -> Report<'a> {
    source_cache.source_cache.set_file(path, selection, source);
    let mut report = report;
    for error in errors {
        report = to_report(report, error.reason(), *error.span(), db);
    }
    report = add_cause(report, cause);
    report
}

fn report_cannot_find_native_def_error(
    string: String,
    error_cause: ErrorCause,
    report: Report,
) -> Report {
    let report = report.with_message(format!(
        "Cannot find native definition for {}",
        string.fg(ind::UNKNOWN)
    ));

    add_cause(report, error_cause)
}

fn report_cannot_find_primitive_decl_error<'a>(
    text_id: TextId,
    error_cause: ErrorCause,
    report: Report<'a>,
    db: &CompilerDatabase,
) -> Report<'a> {
    let report = report.with_message(format!(
        "Cannot find primitive declaration {}",
        text_id.name(db).fg(ind::UNKNOWN)
    ));

    add_cause(report, error_cause)
}

fn add_cause(report: Report, error_cause: ErrorCause) -> Report {
    match error_cause {
        ErrorCause::Span(span) => {
            report.with_label(Label::new(span).with_message("Caused by this"))
        }
        ErrorCause::Message(message) => {
            report.with_note(format!("This error occurred while {}", message))
        }
    }
}

fn report_cannot_find_file<'a>(
    path: Path,
    report: Report<'a>,
    db: &CompilerDatabase,
) -> Report<'a> {
    let mut report = report.with_message(format!(
        "Cannot find file {}",
        format!("src/{}.co", path.name.name(db)).fg(ind::UNKNOWN)
    ));

    if !is_fake_span(db, path.span) {
        report.add_label(
            Label::new(path.span)
                .with_message("Cannot find an associated with that path")
                .with_color(ind::UNKNOWN),
        )
    };

    report
}

fn report_cannot_find_struct_error<'a>(
    name: TextId,
    error_cause: ErrorCause,
    report: Report<'a>,
    db: &CompilerDatabase,
) -> Report<'a> {
    let report = report.with_message(format!(
        "Cannot find {} struct",
        name.name(db).fg(ind::UNKNOWN)
    ));

    add_cause(report, error_cause)
}

fn report_wrong_type_error<'a>(
    expected: Either<Type, CalledTypes>,
    actual: CalledType,
    report: Report<'a>,
    db: &CompilerDatabase,
) -> Report<'a> {
    let report = report.with_message("Wrong type");

    let report = match expected {
        Either::Left(_type) => report,
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

const COMPILER_ERROR: ReportKind =
    ReportKind::Custom("Internal Compiler Error", Color::RGB(219, 13, 17));

pub struct DummyReportValues(Span, EmptyLookupSource);

impl DummyReportValues {
    pub fn new(db: &impl SpanInterner) -> Self {
        let dummy_span = Span {
            offset: CopyRange { start: 0, end: 0 },
            context: db.intern_span_source(PathBuf::from("DummySpanSource")),
        };
        DummyReportValues(dummy_span, EmptyLookupSource::default())
    }
}

/// We use ariadne to print the compiler error, even though we have no span
/// nor source message.
/// If want to have a consistent error reporting so I use ariadne instead of
/// manuel error printing.
/// However this requires to have a span of some type.
pub fn print_cannot_find_primitive_decl(db: &CompilerDatabase, error_code: u16, name: TextId) {
    let dummy_span = Span {
        offset: CopyRange { start: 0, end: 0 },
        context: db.intern_span_source(PathBuf::from("DummySpanSource")),
    };

    ariadne::Report::<Span>::build(
        COMPILER_ERROR,
        dummy_span.context,
        dummy_span.offset.start as usize,
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

// TODO take report as reference
pub fn to_report<'a, T: Debug + InternedName, S: ariadne::Span + Clone>(
    report: ReportBuilder<'a, S>,
    reason: &RichReason<'a, T>,
    span: S,
    interner: &dyn Interner,
) -> ReportBuilder<'a, S> {
    match reason {
        RichReason::ExpectedFound { found, expected } => {
            let message = match found {
                Some(token) => {
                    format!("Unexpected token '{}' found", token.name(interner))
                }
                None => "Expected some token, but found none".to_string(),
            };

            let mut report = report.with_message(message);

            let label = Label::new(span).with_color(ind::UNKNOWN);

            // If expected is empty the loop will not execute and not add the label.
            // Therefore, if expected is empty, we need to add it now.
            if expected.is_empty() {
                report.add_label(label.clone());
            }

            for pattern in expected {
                let label = label.clone();

                let label = match pattern {
                    RichPattern::Token(token) => {
                        label.with_message(format!("Expected token {token:?}"))
                    }
                    RichPattern::Label(label_str) => {
                        label.with_message(format!("Expected label {label_str:?}"))
                    }
                    RichPattern::EndOfInput => label.with_message("Expected no token"),
                };

                report.add_label(label);
            }
            report
        }
        RichReason::Custom(custom) => {
            report.with_label(Label::new(span.clone()).with_message(custom))
        }
        RichReason::Many(errors) => {
            let mut report = report;
            for reason in errors {
                report = to_report(report, reason, span.clone(), interner);
            }
            report
        }
    }
}

pub fn write_error<T, S, U>(
    errors: Vec<(SpanSourceId, Vec<Rich<'static, T, U>>)>,
    cache: &mut LookupSourceCache,
    interner: &dyn Interner,
    span_func: impl for<'a> Fn(SpanSourceId, &'a Rich<'static, T, U>) -> S,
    out_stderr: &mut Cursor<Vec<u8>>,
) where
    T: Debug + InternedName,
    S: ariadne::Span<SourceId = SpanSourceId> + Clone,
{
    for (span_source_id, errors) in errors {
        for error in errors {
            let span = span_func(span_source_id, &error);
            let report = ariadne::Report::build(ReportKind::Error, span_source_id, span.start());
            let report = to_report(report, error.reason(), span, interner);
            report
                .finish()
                .write(&mut *cache, &mut *out_stderr)
                .expect("Writing to a cursor should not fail")
        }
    }
}
