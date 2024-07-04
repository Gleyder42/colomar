use super::cir::{CalledType, CalledTypes, DeclArgId, TypeDesc};
use super::database::CompilerDatabase;
use super::error::{CompilerError, ErrorCause};
use super::span::{CopyRange, Span, SpanInterner, SpanSourceId, FAKE_SPAN_SOURCE_NAME};
use super::{Ident, InternedName, OwnedRich, Text, TextId};
use crate::analysis::interner::Interner;
use crate::cst::Path;
use crate::source_cache::{EmptyLookupSource, SourceCache};
use ariadne::{Color, Fmt, Label, ReportBuilder, ReportKind};
use chumsky::error::{RichPattern, RichReason};
use either::Either;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Debug;
use std::io::Cursor;
use std::path::PathBuf;

type Report<'a> = ReportBuilder<'a, Span>;

mod ind {
    use ariadne::Color;

    pub const UNKNOWN: Color = Color::Red;
    pub const NAME: Color = Color::Cyan;
}

const PRINTING_ERROR_MESSAGE: &'static str = "Error while printing. This is an error";

struct Params<'a> {
    db: &'a dyn Interner,
    report: Report<'a>,
}

pub fn new_print_errors(
    unique_errors: HashSet<CompilerError>,
    db: &CompilerDatabase,
    mut cache: SourceCache,
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

        let mut params = Params { db, report };

        match error {
            CompilerError::NotImplemented(name, span) => {
                report_not_implemented_error(&mut params, name, span);
            }
            CompilerError::DuplicateIdent { first, second } => {
                report_duplicate_ident_error(&mut params, first, second);
            }
            CompilerError::CannotFindIdent(ident) => {
                report_cannot_find_ident_error(&mut params, ident)
            }
            CompilerError::NotA(name, actual, expected) => {
                report_not_a_error(&mut params, name, actual, expected);
            }
            CompilerError::WrongType { expected, actual } => {
                report_wrong_type_error(&mut params, expected, actual);
            }
            CompilerError::CannotFindPrimitiveDecl(text_id, cause) => {
                report_cannot_find_primitive_decl_error(&mut params, text_id, cause);
            }
            CompilerError::CannotFindNativeDef(def, cause) => {
                report_cannot_find_native_def_error(&mut params, def, cause);
            }
            CompilerError::PlaceholderError(_, _) => {
                todo!()
            }
            CompilerError::MissingArg {
                call_site,
                missing_arg,
            } => {
                report_missing_arg_error(&mut params, call_site, missing_arg);
            }
            CompilerError::CannotFindNamedArg(_) => {
                todo!()
            }
            CompilerError::ArgOutOfRange {
                max_index,
                span,
                index,
            } => {
                report_arg_out_of_range(&mut params, max_index, index, span);
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
                report_cannot_find_file(&mut params, path);
            }
            CompilerError::CannotFindStruct(name, cause) => {
                report_cannot_find_struct_error(&mut params, name, cause);
            }
            CompilerError::WstParserError(path, selection, source, errors, cause) => {
                report_wst_error(
                    &mut params,
                    &mut cache,
                    path,
                    selection,
                    &source,
                    errors,
                    cause,
                );
            }
            CompilerError::WstLexerError(path, selection, source, errors, cause) => {
                report_wst_error(
                    &mut params,
                    &mut cache,
                    path,
                    selection,
                    &source,
                    errors,
                    cause,
                );
            }
        };

        params
            .report
            .finish()
            .write(&mut cache, &mut *output)
            .expect(PRINTING_ERROR_MESSAGE);
    }
}

fn report_missing_arg_error(params: &mut Params, span: Span, decl_arg_id: DeclArgId) {
    params.report.set_message("Missing argument");

    let arg = params.db.lookup_intern_decl_arg(decl_arg_id);

    params.report.add_labels([
        Label::new(span)
            .with_color(Color::Cyan)
            .with_message("Missing argument"),
        Label::new(arg.name.span)
            .with_message("Required argument")
            .with_color(Color::Cyan),
    ])
}

fn report_arg_out_of_range(params: &mut Params, max_index: usize, index: usize, span: Span) {
    params.report.set_message("Too many arguments supplied");

    let message = format!(
        "{} argument is out of range, {} is maximum",
        format!("{}th", index + 1).fg(Color::Cyan),
        (max_index + 1).fg(Color::Cyan),
    );
    params.report.add_label(
        Label::new(span)
            .with_message(message)
            .with_color(Color::Red),
    );
}

fn is_fake_span(db: &dyn SpanInterner, span: Span) -> bool {
    db.lookup_intern_span_source(span.context) == FAKE_SPAN_SOURCE_NAME.as_os_str()
}

fn report_wst_error<T: Debug + InternedName>(
    params: &mut Params,
    source_cache: &mut SourceCache,
    path: PathBuf,
    selection: Text,
    source: &str,
    errors: Vec<OwnedRich<T, Span>>,
    cause: ErrorCause,
) {
    source_cache
        .source_cache
        .insert_file(path, selection, source);
    for error in errors {
        to_report(params.db, &mut params.report, error.reason(), *error.span());
    }
    add_cause(params, cause);
}

fn report_cannot_find_native_def_error(
    params: &mut Params,
    string: String,
    error_cause: ErrorCause,
) {
    params.report.set_message(format!(
        "Cannot find native definition for {}",
        string.fg(ind::UNKNOWN)
    ));

    add_cause(params, error_cause);
}

fn report_cannot_find_primitive_decl_error(
    params: &mut Params,
    text_id: TextId,
    error_cause: ErrorCause,
) {
    params.report.set_message(format!(
        "Cannot find primitive declaration {}",
        text_id.name(params.db).fg(ind::UNKNOWN)
    ));

    add_cause(params, error_cause)
}

fn add_cause(params: &mut Params, error_cause: ErrorCause) {
    match error_cause {
        ErrorCause::Span(span) => params
            .report
            .add_label(Label::new(span).with_message("Caused by this")),
        ErrorCause::Message(message) => params
            .report
            .set_note(format!("This error occurred while {}", message)),
    }
}

fn report_cannot_find_file(params: &mut Params, path: Path) {
    params.report.set_message(format!(
        "Cannot find file {}",
        format!("src/{}.co", path.name.name(params.db)).fg(ind::UNKNOWN)
    ));

    if !is_fake_span(params.db, path.span) {
        params.report.add_label(
            Label::new(path.span)
                .with_message("Cannot find an associated with that path")
                .with_color(ind::UNKNOWN),
        );
    }
}

fn report_cannot_find_struct_error(params: &mut Params, name: TextId, error_cause: ErrorCause) {
    params.report.set_message(format!(
        "Cannot find {} struct",
        name.name(params.db).fg(ind::UNKNOWN)
    ));

    add_cause(params, error_cause);
}

fn report_wrong_type_error<'a>(
    params: &mut Params,
    expected: Either<TypeDesc, CalledTypes>,
    actual: CalledType,
) {
    params.report.set_message("Wrong type");

    match expected {
        Either::Left(_type) => {}
        Either::Right(called_types) => {
            params.report.add_label(
                Label::new(called_types.span)
                    .with_message("Expected")
                    .with_color(Color::Red),
            );
        }
    };

    params.report.add_label(
        Label::new(actual.span)
            .with_message(format!("Actual type is {}", actual.r#type.name(params.db)))
            .with_color(Color::Blue),
    )
}

fn report_not_a_error(params: &mut Params, name: &str, expected: Ident, actual: Ident) {
    params.report.set_message(format!(
        "{} is not {}",
        name,
        expected.value.name(params.db)
    ));
    params.report.add_labels([
        Label::new(actual.span)
            .with_message(format!("Found {} here", actual.value.name(params.db)))
            .with_color(Color::Magenta),
        Label::new(expected.span)
            .with_message(format!("Expected {}", expected.value.name(params.db))),
    ]);
}

fn report_cannot_find_ident_error(params: &mut Params, ident: Ident) {
    params
        .report
        .set_message(format!("Cannot find {}", ident.value.name(params.db)));
    params
        .report
        .add_label(Label::new(ident.span).with_color(Color::Red));
}

fn report_duplicate_ident_error(params: &mut Params, first: Ident, second: Ident) {
    params.report.set_message(format!(
        "Duplicated ident {}",
        first.value.name(params.db).fg(ind::NAME)
    ));
    params.report.add_labels([
        Label::new(first.span)
            .with_message("First defined here")
            .with_color(ind::NAME),
        Label::new(second.span)
            .with_message("Second defined here")
            .with_color(Color::Blue),
    ]);
}

fn report_not_implemented_error(params: &mut Params, name: Cow<str>, span: Span) {
    params
        .report
        .set_message(format!("{} is not implemented", name));
    params
        .report
        .add_label(Label::new(span).with_color(Color::Magenta));
}

const COMPILER_ERROR: ReportKind =
    ReportKind::Custom("Internal Compiler Error", Color::RGB(219, 13, 17));

pub struct DummyReportValues(Span, EmptyLookupSource);

impl DummyReportValues {
    pub fn new(db: &dyn SpanInterner) -> Self {
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

pub fn to_report<'a, T: Debug + InternedName, S: ariadne::Span + Clone>(
    db: &dyn Interner,
    report: &mut ReportBuilder<'a, S>,
    reason: &RichReason<'a, T>,
    span: S,
) {
    match reason {
        RichReason::ExpectedFound { found, expected } => {
            let message = match found {
                Some(token) => {
                    format!("Unexpected token '{}' found", token.name(db))
                }
                None => "Expected some token, but found none".to_string(),
            };

            report.set_message(message);

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
        }
        RichReason::Custom(custom) => {
            report.add_label(Label::new(span.clone()).with_message(custom))
        }
        RichReason::Many(errors) => {
            for reason in errors {
                to_report(db, &mut *report, reason, span.clone());
            }
        }
    }
}

pub fn write_error<T, S, U>(
    errors: Vec<(SpanSourceId, Vec<OwnedRich<T, U>>)>,
    cache: &mut SourceCache,
    interner: &dyn Interner,
    span_func: impl for<'a> Fn(SpanSourceId, &'a OwnedRich<T, U>) -> S,
    out_stderr: &mut Cursor<Vec<u8>>,
) where
    T: Debug + InternedName,
    S: ariadne::Span<SourceId = SpanSourceId> + Clone,
{
    for (span_source_id, errors) in errors {
        for error in errors {
            let span = span_func(span_source_id, &error);
            let mut report =
                ariadne::Report::build(ReportKind::Error, span_source_id, span.start());
            to_report::<_, S>(interner, &mut report, error.reason(), span);
            report
                .finish()
                .write(&mut *cache, &mut *out_stderr)
                .expect("Writing to a cursor should not fail")
        }
    }
}
