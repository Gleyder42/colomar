use ariadne::{Label, Report, ReportBuilder, ReportKind, Source, Span};
use chumsky::error::{Rich, RichPattern, RichReason};
use std::fmt::Debug;
use std::ops::Range;

/// Converts the errors to a visual representation intended to be used with the output buffer.
/// Note that this function does not return a String and makes therefore no promise it the buffer contains valid UTF-8.
pub fn to_stdout_buffer<T: Debug>(errors: Vec<Rich<T>>, source: &str) -> Vec<u8> {
    let mut buffer: Vec<u8> = Vec::new();
    const SOURCE_ID: &'static str = "workshop";
    for error in errors {
        let span = error.span().into_range();
        let report = Report::build(ReportKind::Error, (), span.start());
        let report = to_report(report, error.reason(), span);

        report
            .finish()
            .write_for_stdout(Source::from(source), &mut buffer)
            .expect("A string write should not fail");
    }
    buffer
}

pub fn to_report<'a, T: Debug, S: Span + Clone>(
    report: ReportBuilder<'a, S>,
    reason: &RichReason<'a, T>,
    span: S,
) -> ReportBuilder<'a, S> {
    match reason {
        RichReason::ExpectedFound { found, expected } => {
            let message = match found {
                Some(token) => {
                    format!("Found {token:?}")
                }
                None => "No token".to_string(),
            };

            let mut report = report.with_label(Label::new(span.clone()).with_message(message));

            for pattern in expected {
                match pattern {
                    RichPattern::Token(token) => {
                        report = report.with_label(
                            Label::new(span.clone())
                                .with_message(format!("Expected token {token:?}")),
                        );
                    }
                    RichPattern::Label(label) => {
                        report = report.with_label(
                            Label::new(span.clone())
                                .with_message(format!("Expected token {label:?}")),
                        );
                    }
                    RichPattern::EndOfInput => {
                        report = report
                            .with_label(Label::new(span.clone()).with_message("Expected no token"));
                    }
                }
            }
            report
        }
        RichReason::Custom(custom) => {
            report.with_label(Label::new(span.clone()).with_message(custom))
        }
        RichReason::Many(errors) => {
            let mut report = report;
            for reason in errors {
                report = to_report(report, reason, span.clone());
            }
            report
        }
    }
}
