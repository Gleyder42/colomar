use crate::analysis::interner::Interner;
use crate::InternedName;
use ariadne::{Label, Report, ReportBuilder, ReportKind, Source, Span};
use chumsky::error::{Rich, RichPattern, RichReason};
use std::fmt::Debug;

/// Converts the errors to a visual representation intended to be used with the output buffer.
/// Note that this function does not return a String and makes therefore no promise it the buffer contains valid UTF-8.
pub fn to_stdout_buffer<T: Debug + InternedName>(
    errors: Vec<Rich<T>>,
    source: &str,
    interner: &dyn Interner,
) -> Vec<u8> {
    let mut buffer: Vec<u8> = Vec::new();
    for error in errors {
        let span = error.span().into_range();
        let report = Report::build(ReportKind::Error, (), span.start());
        let report = to_report(report, error.reason(), span, interner);

        report
            .finish()
            .write_for_stdout(Source::from(source), &mut buffer)
            .expect("A string write should not fail");
    }
    buffer
}

pub fn to_report<'a, T: Debug + InternedName, S: Span + Clone>(
    report: ReportBuilder<'a, S>,
    reason: &RichReason<'a, T>,
    span: S,
    interner: &dyn Interner,
) -> ReportBuilder<'a, S> {
    match reason {
        RichReason::ExpectedFound { found, expected } => {
            let message = match found {
                Some(token) => {
                    format!("Found {}", token.name(interner))
                }
                None => "No token".to_string(),
            };

            let mut report = report.with_message(message);

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
                                .with_message(format!("Expected label {label:?}")),
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
                report = to_report(report, reason, span.clone(), interner);
            }
            report
        }
    }
}
