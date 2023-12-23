#![feature(result_flattening)]
#![feature(map_try_insert)]

extern crate core;
extern crate salsa;

use crate::compiler::language::lexer::{lexer, Token};
use crate::compiler::language::parser::parser;
use crate::compiler::{cst, source_cache, QueryTrisult};
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::error::RichReason;
use chumsky::prelude::*;
use compiler::database::CompilerDatabase;
use compiler::span::{FatSpan, Span, SpanSourceId};
use compiler::trisult::Trisult;
use hashlink::LinkedHashMap;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{stdout, Read, Write};
use std::path::Path;

use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::printer::PrinterQuery;

use crate::compiler::span::{CopyRange, SpanInterner, StringInterner};

pub mod compiler;
pub mod test_assert;

fn main() {
    let mut db = CompilerDatabase::default();

    let mut ast_map = LinkedHashMap::new();
    let mut source_map = HashMap::new();
    let base_base = Path::new("docs/tutorials/example/test/src");
    for path in source_cache::read_files_to_string(&base_base).unwrap() {
        let (source, ast, parser_errors) = parse_ast(&path, &db, &mut source_map);
        let i = parser_errors.len();
        print_parser_errors(source, &db, parser_errors);

        let result = path.strip_prefix(base_base).unwrap();
        let id = db.intern_string(result.file_stem().unwrap().to_string_lossy().into());
        let segments = vec![id];

        println!("{:?}, {}, {i}", path, ast.is_none());
        match ast {
            Some(ast) => {
                ast_map.insert(cst::Path { segments }, ast);
            }
            None => {}
        };
    }

    let make_path = |name: &'static str, db: &CompilerDatabase| cst::Path {
        segments: vec![db.intern_string(name.to_string())],
    };

    db.set_main_file_name(make_path("main", &db));
    db.set_secondary_files(ast_map);

    let impl_path = Path::new("docs/tutorials/example/test/native");
    let elements = compiler::loader::read_impls(impl_path);

    use crate::compiler::loader::WorkshopScriptLoader;
    db.set_input_wscript_impls(elements);

    let trisult = db.query_workshop_output();
    match trisult {
        Trisult::Ok(value) => {
            println!("{value}")
        }
        QueryTrisult::Par(_, errors) | QueryTrisult::Err(errors) => {
            // Due the nature of demand-driven compilation, the queries return duplicate errors,
            // if an erroneous query is queried multiple times.
            // However, the errors only seem to be duplicate, because they just miss context information
            // which would distinct them.
            // Filtering here while having the all errors in the result wastes space and computing power.
            // The question is if this is negligible.
            let original_len = errors.len();
            let unique_errors = errors.into_iter().collect::<HashSet<_>>();
            let new_len = unique_errors.len();

            println!("Errors: {:?}", unique_errors);
            println!(
                "Reduced errors from {} to {}. \nReduced size by {}",
                original_len,
                new_len,
                100.0 - (new_len as f32 / original_len as f32) * 100.0
            );

            compiler::error_reporter::print_errors(&source_map, &mut db, unique_errors);
        }
    }
}

fn parse_ast<'a>(
    path: &'a Path,
    db: &'a CompilerDatabase,
    source_map: &'a mut HashMap<SpanSourceId, String>,
) -> (&'a String, Option<cst::Ast>, Vec<Rich<'a, Token, Span>>) {
    let mut file = fs::File::open(path).expect("Cannot read from file");

    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Cannot read file content");

    let span_source_id: SpanSourceId = db.intern_span_source(path.to_string_lossy().into());

    let (tokens, lexer_errors) = lexer(span_source_id, db)
        .parse(source.as_str())
        .into_output_errors();

    let out = compiler::loader::error_reporter::to_stdout_buffer(lexer_errors, source.as_str());
    stdout().write(&out).unwrap();

    source_map.insert(span_source_id, source);
    let source = source_map.get(&span_source_id).unwrap();

    if let Some(tokens) = tokens {
        let eoi = Span::new(
            span_source_id,
            CopyRange::from(tokens.len()..tokens.len() + 1),
        );
        let stream = chumsky::input::Stream::from_iter(tokens.into_iter()).spanned(eoi);
        let (ast, parser_errors) = parser().parse(stream).into_output_errors();
        (source, ast, parser_errors)
    } else {
        (source, None, Vec::new())
    }
}

fn print_parser_errors(
    source: &String,
    db: &CompilerDatabase,
    parser_errors: Vec<Rich<Token, Span>>,
) {
    for parser_error in parser_errors {
        let whole_span = FatSpan::from_span(db, *parser_error.span());
        let builder = Report::build(
            ReportKind::Error,
            whole_span.source.clone(),
            whole_span.location.start() as usize,
        );

        let builder = match parser_error.reason() {
            RichReason::ExpectedFound { found, expected } => builder
                .with_message(format!("Unexpected token '{:?}'", found))
                .with_label(
                    Label::new(whole_span.clone())
                        .with_color(Color::Red)
                        .with_message(format!("Expected: {:?}", expected)),
                ),
            RichReason::Custom(message) => {
                let span = FatSpan::from_span(db, *parser_error.span());

                builder
                    .with_message(message)
                    .with_label(Label::new(span).with_color(Color::Cyan))
            }
            RichReason::Many(_) => unimplemented!(),
        };

        builder
            .finish()
            .eprint(sources(vec![(whole_span.source.clone(), source)]))
            .unwrap();
    }
}
