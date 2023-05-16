#![feature(box_patterns)]
#![feature(result_flattening)]
#![feature(map_try_insert)]

extern crate core;
extern crate salsa;

use std::fs;
use std::io::{Read};
use std::ops::Range;
use std::path::Path;
use chumsky::prelude::*;
use chumsky::Stream;
use crate::language::analysis::AnalysisDatabase;
use crate::language::analysis::error::QueryResult;
use crate::language::analysis::interner::Interner;
use crate::language::im;
use crate::language::im::{DeclaredArgument, Root, StructDeclaration};
use crate::language::lexer::{lexer};
use crate::language::parser::parser;
use crate::language::analysis::file::RootFileQuery;
use crate::language::analysis::im::Im;

pub type Span = Range<usize>;

pub mod workshop;
pub mod language;
pub mod test_assert;
mod compiler;

fn main() {
    let filename = "v4.colo";
    let filepath = format!("dsl/example/{filename}");
    let path = Path::new(&filepath);
    let mut file = fs::File::open(path).expect("Cannot read from file");

    let mut source = String::new();
    file.read_to_string(&mut source).expect("Cannot read file content");

    let (tokens, lexer_errors) = lexer().parse_recovery(source.as_str());
    println!("{:#?}", lexer_errors);

    let (ast, parser_errors) = if let Some(tokens) = tokens {
        let stream = Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter());
        parser().parse_recovery(stream)
    } else {
        (None, Vec::new())
    };
    println!("{:#?}", parser_errors);

    if let Some(ast) = ast {
        println!("{:#?}", ast);
        let mut database = AnalysisDatabase::default();
        database.set_input_content(ast);

        let im: QueryResult<im::Im, _> = database.query_im();

        let output = im.to_option();

        println!("{:#?}", output.1);

        if let Some(im) = output.0 {
            for root in im {
                match root {
                    Root::Rule(_rule) => todo!(),
                    Root::Event(event) => {
                        let decl = database.lookup_intern_event_decl(event.declaration);


                        let vec = event.definition.arguments.into_iter()
                            .map(|it| database.lookup_intern_decl_arg(it))
                            .collect::<Vec<DeclaredArgument>>();

                        println!("Event\nDecl: {:#?}\nDef: {:#?}, Proprs: {:#?}", decl, vec, event.definition.properties);
                    }
                    Root::Enum(r#enum) => {
                        let decl = database.lookup_intern_enum_decl(r#enum.declaration);

                        let constants: Vec<_> = r#enum.definition.constants.into_iter()
                            .map(|it| database.lookup_intern_enum_constant(it))
                            .collect();

                        println!("Enum\nDecl: {:#?}\nDef: {:#?}\nSpan: {:#?}", decl, constants, r#enum.span);
                    },
                    Root::Struct(r#struct) => {
                        let struct_decl: StructDeclaration = database.lookup_intern_struct_decl(r#struct.decl);

                        println!("Struct {:#?}", struct_decl);
                    },
                };
                println!("===")
            }
        }

        println!("{:#?}", output.1);
    }
}
