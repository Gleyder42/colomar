use crate::language::analysis::file::RootFileQuery;
use crate::language::analysis::r#enum::EnumQuery;
use crate::language::{ast, im};
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::event::EventQuery;
use crate::language::analysis::r#struct::StructDeclQuery;
use crate::language::im::StructDefinition;

#[salsa::query_group(ImDatabase)]
pub trait Im: EnumQuery + EventQuery + StructDeclQuery + RootFileQuery {

    fn query_im(&self) -> QueryResult<im::Im, AnalysisError>;
}

fn query_im(db: &dyn Im) -> QueryResult<im::Im, AnalysisError> {
    db.input_content().into_iter()
        .map(|root| {
            match root {
                ast::Root::Event(event) => { db.query_event(event).map(im::Root::Event) }
                ast::Root::Rule(_) => { todo!() }
                ast::Root::Enum(r#enum) => db.query_enum(r#enum).map(im::Root::Enum),
                ast::Root::Struct(r#struct) => {
                    let struct_definition = StructDefinition {
                        decl: db.query_struct_decl(r#struct.declaration),
                        properties: Vec::new(),
                        functions: Vec::new()
                    };
                    QueryResult::Ok(im::Root::Struct(struct_definition))
                }
            }
        })
        .collect::<QueryResult<Vec<_>, _>>()
        .map(im::Im)
}