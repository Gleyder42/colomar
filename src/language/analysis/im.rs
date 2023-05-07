use crate::language::analysis::file::RootFileQuery;
use crate::language::analysis::r#enum::EnumQuery;
use crate::language::{ast, im};
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::event::EventQuery;

#[salsa::query_group(ImDatabase)]
pub trait Im: EnumQuery + EventQuery + RootFileQuery {

    fn query_im(&self) -> QueryResult<im::Im, AnalysisError>;
}

fn query_im(db: &dyn Im) -> QueryResult<im::Im, AnalysisError> {
    db.input_content().into_iter()
        .map(|root| {
            match root {
                ast::Root::Event(event) => { db.query_event(event).map(im::Root::Event) }
                ast::Root::Rule(_) => { todo!() }
                ast::Root::Enum(r#enum) => QueryResult::Ok(db.query_enum(r#enum)).map(im::Root::Enum),
                ast::Root::Struct(_) => { todo!() }
            }
        })
        .collect::<QueryResult<Vec<_>, _>>()
        .map(im::Im)
}