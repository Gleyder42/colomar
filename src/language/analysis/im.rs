use crate::language::analysis::file::RootFileQuery;
use crate::language::analysis::r#enum::EnumQuery;
use crate::language::{ast, im};
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::event::EventQuery;
use crate::language::analysis::r#struct::{StructDeclQuery, StructDefQuery, StructQuery};
use crate::language::analysis::rule::RuleQuery;
use crate::language::im::{Struct, StructDefinition};

#[salsa::query_group(ImDatabase)]
pub trait Im: EnumQuery + EventQuery + StructQuery + RuleQuery +  RootFileQuery {

    fn query_im(&self) -> QueryResult<im::Im, AnalysisError>;
}

fn query_im(db: &dyn Im) -> QueryResult<im::Im, AnalysisError> {
    db.input_content().into_iter()
        .map(|root| {
            match root {
                ast::Root::Event(event) => { db.query_event(event).map(im::Root::Event) }
                ast::Root::Rule(rule) => { db.query_rule_decl(rule).map(im::Root::Rule) }
                ast::Root::Enum(r#enum) => db.query_enum(r#enum).map(im::Root::Enum),
                ast::Root::Struct(r#struct) => db.query_struct(r#struct).map(im::Root::Struct),
            }
        })
        .collect::<QueryResult<Vec<_>, _>>()
        .map(im::Im)
}