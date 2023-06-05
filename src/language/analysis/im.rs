use crate::language::{ast, im};
use crate::language::analysis::QueryTrisult;
use crate::language::analysis::def::DefQuery;

pub(in super) fn query_im(db: &dyn DefQuery) -> QueryTrisult<im::Im> {
    db.input_content().into_iter()
        .map(|root| {
            match root {
                ast::Root::Event(event) => { db.query_event(event).map(im::Root::Event) }
                ast::Root::Rule(rule) => { db.query_rule_decl(rule).map(im::Root::Rule) }
                ast::Root::Enum(r#enum) => db.query_enum(r#enum).map(im::Root::Enum),
                ast::Root::Struct(r#struct) => db.query_struct(r#struct).map(im::Root::Struct),
            }
        })
        .collect::<QueryTrisult<_>>()
        .map(im::Im)
}