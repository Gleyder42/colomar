use std::collections::HashMap;
use crate::language::{ast, Ident, im};
use crate::language::analysis::decl::DeclQuery;

// Maybe add a projection query which excludes rule
pub(in super) fn query_type_map(db: &dyn DeclQuery) -> HashMap<Ident, im::Type> {
    db.input_content().into_iter()
        .filter_map(|root| {
            match root {
                ast::Root::Event(event) => (
                    event.declaration.name.clone(),
                    im::Type::Event(db.query_event_decl(event.declaration))
                ).into(),
                ast::Root::Enum(r#enum) => (
                    r#enum.declaration.name.clone(),
                    im::Type::Enum(db.query_enum_decl(r#enum.declaration))
                ).into(),
                ast::Root::Struct(r#struct) => (
                    r#struct.declaration.name.clone(),
                    im::Type::Struct(db.query_struct_decl(r#struct.declaration))
                ).into(),
                ast::Root::Rule(_) => { None /* Rules have no type */ }
            }
        }).collect()
}