use crate::language::analysis::decl::DeclQuery;
use crate::language::{ast, im, Ident};
use std::collections::HashMap;

// TODO Maybe add a projection query which excludes rule
pub(super) fn query_type_map(db: &dyn DeclQuery) -> HashMap<Ident, im::Type> {
    db.input_content()
        .into_iter()
        .filter_map(|root| {
            match root {
                ast::Root::Event(event) => Some((
                    event.declaration.name.clone(),
                    im::Type::Event(db.query_event_decl(event.declaration)),
                )),
                ast::Root::Enum(r#enum) => Some((
                    r#enum.declaration.name.clone(),
                    im::Type::Enum(db.query_enum_decl(r#enum.declaration)),
                )),
                ast::Root::Struct(r#struct) => Some((
                    r#struct.declaration.name.clone(),
                    im::Type::Struct(db.query_struct_decl(r#struct.declaration)),
                )),
                ast::Root::Rule(_) => {
                    None /* Rules have no type */
                }
            }
        })
        .collect()
}
