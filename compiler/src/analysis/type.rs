use crate::analysis::decl::DeclQuery;
use crate::{cir, cst, Ident};

use std::collections::HashMap;

// TODO Maybe add a projection query which excludes rule
pub(super) fn query_type_map(db: &dyn DeclQuery) -> HashMap<Ident, cir::Type> {
    db.input_content()
        .into_iter()
        .filter_map(|root| {
            match root {
                cst::Root::Event(event) => Some((
                    event.declaration.name.clone(),
                    cir::Type::Event(db.query_event_decl(event.declaration)),
                )),
                cst::Root::Enum(r#enum) => Some((
                    r#enum.declaration.name.clone(),
                    cir::Type::Enum(db.query_enum_decl(r#enum.declaration)),
                )),
                cst::Root::Struct(r#struct) => Some((
                    r#struct.declaration.name.clone(),
                    cir::Type::Struct(db.query_struct_decl(r#struct.declaration)),
                )),
                cst::Root::Rule(_) => {
                    None /* Rules have no type */
                }
            }
        })
        .collect()
}
