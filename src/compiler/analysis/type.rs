use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::{cir, cst, Ident};

use std::collections::HashMap;

pub(super) fn query_type_map(db: &dyn DeclQuery) -> HashMap<Ident, cir::Type> {
    db.query_type_items()
        .into_iter()
        .filter_map(|root| match root {
            cst::TypeRoot::Event(event) => Some((
                event.decl.name.clone(),
                cir::Type::Event(db.query_event_decl(event.decl)),
            )),
            cst::TypeRoot::Enum(r#enum) => Some((
                r#enum.decl.name.clone(),
                cir::Type::Enum(db.query_enum_decl(r#enum.decl)),
            )),
            cst::TypeRoot::Struct(r#struct) => Some((
                r#struct.decl.name.clone(),
                cir::Type::Struct(db.query_struct_decl(r#struct.decl)),
            )),
        })
        .collect()
}
