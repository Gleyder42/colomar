use std::collections::HashMap;
use crate::language::{ast, Ident, im};
use crate::language::analysis::error::AnalysisError;
use crate::language::analysis::event::{EventDeclQuery};
use crate::language::analysis::file::RootFileQuery;
use crate::language::analysis::r#enum::{EnumDeclQuery};
use crate::language::analysis::r#struct::StructDeclQuery;

#[salsa::query_group(TypeDatabase)]
pub trait TypeQuery: RootFileQuery + EnumDeclQuery + EventDeclQuery + StructDeclQuery {

    /// Queries the type map.
    /// If you want to find a type by ident, use [query_type] instead.
    fn query_type_map(&self) -> HashMap<Ident, im::Type>;
}

// Maybe add a projection query which excludes rule
fn query_type_map(db: &dyn TypeQuery) -> HashMap<Ident, im::Type> {
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