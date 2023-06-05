use crate::language::{ast, im};
use crate::language::analysis::QueryTrisult;
use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::def::DefQuery;
use crate::language::analysis::interner::IntoInternId;
use crate::language::im::{FunctionDeclId, PropertyDeclId};


pub(in super) fn query_struct(db: &dyn DefQuery, r#struct: ast::Struct) -> QueryTrisult<im::Struct> {
    let struct_decl = db.query_struct_decl(r#struct.declaration);
    db.query_struct_def(r#struct.definition)
        .map(|struct_def| im::Struct {
            decl: struct_decl,
            def: struct_def,
        })
}

pub(in super) fn query_struct_decl(db: &dyn DeclQuery, r#struct: ast::StructDeclaration) -> im::StructDeclarationId {
    im::StructDeclaration {
        name: r#struct.name,
        is_open: r#struct.is_open,
        is_workshop: r#struct.is_workshop,
    }.intern(db)
}

pub(in super) fn query_struct_functions(
    db: &dyn DeclQuery,
    functions: Vec<ast::FunctionDeclaration>
) -> QueryTrisult<Vec<FunctionDeclId>> {
    functions.into_iter()
        .map(|function_decl| db.query_function_decl(function_decl))
        .collect::<QueryTrisult<_>>()
        .intern_inner(db)
}

pub(in super) fn query_struct_properties(
    db: &dyn DeclQuery,
    properties: Vec<ast::PropertyDeclaration>
) -> QueryTrisult<Vec<PropertyDeclId>> {
    properties.into_iter()
        .map(|property_decl| db.query_property(property_decl))
        .collect::<QueryTrisult<_>>()
        .intern_inner(db)
}

pub(in super) fn query_struct_def(
    db: &dyn DefQuery,
    struct_def: ast::StructDefinition,
) -> QueryTrisult<im::StructDefinition> {
    db.query_struct_functions(struct_def.functions)
        .and_or_default(db.query_struct_properties(struct_def.properties))
        .map(|(functions, properties)| {
            im::StructDefinition { functions, properties }
        })
}