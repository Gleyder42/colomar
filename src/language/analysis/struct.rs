use crate::language::{ast, im};
use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::def::DefQuery;
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::interner::{Interner, IntoInternId};


pub(in super) fn query_struct(db: &dyn DefQuery, r#struct: ast::Struct) -> QueryResult<im::Struct, AnalysisError> {
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

pub(in super) fn query_struct_def(
    db: &dyn DefQuery,
    struct_def: ast::StructDefinition,
) -> QueryResult<im::StructDefinition, AnalysisError> {
    let functions = struct_def.functions.into_iter()
        .map(|function_decl| db.query_function_decl(function_decl))
        .collect::<QueryResult<Vec<_>, _>>()
        .intern_inner(db);

    let properties = struct_def.properties.into_iter()
        .map(|property_decl| db.query_property(property_decl))
        .collect::<QueryResult<Vec<_>, _>>()
        .intern_inner(db);

    functions
        .and_or_default(properties)
        .map(|(functions, properties)| {
            im::StructDefinition { functions, properties }
        })
}