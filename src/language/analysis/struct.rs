use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::def::DefQuery;
use crate::language::analysis::interner::IntoInternId;
use crate::language::analysis::QueryTrisult;
use crate::language::im::{FunctionDecl, FunctionDeclIds, PropertyDecl, PropertyDeclIds};
use crate::language::{ast, im, FUNCTIONS_DECLS_LEN, PROPERTY_DECLS_LEN};
use smallvec::SmallVec;

pub(super) fn query_struct(db: &dyn DefQuery, r#struct: ast::Struct) -> QueryTrisult<im::Struct> {
    let struct_decl = db.query_struct_decl(r#struct.declaration);
    db.query_struct_def(r#struct.definition)
        .map(|struct_def| im::Struct {
            decl: struct_decl,
            def: struct_def,
        })
}

pub(super) fn query_struct_decl(
    db: &dyn DeclQuery,
    r#struct: ast::StructDeclaration,
) -> im::StructDeclarationId {
    im::StructDeclaration {
        name: r#struct.name,
        is_open: r#struct.is_open,
        is_native: r#struct.is_native,
    }
    .intern(db)
}

pub(super) fn query_struct_functions(
    db: &dyn DeclQuery,
    functions: ast::FunctionDecls,
) -> QueryTrisult<FunctionDeclIds> {
    functions
        .into_iter()
        .map(|function_decl| db.query_function_decl(function_decl))
        .collect::<QueryTrisult<SmallVec<[FunctionDecl; FUNCTIONS_DECLS_LEN]>>>()
        .intern_inner(db)
}

pub(super) fn query_struct_properties(
    db: &dyn DeclQuery,
    properties: ast::PropertyDecls,
) -> QueryTrisult<PropertyDeclIds> {
    properties
        .into_iter()
        .map(|property_decl| db.query_property(property_decl))
        .collect::<QueryTrisult<SmallVec<[PropertyDecl; PROPERTY_DECLS_LEN]>>>()
        .intern_inner(db)
}

pub(super) fn query_struct_def(
    db: &dyn DefQuery,
    struct_def: ast::StructDefinition,
) -> QueryTrisult<im::StructDefinition> {
    db.query_struct_functions(struct_def.functions)
        .and_or_default(db.query_struct_properties(struct_def.properties))
        .map(|(functions, properties)| im::StructDefinition {
            functions,
            properties,
        })
}
