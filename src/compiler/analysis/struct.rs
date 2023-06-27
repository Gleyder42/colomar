use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::analysis::def::DefQuery;
use crate::compiler::analysis::interner::IntoInternId;
use crate::compiler::cir::{
    FunctionDecl, FunctionDeclIds, PropertyDecl, PropertyDeclIds, StructDeclarationId, Type,
};
use crate::compiler::QueryTrisult;
use crate::compiler::{cir, cst, FUNCTIONS_DECLS_LEN, PROPERTY_DECLS_LEN};

use smallvec::SmallVec;

pub(super) fn query_struct(db: &dyn DefQuery, r#struct: cst::Struct) -> QueryTrisult<cir::Struct> {
    let struct_decl = db.query_struct_decl(r#struct.declaration);
    db.query_struct_def(struct_decl, r#struct.definition)
        .map(|struct_def| cir::Struct {
            decl: struct_decl,
            def: struct_def,
        })
}

pub(super) fn query_struct_decl(
    db: &dyn DeclQuery,
    r#struct: cst::StructDeclaration,
) -> cir::StructDeclarationId {
    cir::StructDeclaration {
        name: r#struct.name,
        is_open: r#struct.is_open,
        is_native: r#struct.is_native,
    }
    .intern(db)
}

pub(super) fn query_struct_functions(
    db: &dyn DeclQuery,
    functions: cst::FunctionDecls,
) -> QueryTrisult<FunctionDeclIds> {
    functions
        .into_iter()
        .map(|function_decl| db.query_function_decl(function_decl))
        .collect::<QueryTrisult<SmallVec<[FunctionDecl; FUNCTIONS_DECLS_LEN]>>>()
        .intern_inner(db)
}

pub(super) fn query_struct_properties(
    db: &dyn DeclQuery,
    struct_decl_id: StructDeclarationId,
    properties: cst::PropertyDecls,
) -> QueryTrisult<PropertyDeclIds> {
    let struct_type = Type::Struct(struct_decl_id);

    properties
        .into_iter()
        .map(|property_decl| db.query_property(Some(struct_type), property_decl))
        .collect::<QueryTrisult<SmallVec<[PropertyDecl; PROPERTY_DECLS_LEN]>>>()
        .intern_inner(db)
}

pub(super) fn query_struct_def(
    db: &dyn DefQuery,
    struct_decl_id: StructDeclarationId,
    struct_def: cst::StructDefinition,
) -> QueryTrisult<cir::StructDefinition> {
    db.query_struct_functions(struct_def.functions)
        .and_or_default(db.query_struct_properties(struct_decl_id, struct_def.properties))
        .map(|(functions, properties)| cir::StructDefinition {
            functions,
            properties,
        })
}
