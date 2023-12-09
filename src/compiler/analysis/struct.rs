use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::analysis::def::DefQuery;
use crate::compiler::analysis::interner::IntoInternId;
use crate::compiler::cir::{FunctionDecl, FunctionDeclIds, PropertyDecl, PropertyDeclIds, Type};
use crate::compiler::QueryTrisult;
use crate::compiler::{cir, cst, FUNCTIONS_DECLS_LEN, PROPERTY_DECLS_LEN};

use smallvec::SmallVec;

pub(super) fn query_struct(db: &dyn DefQuery, r#struct: cst::Struct) -> QueryTrisult<cir::Struct> {
    let struct_decl = db.query_struct_decl(r#struct.decl);
    db.query_struct_def(struct_decl, r#struct.def)
        .map(|struct_def| cir::Struct {
            decl: struct_decl,
            def: struct_def,
        })
}

pub(super) fn query_struct_decl(
    db: &dyn DeclQuery,
    r#struct: cst::StructDecl,
) -> cir::StructDeclId {
    cir::StructDecl {
        name: r#struct.name,
        is_open: r#struct.is_open,
        is_native: r#struct.is_native,
    }
    .intern(db)
}

pub(super) fn query_struct_functions(
    db: &dyn DeclQuery,
    struct_decl_id: cir::StructDeclId,
    functions: cst::FunctionDecls,
) -> QueryTrisult<FunctionDeclIds> {
    functions
        .into_iter()
        .map(|function_decl| db.query_function_decl(Some(struct_decl_id.into()), function_decl))
        .collect::<QueryTrisult<SmallVec<[FunctionDecl; FUNCTIONS_DECLS_LEN]>>>()
        .intern_inner(db)
}

pub(super) fn query_struct_properties(
    db: &dyn DeclQuery,
    struct_decl_id: cir::StructDeclId,
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
    struct_decl_id: cir::StructDeclId,
    struct_def: cst::StructDef,
) -> QueryTrisult<cir::StructDef> {
    db.query_struct_functions(struct_decl_id, struct_def.functions)
        .and_or_default(db.query_struct_properties(struct_decl_id, struct_def.properties))
        .map(|(functions, properties)| cir::StructDef {
            functions,
            properties,
        })
}
