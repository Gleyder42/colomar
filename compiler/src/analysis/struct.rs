use super::super::analysis::decl::DeclQuery;
use super::super::analysis::def::DefQuery;
use super::super::analysis::interner::IntoInternId;
use super::super::cir::{FunctionDecl, FunctionDeclIds, PropertyDecl, PropertyDeclIds, TypeDesc};
use super::super::QueryTrisult;
use super::super::{cir, cst, FUNCTIONS_DECLS_LEN, PROPERTY_DECLS_LEN};
use hashlink::LinkedHashSet;

use super::super::trisult::Errors;
use crate::tri;
use smallvec::SmallVec;

pub(super) fn query_struct(db: &dyn DefQuery, r#struct: cst::Struct) -> QueryTrisult<cir::Struct> {
    let mut errors = Errors::default();

    let structs = db
        .query_struct_by_name(r#struct.decl.name.value)
        .complete_with_span(r#struct.decl.name.span);
    let structs = tri!(structs, errors);

    let mut decls = SmallVec::with_capacity(structs.len());
    let mut defs = SmallVec::with_capacity(structs.len());

    for r#struct in structs {
        let struct_decl_id = db.query_struct_decl(r#struct.decl);
        decls.push(struct_decl_id);

        defs.push(tri!(
            db.query_struct_def(struct_decl_id, r#struct.def),
            errors
        ));
    }

    let r#struct = cir::Struct {
        decl: decls,
        def: defs,
    };
    errors.value(r#struct)
}

pub(super) fn query_struct_decl(
    db: &dyn DeclQuery,
    r#struct: cst::StructDecl,
) -> cir::StructDeclId {
    cir::StructDecl {
        name: r#struct.name,
        is_partial: r#struct.is_partial,
        is_native: r#struct.is_native,
        generics: r#struct.generics,
    }
    .intern(db)
}

pub(super) fn query_struct_functions(
    db: &dyn DeclQuery,
    struct_decl_id: cir::StructDeclId,
    functions: cst::FunctionDecls,
) -> QueryTrisult<FunctionDeclIds> {
    let struct_decl = db.lookup_intern_struct_decl(struct_decl_id);
    let generic_names: LinkedHashSet<_> = struct_decl
        .generics
        .into_iter()
        .map(|it| it.value)
        .collect();

    functions
        .into_iter()
        .map(|function_decl| {
            db.query_function_decl(
                Some(struct_decl_id.into()),
                function_decl,
                generic_names.clone(),
            )
        })
        .collect::<QueryTrisult<SmallVec<[FunctionDecl; FUNCTIONS_DECLS_LEN]>>>()
        .intern_inner(db)
}

pub(super) fn query_struct_properties(
    db: &dyn DeclQuery,
    struct_decl_id: cir::StructDeclId,
    properties: cst::PropertyDecls,
) -> QueryTrisult<PropertyDeclIds> {
    let struct_type = TypeDesc::Struct(struct_decl_id);

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
