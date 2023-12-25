use super::super::analysis::decl::DeclQuery;
use super::super::analysis::interner::IntoInternId;
use super::super::cir::{EnumConstant, EnumConstants, EnumDeclId};
use super::super::error::CompilerError;
use super::super::trisult::Trisult;
use super::super::{cir, cst, Ident, QueryTrisult};

use std::collections::HashMap;

pub(super) fn query_enum_ast(
    db: &dyn DeclQuery,
    enum_decl_id: EnumDeclId,
) -> Result<cst::Enum, CompilerError> {
    db.query_enum_ast_map()
        .get(&enum_decl_id)
        .cloned()
        .ok_or_else(|| {
            let enum_decl: cir::EnumDecl = db.lookup_intern_enum_decl(enum_decl_id);
            CompilerError::CannotFindIdent(enum_decl.name)
        })
}

pub(super) fn query_enum_ast_map(db: &dyn DeclQuery) -> HashMap<EnumDeclId, cst::Enum> {
    db.query_main_file()
        .into_iter()
        .filter_map(|it| {
            if let cst::Root::Enum(r#enum) = it {
                Some((db.query_enum_decl(r#enum.decl.clone()), r#enum))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>()
}

pub(super) fn query_enum_def(
    db: &dyn DeclQuery,
    enum_decl_id: EnumDeclId,
) -> QueryTrisult<cir::Enum> {
    db.query_enum_ast(enum_decl_id)
        .map(|enum_ast| db.query_enum(enum_ast))
        .into()
}

fn no_duplicates(constants: EnumConstants) -> QueryTrisult<EnumConstants> {
    let mut constants_map = HashMap::new();
    let mut duplicates = Vec::new();

    for enum_constant in constants {
        let constant_name = enum_constant.name.value.clone();

        let result = constants_map.try_insert(constant_name, enum_constant.clone());
        // Returns Err if the constants is already present in the constants_map
        if let Err(error) = result {
            let first = Ident {
                value: error.entry.key().clone(),
                span: error.entry.get().name.span,
            };

            duplicates.push(CompilerError::DuplicateIdent {
                first,
                second: enum_constant.name.clone(),
            })
        }
    }
    let unique_constants = constants_map.into_values().collect::<EnumConstants>();

    Trisult::from((unique_constants, duplicates))
}

pub(super) fn query_enum(db: &dyn DeclQuery, r#enum: cst::Enum) -> QueryTrisult<cir::Enum> {
    let decl = db.query_enum_decl(r#enum.decl);

    let constants: EnumConstants = r#enum
        .def
        .constants
        .into_iter()
        .map(|name| EnumConstant { name, r#enum: decl })
        .collect();

    no_duplicates(constants)
        .intern_inner(db)
        .map(|constants| cir::Enum {
            decl,
            def: cir::EnumDef { constants },
            span: r#enum.span,
        })
}

pub(super) fn query_enum_decl(db: &dyn DeclQuery, r#enum: cst::EnumDecl) -> cir::EnumDeclId {
    cir::EnumDecl {
        name: r#enum.name,
        is_native: r#enum.is_native,
    }
    .intern(db)
}
