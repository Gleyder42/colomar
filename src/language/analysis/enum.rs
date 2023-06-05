use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::interner::IntoInternId;
use crate::language::analysis::{AnalysisError, QueryTrisult};
use crate::language::error::Trisult;
use crate::language::im::{EnumConstant, EnumDeclarationId};
use crate::language::{ast, im, Ident};
use std::collections::HashMap;
use std::rc::Rc;

pub(super) fn query_enum_ast(
    db: &dyn DeclQuery,
    enum_decl_id: EnumDeclarationId,
) -> Result<ast::Enum, AnalysisError> {
    db.query_enum_ast_map()
        .get(&enum_decl_id)
        .map(|it| it.clone())
        .ok_or_else(|| {
            let enum_decl: im::EnumDeclaration = db.lookup_intern_enum_decl(enum_decl_id);
            AnalysisError::CannotFindIdent(enum_decl.name)
        })
}

pub(super) fn query_enum_ast_map(db: &dyn DeclQuery) -> HashMap<EnumDeclarationId, ast::Enum> {
    db.input_content()
        .into_iter()
        .filter_map(|it| {
            if let ast::Root::Enum(r#enum) = it {
                Some((db.query_enum_decl(r#enum.declaration.clone()), r#enum))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>()
}

pub(super) fn query_enum_def(
    db: &dyn DeclQuery,
    enum_decl_id: EnumDeclarationId,
) -> QueryTrisult<im::Enum> {
    db.query_enum_ast(enum_decl_id)
        .map(|enum_ast| db.query_enum(enum_ast))
        .into()
}

fn no_duplicates(constants: Vec<EnumConstant>) -> QueryTrisult<Vec<EnumConstant>> {
    let mut constants_map = HashMap::new();
    let mut duplicates = Vec::new();

    for enum_constant in constants {
        let constant_name = enum_constant.name.value.clone();

        let result = constants_map.try_insert(constant_name, enum_constant.clone());
        // Returns Err if the constants is already present in the constants_map
        if let Err(error) = result {
            let first = Ident {
                value: Rc::clone(error.entry.key()),
                span: error.entry.get().name.span.clone(),
            };

            duplicates.push(AnalysisError::DuplicateIdent {
                first,
                second: enum_constant.name.clone(),
            })
        }
    }
    let unique_constants = constants_map.into_values().collect::<Vec<EnumConstant>>();

    Trisult::from((unique_constants, duplicates))
}

pub(super) fn query_enum(db: &dyn DeclQuery, r#enum: ast::Enum) -> QueryTrisult<im::Enum> {
    let declaration = db.query_enum_decl(r#enum.declaration);

    let constants: Vec<_> = r#enum
        .definition
        .constants
        .into_iter()
        .map(|name| EnumConstant {
            name,
            r#enum: declaration,
        })
        .collect();

    no_duplicates(constants)
        .intern_inner(db)
        .map(|constants| im::Enum {
            declaration,
            definition: im::EnumDefinition { constants },
            span: r#enum.span,
        })
}

pub(super) fn query_enum_decl(
    db: &dyn DeclQuery,
    r#enum: ast::EnumDeclaration,
) -> im::EnumDeclarationId {
    im::EnumDeclaration {
        name: r#enum.name,
        is_workshop: r#enum.is_workshop,
    }
    .intern(db)
}
