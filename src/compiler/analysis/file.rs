use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::cir::{EnumDeclarationId, EventDeclarationId, StructDeclarationId};
use crate::compiler::cst::{
    Definition, EventDefinition, Root, StructDefinition, TypeRoot, Visibility,
};
use crate::compiler::error::CompilerError;

use crate::compiler::trisult::{Errors, IntoTrisult};
use crate::compiler::{cst, QueryTrisult};
use crate::tri;
use either::Either;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum DefKey {
    Event(EventDeclarationId),
    Struct(StructDeclarationId),
    Enum(EnumDeclarationId),
}

pub(super) fn query_ast_struct_def_map(db: &dyn DeclQuery) -> HashMap<DefKey, StructDefinition> {
    db.query_ast_def_map()
        .into_iter()
        .filter_map(|(id, def)| {
            let result: Result<StructDefinition, _> = def.try_into();
            match result {
                Ok(value) => Some((id, value)),
                Err(_) => None,
            }
        })
        .collect()
}

pub(super) fn query_ast_struct_def(
    db: &dyn DeclQuery,
    struct_decl_id: StructDeclarationId,
) -> QueryTrisult<StructDefinition> {
    db.query_ast_struct_def_map()
        .remove(&DefKey::Struct(struct_decl_id))
        .ok_or_else(|| CompilerError::CannotFindDefinition(Either::Left(struct_decl_id)))
        .into()
}

pub(super) fn query_main_file(db: &dyn DeclQuery) -> cst::Ast {
    db.query_file(db.main_file_name(), false)
        .expect_ok("There should always be a main file present")
}

pub(super) fn query_action_items(db: &dyn DeclQuery) -> Vec<Root> {
    db.query_main_file()
        .into_iter()
        .filter(|root| match root {
            Root::Rule(_) | Root::Struct(_) | Root::Event(_) | Root::Enum(_) => true,
            Root::Import(_) => false,
        })
        .collect()
}

pub(super) fn query_file(
    db: &dyn DeclQuery,
    path: cst::Path,
    include_only_public: bool,
) -> QueryTrisult<cst::Ast> {
    let mut errors = Errors::default();
    let mut ast: cst::Ast = tri!(db.query_secondary_file(path), errors);

    let import_indices: Vec<_> = ast
        .0
        .iter()
        .enumerate()
        .filter_map(|(index, element)| match element {
            Root::Import(_) => Some(index),
            _ => None,
        })
        .collect();

    let imports: QueryTrisult<Vec<cst::Ast>> = import_indices
        .into_iter()
        .filter_map(|index| match ast.0.swap_remove(index) {
            Root::Import(import) => Some(import),
            _ => None,
        })
        .map(|import| db.query_file(import.path, true))
        .collect();
    let imported_asts = tri!(imports, errors);
    let element_count = imported_asts.iter().map(|ast| ast.0.len()).sum();

    if include_only_public {
        ast.0
            .retain(|element| element.visibility() == Visibility::Public);
    }

    ast.0.reserve_exact(element_count);
    for mut imported in imported_asts {
        ast.0.append(&mut imported.0);
    }

    errors.value(ast)
}

pub(super) fn query_secondary_file(db: &dyn DeclQuery, path: cst::Path) -> QueryTrisult<cst::Ast> {
    db.secondary_files()
        .remove(&path)
        .trisult_ok_or(CompilerError::CannotFindFile(path))
}

pub(super) fn query_type_items(db: &dyn DeclQuery) -> Vec<TypeRoot> {
    db.query_main_file()
        .into_iter()
        .filter_map(|root| match root {
            Root::Event(event) => Some(TypeRoot::Event(event)),
            Root::Enum(r#enum) => Some(TypeRoot::Enum(r#enum)),
            Root::Struct(r#struct) => Some(TypeRoot::Struct(r#struct)),
            _ => None,
        })
        .collect()
}

pub(super) fn query_ast_def_map(db: &dyn DeclQuery) -> HashMap<DefKey, Definition> {
    let map: HashMap<_, _> = db
        .query_type_items()
        .into_iter()
        .filter_map(|root| match root {
            TypeRoot::Event(event) => {
                let event_decl_id = db.query_event_decl(event.declaration);
                println!("Event {:?}", event_decl_id);
                Some((DefKey::Event(event_decl_id), event.definition.into()))
            }
            TypeRoot::Enum(r#enum) => {
                let enum_decl_id = db.query_enum_decl(r#enum.declaration);
                println!("Enum {:?}", enum_decl_id);
                Some((DefKey::Enum(enum_decl_id), r#enum.definition.into()))
            }
            TypeRoot::Struct(r#struct) => {
                let struct_decl_id = db.query_struct_decl(r#struct.declaration);
                println!("Struct {:?}", struct_decl_id);
                Some((DefKey::Struct(struct_decl_id), r#struct.definition.into()))
            }
        })
        .collect();
    map
}

pub(super) fn query_ast_event_def_map(db: &dyn DeclQuery) -> HashMap<DefKey, EventDefinition> {
    db.query_ast_def_map()
        .into_iter()
        .filter_map(|(id, def)| {
            let result: Result<EventDefinition, _> = def.try_into();
            match result {
                Ok(value) => Some((id, value)),
                Err(_) => None,
            }
        })
        .collect()
}

pub(super) fn query_ast_event_def(
    db: &dyn DeclQuery,
    event_decl_id: EventDeclarationId,
) -> QueryTrisult<EventDefinition> {
    db.query_ast_event_def_map()
        .remove(&DefKey::Event(event_decl_id))
        .ok_or_else(|| CompilerError::CannotFindDefinition(Either::Right(event_decl_id)))
        .into()
}
