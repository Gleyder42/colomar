use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::cir::{EnumDeclarationId, EventDeclarationId, StructDeclarationId};
use crate::compiler::cst::{Definition, EventDefinition, Root, StructDefinition};
use crate::compiler::error::CompilerError;

use crate::compiler::QueryTrisult;
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

pub(super) fn query_ast_def_map(db: &dyn DeclQuery) -> HashMap<DefKey, Definition> {
    let map: HashMap<_, _> = db
        .input_content()
        .into_iter()
        .filter_map(|root| match root {
            Root::Event(event) => {
                let event_decl_id = db.query_event_decl(event.declaration);
                println!("Event {:?}", event_decl_id);
                Some((DefKey::Event(event_decl_id), event.definition.into()))
            }
            Root::Enum(r#enum) => {
                let enum_decl_id = db.query_enum_decl(r#enum.declaration);
                println!("Enum {:?}", enum_decl_id);
                Some((DefKey::Enum(enum_decl_id), r#enum.definition.into()))
            }
            Root::Struct(r#struct) => {
                let struct_decl_id = db.query_struct_decl(r#struct.declaration);
                println!("Struct {:?}", struct_decl_id);
                Some((DefKey::Struct(struct_decl_id), r#struct.definition.into()))
            }
            Root::Rule(_) => {
                /* Rules don't need to be queried */
                None
            }
        })
        .collect();
    println!("{}", map.len());
    dbg!(map)
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
