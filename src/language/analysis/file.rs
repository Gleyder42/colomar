use std::collections::HashMap;
use std::fmt::format;
use chumsky::prelude::todo;
use salsa::InternKey;
use crate::language::{ast, ImmutableString};
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::event::EventDeclQuery;
use crate::language::analysis::interner::Interner;
use crate::language::analysis::r#enum::EnumDeclQuery;
use crate::language::analysis::r#struct::StructDeclQuery;
use crate::language::ast::{Definition, EventDefinition, Root};
use crate::language::im::{EnumDeclarationId, EventDeclarationId, Rule, StructDeclarationId};

#[salsa::query_group(FileDatabase)]
pub trait RootFileQuery {

    #[salsa::input]
    fn input_content(&self) -> ast::Ast;
}

#[salsa::query_group(AstDefDatabase)]
pub trait AstDefQuery: Interner + RootFileQuery + EventDeclQuery + EnumDeclQuery + StructDeclQuery {

    /// Queries a map containing declaration ids and definitions.
    /// If you want to get the definition by declaration id use [AstDefQuery::query_ast_event_def]
    /// instead
    fn query_ast_def_map(&self) -> HashMap<DefKey, Definition>;

    fn query_ast_event_def_map(&self) -> HashMap<DefKey, EventDefinition>;

    /// Queries an event definition my even declaration id
    fn query_ast_event_def(&self, event_decl_id: EventDeclarationId) -> QueryResult<EventDefinition, AnalysisError>;
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum DefKey {
    Event(EventDeclarationId),
    Struct(StructDeclarationId),
    Enum(EnumDeclarationId)
}

fn query_ast_def_map(db: &dyn AstDefQuery) -> HashMap<DefKey, Definition> {
    let map: HashMap<_, _> = db.input_content().into_iter()
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

fn query_ast_event_def_map(db: &dyn AstDefQuery) -> HashMap<DefKey, EventDefinition> {
    db.query_ast_def_map().into_iter()
        .filter_map(|(id, def)| {
            let result: Result<EventDefinition, _> = def.try_into();
            match result {
                Ok(value) => Some((id, value)),
                Err(_) => None
            }
        })
        .collect()
}

fn query_ast_event_def(db: &dyn AstDefQuery, event_decl_id: EventDeclarationId) -> QueryResult<EventDefinition, AnalysisError> {
    db.query_ast_event_def_map()
        .remove(&DefKey::Event(event_decl_id))
        .ok_or_else(|| AnalysisError::CannotFindDefinition(event_decl_id.as_intern_id()))
        .into()
}