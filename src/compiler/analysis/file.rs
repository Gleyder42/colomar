use super::super::analysis::decl::DeclQuery;
use super::super::cir::{EnumDeclId, EventDeclId};
use super::super::cst::{Def, EventDef, Root, StructDef, TypeRoot, Visibility};
use super::super::error::CompilerError;

use super::super::trisult::{Errors, IntoTrisult};
use super::super::{cst, QueryTrisult, SVMultiMap, SVMultiMapWrapper, StructId, TextId};
use crate::tri;
use smallvec::SmallVec;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum DefKey {
    Event(EventDeclId),
    Struct(StructId),
    Enum(EnumDeclId),
}

pub(super) fn query_structs(
    db: &dyn DeclQuery,
) -> QueryTrisult<HashMap<TextId, SmallVec<[cst::Struct; 1]>>> {
    let ast = db.query_main_file();

    let struct_decls: Vec<_> = ast
        .into_iter()
        .filter_map(|element| match element {
            Root::Struct(r#struct) => Some((r#struct.decl.name.value, r#struct)),
            _ => None,
        })
        .collect();

    let mut struct_map = HashMap::new();
    for (text, r#struct) in struct_decls {
        struct_map
            .entry(text)
            .or_insert(SmallVec::new())
            .push(r#struct);
    }

    QueryTrisult::Ok(struct_map)
}

pub(super) fn query_struct_by_name(
    db: &dyn DeclQuery,
    text: TextId,
) -> QueryTrisult<SmallVec<[cst::Struct; 1]>> {
    db.query_structs().flat_map(|mut struct_map| {
        struct_map
            .remove(&text)
            .trisult_ok_or(CompilerError::CannotFindStruct(text))
    })
}

pub(super) fn query_ast_struct_def_map(db: &dyn DeclQuery) -> SVMultiMap<DefKey, StructDef, 1> {
    db.query_ast_def_map()
        .into_iter()
        .filter_map(|(def_key, def)| match def_key {
            DefKey::Struct(_) => {
                let defs: SmallVec<[StructDef; 1]> = def
                    .into_iter()
                    .map(|def| {
                        const MESSAGE: &'static str =
                            "Expected struct def because key was a struct key";
                        def.expect_struct(MESSAGE)
                    })
                    .collect();

                Some((def_key, defs))
            }
            _ => None,
        })
        .collect::<SVMultiMapWrapper<DefKey, StructDef, 1>>()
        .into()
}

pub(super) fn query_ast_struct_def(
    db: &dyn DeclQuery,
    struct_id: StructId,
) -> QueryTrisult<SmallVec<[StructDef; 1]>> {
    let vec = db
        .query_ast_struct_def_map()
        .remove(&DefKey::Struct(struct_id))
        .expect(&format!(
            "If a struct declaration id is supplied, the struct definition should also exist"
        ));
    QueryTrisult::Ok(vec)
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
    println!("{}", db.lookup_intern_string(path.segments[0]));
    let mut ast: cst::Ast = tri!(db.query_secondary_file(path), errors);
    println!("Test");

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

pub(super) fn query_ast_def_map(db: &dyn DeclQuery) -> SVMultiMap<DefKey, Def, 1> {
    db.query_type_items()
        .into_iter()
        .filter_map(|root| match root {
            TypeRoot::Event(event) => {
                let event_decl_id = db.query_event_decl(event.decl);
                println!("Event {:?}", event_decl_id);
                Some((DefKey::Event(event_decl_id), Def::Event(event.def)))
            }
            TypeRoot::Enum(r#enum) => {
                let enum_decl_id = db.query_enum_decl(r#enum.decl);
                println!("Enum {:?}", enum_decl_id);
                Some((DefKey::Enum(enum_decl_id), Def::Enum(r#enum.def)))
            }
            TypeRoot::Struct(r#struct) => {
                let ident: StructId = r#struct.decl.name.value;
                let struct_decl_id = db.query_struct_decl(r#struct.decl);
                println!("Struct {:?}", struct_decl_id);
                Some((DefKey::Struct(ident), Def::Struct(r#struct.def)))
            }
        })
        .collect::<SVMultiMapWrapper<DefKey, Def, 1>>()
        .into()
}

pub(super) fn query_ast_event_def_map(db: &dyn DeclQuery) -> HashMap<DefKey, EventDef> {
    db.query_ast_def_map()
        .into_iter()
        .filter_map(|(def_key, mut def)| match def_key {
            DefKey::Event(_) => {
                assert_eq!(
                    def.len(),
                    1,
                    "Events must have exactly 1 definitions, but had {}",
                    def.len()
                );

                const MESSAGE: &str =
                    "Current definition key is an event, but the definition itself was not";
                Some((def_key, def.remove(0).try_into().expect(MESSAGE)))
            }
            DefKey::Struct(_) | DefKey::Enum(_) => None,
        })
        .collect()
}

pub(super) fn query_ast_event_def(
    db: &dyn DeclQuery,
    event_decl_id: EventDeclId,
) -> QueryTrisult<EventDef> {
    let vec = db
        .query_ast_event_def_map()
        .remove(&DefKey::Event(event_decl_id))
        .expect("If an event declaration id is supplied, the event definition should also exist");
    QueryTrisult::Ok(vec)
}
