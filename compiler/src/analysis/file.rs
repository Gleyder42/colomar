use super::super::analysis::decl::DeclQuery;
use super::super::cir::{EnumDeclId, EventDeclId};
use super::super::cst::{Def, EventDef, Root, StructDef, TypeRoot, Visibility};
use super::super::error::CompilerError;

use super::super::trisult::{Errors, IntoTrisult};
use super::super::{cst, QueryTrisult, SVMultiMap, SVMultiMapWrapper, StructId, TextId};
use crate::error::PartialCompilerError;
use crate::{tri, PartialQueryTrisult};
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
    let mut errors = Errors::new();

    let main_file_cst = tri!(db.query_main_file(), errors);

    let struct_decls: Vec<_> = main_file_cst
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

    errors.value(struct_map)
}

pub(super) fn query_struct_by_name(
    db: &dyn DeclQuery,
    text: TextId,
) -> PartialQueryTrisult<SmallVec<[cst::Struct; 1]>> {
    db.query_structs()
        .partial_errors()
        .flat_map(|mut struct_map| {
            struct_map
                .remove(&text)
                .trisult_ok_or(PartialCompilerError::CannotFindStruct(text).into())
        })
}

pub(super) fn query_ast_struct_def_map(
    db: &dyn DeclQuery,
) -> QueryTrisult<SVMultiMap<DefKey, StructDef, 1>> {
    let mut errors = Errors::new();

    let ast_def_map = tri!(db.query_ast_def_map(), errors);

    let wrapper = ast_def_map
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
        .collect::<SVMultiMapWrapper<DefKey, StructDef, 1>>();

    errors.value(wrapper.into())
}

pub(super) fn query_ast_struct_def(
    db: &dyn DeclQuery,
    struct_id: StructId,
) -> QueryTrisult<SmallVec<[StructDef; 1]>> {
    let mut errors = Errors::new();

    let mut map = tri!(db.query_ast_struct_def_map(), errors);

    let vec = map
        .remove(&DefKey::Struct(struct_id))
        .expect("If a struct declaration id is supplied, the struct definition should also exist");

    errors.value(vec)
}

pub(super) fn query_main_file(db: &dyn DeclQuery) -> QueryTrisult<cst::Cst> {
    db.query_file(db.main_file_name().with_fake_span(db), false)
}

pub(super) fn query_action_items(db: &dyn DeclQuery) -> QueryTrisult<Vec<Root>> {
    db.query_main_file().map(|it| {
        it.into_iter()
            .filter(|root| match root {
                Root::Rule(_) | Root::Struct(_) | Root::Event(_) | Root::Enum(_) => true,
                Root::Import(_) => false,
            })
            .collect()
    })
}

pub(super) fn query_file(
    db: &dyn DeclQuery,
    path: cst::Path,
    include_only_public: bool,
) -> QueryTrisult<cst::Cst> {
    let mut errors = Errors::default();
    let mut ast: cst::Cst = tri!(db.query_secondary_file(path), errors);

    let import_indices: Vec<_> = ast
        .0
        .iter()
        .enumerate()
        .filter_map(|(index, element)| match element {
            Root::Import(_) => Some(index),
            _ => None,
        })
        .collect();

    let imports: QueryTrisult<Vec<cst::Cst>> = import_indices
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

pub(super) fn query_secondary_file(db: &dyn DeclQuery, path: cst::Path) -> QueryTrisult<cst::Cst> {
    db.secondary_files()
        .remove(&path.name)
        .trisult_ok_or(CompilerError::CannotFindFile(path))
}

pub(super) fn query_type_items(db: &dyn DeclQuery) -> QueryTrisult<Vec<TypeRoot>> {
    db.query_main_file().map(|ast| {
        ast.into_iter()
            .filter_map(|root| match root {
                Root::Event(event) => Some(TypeRoot::Event(event)),
                Root::Enum(r#enum) => Some(TypeRoot::Enum(r#enum)),
                Root::Struct(r#struct) => Some(TypeRoot::Struct(r#struct)),
                _ => None,
            })
            .collect()
    })
}

pub(super) fn query_ast_def_map(db: &dyn DeclQuery) -> QueryTrisult<SVMultiMap<DefKey, Def, 1>> {
    let mut errors = Errors::new();

    let type_items = tri!(db.query_type_items(), errors);

    let wrapper = type_items
        .into_iter()
        .filter_map(|root| match root {
            TypeRoot::Event(event) => {
                let event_decl_id = db.query_event_decl(event.decl);
                Some((DefKey::Event(event_decl_id), Def::Event(event.def)))
            }
            TypeRoot::Enum(r#enum) => {
                let enum_decl_id = db.query_enum_decl(r#enum.decl);
                Some((DefKey::Enum(enum_decl_id), Def::Enum(r#enum.def)))
            }
            TypeRoot::Struct(r#struct) => {
                let ident: StructId = r#struct.decl.name.value;
                Some((DefKey::Struct(ident), Def::Struct(r#struct.def)))
            }
        })
        .collect::<SVMultiMapWrapper<DefKey, Def, 1>>();

    errors.value(wrapper.into())
}

pub(super) fn query_ast_event_def_map(
    db: &dyn DeclQuery,
) -> QueryTrisult<HashMap<DefKey, EventDef>> {
    let mut errors = Errors::new();

    let ast_def_map = tri!(db.query_ast_def_map(), errors);

    let map: HashMap<_, _> = ast_def_map
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
        .collect();

    errors.value(map)
}

pub(super) fn query_ast_event_def(
    db: &dyn DeclQuery,
    event_decl_id: EventDeclId,
) -> QueryTrisult<EventDef> {
    let mut errors = Errors::new();

    let mut ast_event_def_map = tri!(db.query_ast_event_def_map(), errors);

    let vec = ast_event_def_map
        .remove(&DefKey::Event(event_decl_id))
        .expect("If an event declaration id is supplied, the event definition should also exist");

    errors.value(vec)
}
