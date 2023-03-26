use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use chumsky::prelude::todo;
use crate::language::ast;
use crate::language::ast::Spanned;
use crate::language::im;
use crate::language::query::{QueryCache};
use crate::language::validator::{Namespace, Validator};
use crate::multimap::Multimap;

type Map<K, V> = HashMap<K, V>;

pub fn convert(ast: ast::Ast) -> im::Im {
    let mut iter = ast.0.into_iter();

    loop {
        let root = match iter.next() {
            Some(root) => root,
            None => break
        };
    };

    todo!()
}

fn convert_rule(rule: ast::Rule) -> im::DeclaredRule {
    todo!()
}

fn convert_event(
    type_cache: &mut QueryCache<ast::Ident, im::Type>,
    enum_cache: &mut QueryCache<ast::Enum, im::Enum>,
    event_cache: &mut QueryCache<ast::Event, im::Event>,
    ident_map: &mut Multimap<String, ast::Root>,
    event: ast::Event
) -> im::Event {
    if let Some(cached) = event_cache.contains(&event) {

    }


    let args: Vec<_> = event.args.iter()
        .map(|decl_args| {
            im::DeclaredArgument {
                name: decl_args.name.0.clone(),
                types: decl_args.types.iter().map(|it| resolve_type(type_cache, it.clone(), ident_map, TypeHint::Enum)).collect(),
                default_values: None,
            }
        })
        .map(|it| Rc::new(it))
        .collect();

    let im_event = im::Event {
        name: Spanned(event.event.0, event.event.1),
        arguments: args
    };

    todo!()
}

#[derive(Copy, Clone)]
enum TypeHint {
    Enum,
    Event
}

fn resolve_type(
    cache: &mut QueryCache<ast::Ident, im::Type>,
    ident: ast::Ident,
    ident_map: &mut Multimap<String, ast::Root>,
    type_hint: TypeHint
) -> im::Type {
    if let Some(cached) = cache.contains(&ident) {
        return cached;
    }

    let _ = if let Some(resolved_idents) = ident_map.get(&ident.0) {
        let filtered: Vec<_> = resolved_idents.iter()
            .filter_map(|it| {
                match (it, type_hint) {
                    (ast::Root::Enum(r#enum), TypeHint::Enum) => Some(r#enum.name.0.clone()),
                    (ast::Root::Event(event), TypeHint::Event) => Some(event.event.0.clone()),
                    _ => None
                }
            })
            .collect();

        if filtered.len() == 1 {
            cache.add(ident, im::Type(filtered.first().unwrap().clone()))
        } else {
            panic!("Ambiguous naming: {:?}", resolved_idents)
        }
    } else {
        panic!("Cannot find elements by the name {}", ident.0)
    };

    todo!()
}

fn convert_enum(
    cache: &mut QueryCache<ast::Enum, im::Enum>,
    r#enum: ast::Enum
) -> im::Enum {
    if let Some(cached) = cache.contains(&r#enum) {
        return cached;
    }

    let im_enum = im::Enum {
        is_workshop: r#enum.is_workshop.clone(),
        constants: r#enum.constants.iter()
            .map(|it| Rc::new(im::EnumConstant { name: it.0.clone() }))
            .collect()
    };
    cache.add(r#enum, im_enum)
}