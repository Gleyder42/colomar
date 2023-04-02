use std::any::Any;
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use chumsky::prelude::todo;
use petgraph::prelude::*;
use crate::language::{ast, Ident, Span};
use crate::language::imt;
use crate::language::validator::{Namespace, Validator};
use crate::multimap::Multimap;

type Map<K, V> = HashMap<K, V>;
type QueryCache<K, V> = HashMap<K, V>;

#[derive(Debug)]
pub enum ConverterError {
    CannotResolveIdent(String, Span)
}

pub fn convert(ast: ast::Ast) -> (imt::Imt, Vec<ConverterError>) {
    let mut enum_cache: QueryCache<ast::Enum, Rc<imt::Enum>> = QueryCache::new();
    let mut event_cache: QueryCache<ast::Event, Rc<imt::Event>> = QueryCache::new();
    let mut ident_map = build_ident_map(&ast);

    let mut ast_vec = Vec::new();
    let mut error_vec = Vec::new();

    let mut iter = ast.0.iter();
    loop {
        let root = match iter.next() {
            Some(root) => root,
            None => break
        };

        let im_root = match root {
            ast::Root::Event(event) => {
                let im_event = resolve_event(
                    &mut enum_cache,
                    &mut event_cache,
                    &mut ident_map,
                    &mut error_vec,
                    event
                );
                imt::Root::Event(im_event)
            }
            ast::Root::Enum(r#enum) => {
                let im_enum = resolve_enum(&mut enum_cache, r#enum);
                imt::Root::Enum(im_enum)
            },
            ast::Root::Rule(rule) => {
                todo!()
            }
        };

        ast_vec.push(im_root);
    };

    (imt::Imt(ast_vec), error_vec)
}

fn build_ident_map(ast: &ast::Ast) -> Map<String, &ast::Root> {
    let mut ident_map: Map<String, &ast::Root> = HashMap::new();

    for root in &ast.0 {
        match root {
            ast::Root::Event(event) => ident_map.insert(event.name.0.clone(), root),
            ast::Root::Rule(rule) => ident_map.insert(rule.event.0.clone(), root),
            ast::Root::Enum(r#enum) => ident_map.insert(r#enum.name.0.clone(), root)
        };
    }
    ident_map
}

fn resolve_rule(
    rule_cache: QueryCache<ast::Rule, Rc<imt::Rule>>,
    rule: ast::Rule
) -> Rc<imt::Rule> {
    if let Some(cached) = rule_cache.get(&rule) {
        return Rc::clone(cached);
    }

    todo!()
}

fn resolve_called_argument(
    type_cache: &mut QueryCache<String, imt::Type>,
    enum_cache: &mut QueryCache<ast::Enum, Rc<imt::Enum>>,
    event_cache: &mut QueryCache<ast::Event, Rc<imt::Event>>,
    ident_map: &mut Map<String, &ast::Root>,
    errors: &mut Vec<ConverterError>,
    called_argument_cache: QueryCache<Box<ast::Call>, Vec<imt::CalledArgument>>,
    call: &Box<ast::Call>
) -> Vec<imt::CalledArgument> {
    if let Some(cached) = called_argument_cache.get(call) {
        return cached.clone()
    }

    todo!()
}

fn resolve_event(
    enum_cache: &mut QueryCache<ast::Enum, Rc<imt::Enum>>,
    event_cache: &mut QueryCache<(ast::Event), Rc<imt::Event>>,
    ident_map: &mut Map<String, &ast::Root>,
    errors: &mut Vec<ConverterError>,
    event: &ast::Event
) -> Rc<imt::Event> {
    if let Some(cached) = event_cache.get(&event) {
        return Rc::clone(cached);
    }

    let arguments: Vec<_> = event.args.iter()
        .map(|decl_args| {
            imt::DeclaredArgument {
                name: decl_args.name.clone(),
                types: decl_args.types.iter().filter_map(|it| resolve_type(enum_cache, errors, ident_map, &it)).collect(),
                default_values: None
            }
        })
        .map(|it| Rc::new(it))
        .collect();

    let im_event = imt::Event { name: event.name.clone(), arguments };
    let im_event = Rc::new(im_event);
    event_cache.insert(event.clone(), Rc::clone(&im_event));
    im_event
}

fn resolve_type(
    enum_cache: &mut QueryCache<ast::Enum, Rc<imt::Enum>>,
    errors: &mut Vec<ConverterError>,
    ident_map: &mut Map<String, &ast::Root>,
    ident: &Ident,
) -> Option<imt::Type> {
    if let Some(root) = ident_map.get(&ident.0) {
        let r#type = match root {
            ast::Root::Enum(r#enum) => imt::Type::Enum(resolve_enum(enum_cache, r#enum)),
            _ => todo!()
        };

        Some(r#type)
    } else {
        errors.push(ConverterError::CannotResolveIdent(format!("Cannot find type {} in global scope", ident.0), ident.1.clone()));
        None
    }
}

fn resolve_enum(
    cache: &mut QueryCache<ast::Enum, Rc<imt::Enum>>,
    r#enum: &ast::Enum
) -> Rc<imt::Enum> {
    if let Some(cached) = cache.get(r#enum) {
        return Rc::clone(cached);
    }

    let im_enum = imt::Enum {
        name: r#enum.name.clone(),
        is_workshop: r#enum.is_workshop.clone(),
        constants: r#enum.constants.iter()
            .map(|it| Rc::new(imt::EnumConstant { name: it.clone() }))
            .collect()
    };

    let im_enum = Rc::new(im_enum);
    cache.insert(r#enum.clone(), Rc::clone(&im_enum));
    im_enum
}