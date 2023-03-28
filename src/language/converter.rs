use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use crate::language::{ast, Ident, Span};
use crate::language::im;
use crate::language::validator::{Namespace, Validator};
use crate::multimap::Multimap;

type Map<K, V> = HashMap<K, V>;
type QueryCache<K, V> = HashMap<K, V>;

#[derive(Debug)]
pub enum ConverterError {
    CannotResolveIdent(String, Span)
}

pub fn convert(ast: ast::Ast) -> (im::Im, Vec<ConverterError>) {
    let mut type_cache: QueryCache<String, im::Type> = QueryCache::new();
    let mut enum_cache: QueryCache<ast::Enum, Rc<im::Enum>> = QueryCache::new();
    let mut event_cache: QueryCache<ast::Event, Rc<im::Event>> = QueryCache::new();
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
                    &mut type_cache,
                    &mut enum_cache,
                    &mut event_cache,
                    &mut ident_map,
                    &mut error_vec,
                    event
                );
                im::Root::Event(im_event)
            }
            ast::Root::Enum(r#enum) => {
                let im_enum = resolve_enum(&mut enum_cache, r#enum);
                im::Root::Enum(im_enum)
            },
            _ => todo!()
        };

        ast_vec.push(im_root);
    };

    (im::Im(ast_vec), error_vec)
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

fn convert_rule(rule: ast::Rule) -> im::DeclaredRule {
    todo!()
}

fn resolve_event(
    type_cache: &mut QueryCache<String, im::Type>,
    enum_cache: &mut QueryCache<ast::Enum, Rc<im::Enum>>,
    event_cache: &mut QueryCache<ast::Event, Rc<im::Event>>,
    ident_map: &mut Map<String, &ast::Root>,
    errors: &mut Vec<ConverterError>,
    event: &ast::Event
) -> Rc<im::Event> {
    if let Some(cached) = event_cache.get(&event) {
        return Rc::clone(cached);
    }

    let arguments: Vec<_> = event.args.iter()
        .map(|decl_args| {
            im::DeclaredArgument {
                name: decl_args.name.clone(),
                types: decl_args.types.iter().filter_map(|it| resolve_type(type_cache, enum_cache, ident_map, errors, &it)).collect(),
                default_values: None
            }
        })
        .map(|it| Rc::new(it))
        .collect();

    let im_event = im::Event { name: event.name.clone(), arguments };
    let im_event = Rc::new(im_event);
    event_cache.insert(event.clone(), Rc::clone(&im_event));
    im_event
}

fn resolve_type(
    type_cache: &mut QueryCache<String, im::Type>,
    enum_cache: &mut QueryCache<ast::Enum, Rc<im::Enum>>,
    ident_map: &mut Map<String, &ast::Root>,
    errors: &mut Vec<ConverterError>,
    ident: &Ident,
) -> Option<im::Type> {
    if let Some(cached) = type_cache.get(&ident.0) {
        return Some(cached.clone());
    }

    if let Some(root) = ident_map.get(&ident.0) {
        let r#type = match root {
            ast::Root::Enum(r#enum) => im::Type::Enum(resolve_enum(enum_cache, r#enum)),
            _ => todo!()
        };

        type_cache.insert(ident.0.clone(), r#type.clone());
        Some(r#type)
    } else {
        errors.push(ConverterError::CannotResolveIdent(format!("Cannot find type {} in global scope", ident.0), ident.1.clone()));
        None
    }
}

fn resolve_enum(
    cache: &mut QueryCache<ast::Enum, Rc<im::Enum>>,
    r#enum: &ast::Enum
) -> Rc<im::Enum> {
    if let Some(cached) = cache.get(r#enum) {
        println!("Cache Hit! {:?}", cached);
        return Rc::clone(cached);
    }

    let im_enum = im::Enum {
        is_workshop: r#enum.is_workshop.clone(),
        constants: r#enum.constants.iter()
            .map(|it| Rc::new(im::EnumConstant { name: it.0.clone() }))
            .collect()
    };

    let im_enum = Rc::new(im_enum);
    cache.insert(r#enum.clone(), Rc::clone(&im_enum));
    im_enum
}