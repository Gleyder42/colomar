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
use crate::language::imt::{IdentChain};
use crate::language::validator::{Namespace, Validator};
use crate::multimap::Multimap;

type Map<K, V> = HashMap<K, V>;
type QueryCache<K, V> = HashMap<K, V>;

#[derive(Debug)]
pub enum ConverterError {
    CannotResolveIdent(String, Span),
    ResolvedIdentWrongType {
        message: String,
        help: String,
        called_span: Span,
        referenced_span: Span
    }
}

pub fn convert(ast: ast::Ast) -> (imt::Imt, Vec<ConverterError>) {
    let mut enum_cache = QueryCache::new();
    let mut event_cache= QueryCache::new();
    let mut rule_cache = QueryCache::new();
    let mut ident_map: HashMap<String, imt::Root> = HashMap::new();

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
                let event = resolve_event(&mut event_cache, event);
                let event_root = imt::Root::Event(Rc::clone(&event));
                ident_map.insert(event.borrow().name.0.clone(), event_root.clone());
                event_root
            }
            ast::Root::Enum(r#enum) => {
                let r#enum = resolve_enum(&mut enum_cache, r#enum);
                let enum_root = imt::Root::Enum(Rc::clone(&r#enum));
                ident_map.insert(r#enum.borrow().name.0.clone(), enum_root.clone());
                enum_root
            },
            ast::Root::Rule(rule) => {
                let rule = resolve_rule(&mut rule_cache, rule);
                imt::Root::Rule(rule)
            }
        };

        ast_vec.push(im_root);
    };


    for root in &ast_vec {
        match root {
            imt::Root::Rule(rule) => {
                let name = if let imt::Ref::Unbound(name) = &rule.borrow().event {
                    name.clone()
                }  else {
                    continue
                };

                match ident_map.get(&name.0) {
                    Some(root) => {
                        match root {
                            imt::Root::Event(event) => {
                                rule.borrow_mut().event = imt::Ref::Bound(Rc::clone(event))
                            },
                            imt::Root::Enum(r#enum) => {
                                error_vec.push(ConverterError::CannotResolveIdent(
                                    format!("{} is an enum, but an event is required ", r#enum.borrow().name.0),
                                    r#enum.borrow().name.1.clone()
                                ))
                            }
                            imt::Root::Rule(_) => panic!("Rules have no ident")
                        }
                    },
                    None => error_vec.push(ConverterError::CannotResolveIdent(
                        format!("Cannot find event with name {}", name.0),
                        name.1.clone()
                    ))
                }
            }
            imt::Root::Event(event) => {
                for x in &event.borrow_mut().arguments {
                     let y = x.borrow().types.iter()
                        .filter_map(|it| link_type(it, &ident_map, &mut error_vec))
                        .map(|it| imt::Ref::Bound(it))
                        .collect();

                    x.borrow_mut().types = y;
                }
            },
            imt::Root::Enum(r#enum) => {
                // Rules do not need to be linked
            }
        }
    }

    (imt::Imt(ast_vec), error_vec)
}

fn link_type(
    ref_type: &imt::Ref<Ident, imt::Type>,
    ident_map: &HashMap<String, imt::Root>,
    error_vec: &mut Vec<ConverterError>
) -> Option<imt::Type> {
    let name= if let imt::Ref::Unbound(name) = ref_type { name } else { return None };

    match ident_map.get(&name.0) {
        Some(root) => {
            match root {
                imt::Root::Enum(r#enum) => {
                    Some(imt::Type::Enum(Rc::clone(&r#enum)))
                },
                imt::Root::Event(event) => {
                    error_vec.push(ConverterError::ResolvedIdentWrongType {
                        message: format!("rules cannot be used as types"),
                        help: format!("{} is an event", event.borrow().name.0),
                        called_span: name.1.clone(),
                        referenced_span: event.borrow().name.1.clone()
                    });
                    None
                },
                imt::Root::Rule(_) => panic!("Rules have no ident")
            }
        },
        None => {
            error_vec.push(ConverterError::CannotResolveIdent(
                format!("Cannot find type with name {}", name.0),
                name.1.clone()
            ));
            None
        }
    }
}

fn resolve_rule(
    rule_cache: &mut QueryCache<ast::Rule, Rc<RefCell<imt::Rule>>>,
    rule: &ast::Rule
) -> Rc<RefCell<imt::Rule>> {
    if let Some(cached) = rule_cache.get(&rule) {
        return Rc::clone(cached);
    }

    let arguments = rule.args.iter().map(|it| {
        let mut current = it;
        let mut ident_chain = Vec::new();

        loop {
            match current {
                box ast::Call::Var { name, next } => {
                    ident_chain.push(name.clone());

                    match next {
                        Some(next) => current = next,
                        None => break
                    };
                },
                box ast::Call::Fn { .. } => todo!()
            };
        }

        IdentChain(ident_chain)
    }).collect();

    let imt_rule = imt::Rule {
        title: rule.name.0.clone(),
        event: imt::Ref::Unbound(rule.event.clone()),
        arguments: imt::Ref::Unbound(arguments)
    };

    let imt_rule = Rc::new(RefCell::new(imt_rule));
    rule_cache.insert(rule.clone(), Rc::clone(&imt_rule));
    imt_rule
}

fn resolve_event(
    cache: &mut QueryCache<(ast::Event), Rc<RefCell<imt::Event>>>,
    event: &ast::Event
) -> Rc<RefCell<imt::Event>> {
    if let Some(cached) = cache.get(&event) {
        return Rc::clone(cached);
    }

    let arguments: Vec<_> = event.args.iter()
        .map(|decl_args| {
            imt::DeclaredArgument {
                name: decl_args.name.clone(),
                types: decl_args.types.iter().map(|it| imt::Ref::Unbound(it.clone())).collect(),
                default_value: None
            }
        })
        .map(|it| Rc::new(RefCell::new(it)))
        .collect();

    let im_event = imt::Event { name: event.name.clone(), arguments  };
    let im_event = Rc::new(RefCell::new(im_event));
    cache.insert(event.clone(), Rc::clone(&im_event));
    im_event
}

fn resolve_enum(
    cache: &mut QueryCache<ast::Enum, Rc<RefCell<imt::Enum>>>,
    r#enum: &ast::Enum
) -> Rc<RefCell<imt::Enum>> {
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

    let im_enum = Rc::new(RefCell::new(im_enum));
    cache.insert(r#enum.clone(), Rc::clone(&im_enum));
    im_enum
}