use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::language::{ast, Ident, Span};
use crate::language::imt;
use crate::language::imt::RefRule;

type QueryCache<K, V> = HashMap<K, V>;

#[derive(Debug)]
pub enum ConverterError {
    CannotResolveIdent(String, Span),
    ResolvedIdentWrongType {
        message: String,
        help: String,
        called_span: Span,
        referenced_span: Span,
    },
}

type IdentMap = HashMap<String, imt::Root>;

pub fn convert(ast: ast::Ast) -> (imt::Imt, Vec<ConverterError>) {
    let mut enum_cache = QueryCache::new();
    let mut event_cache = QueryCache::new();
    let mut rule_cache = QueryCache::new();
    let mut ident_map = IdentMap::new();

    let mut ast_vec = Vec::new();
    let mut error_vec = Vec::new();

    // Converter Phase
    let mut iter = ast.0.iter();
    loop {
        let root: &ast::Root = match iter.next() {
            Some(root) => root,
            None => break
        };

        let im_root = match root {
            ast::Root::Event(event) => {
                let event = convert_event(&mut event_cache, event);
                let event_root = imt::Root::Event(Rc::clone(&event));
                ident_map.insert(event.borrow().name.value.clone(), event_root.clone());
                event_root
            }
            ast::Root::Enum(r#enum) => {
                let r#enum = convert_enum(&mut enum_cache, r#enum);
                let enum_root = imt::Root::Enum(Rc::clone(&r#enum));
                ident_map.insert(r#enum.borrow().name.value.clone(), enum_root.clone());
                enum_root
            }
            ast::Root::Rule(rule) => {
                let rule = convert_rule(&mut rule_cache, rule);
                imt::Root::Rule(rule)
            }
        };

        ast_vec.push(im_root);
    };

    // Link Phase
    for root in &ast_vec {
        match root {
            imt::Root::Rule(rule) => {
                let name = if let imt::Link::Unbound(name) = &rule.borrow().event {
                    name.clone()
                } else {
                    continue;
                };

                match ident_map.get(&name.value) {
                    Some(root) => {
                        match root {
                            imt::Root::Event(event) => {
                                rule.borrow_mut().event = imt::Link::Bound(Rc::clone(event));

                                let result = link_ident_chain(&rule, rule.borrow().arguments.unbound_or_panic(), &ident_map);
                                let vec = result.unwrap();

                                rule.borrow_mut().arguments = imt::Link::Bound(vec);
                            }
                            imt::Root::Enum(r#enum) => {
                                error_vec.push(ConverterError::CannotResolveIdent(
                                    format!("{} is an enum, but an event is required ", r#enum.borrow().name.value),
                                    r#enum.borrow().name.span.clone(),
                                ))
                            }
                            imt::Root::Rule(_) => panic!("Rules have no ident")
                        }
                    }
                    None => error_vec.push(ConverterError::CannotResolveIdent(
                        format!("Cannot find event with name {}", name.value),
                        name.span.clone(),
                    ))
                }
            }
            imt::Root::Event(event) => {
                for x in &event.borrow_mut().arguments {
                    let y = x.borrow().types.iter()
                        .filter_map(|it| link_type(it, &ident_map, &mut error_vec))
                        .map(|it| imt::Link::Bound(it))
                        .collect();

                    x.borrow_mut().types = y;
                }
            }
            imt::Root::Enum(_enum) => {
                // Rules do not need to be linked
            }
        }
    }

    (imt::Imt(ast_vec), error_vec)
}

fn link_ident_chain(rule: &RefRule, ident_chains: &Vec<imt::IdentChain>, ident_map: &IdentMap) -> Result<Vec<imt::CalledArgument>, ConverterError> {
    let mut vec = Vec::new();

    let mut counter = 0;
    for ident_chain in ident_chains {
        if ident_chain.0.len() == 2 {
            let enum_name = &ident_chain.0[0];
            let constant_name = &ident_chain.0[1];

            if let Some(imt::Root::Enum(r#enum)) = ident_map.get(&enum_name.value) {
                let enum_constant = r#enum.borrow().constants.iter().find(|it| it.name.value == constant_name.value).unwrap().clone();

                let event = rule.borrow().event.bound_or_panic().clone();
                vec.push(imt::CalledArgument {
                    value: imt::ConstValue::EnumConstant(Rc::clone(&enum_constant)),
                    declared: Rc::clone(&event.borrow().arguments[counter])
                });
            } else {
                let error = ConverterError::CannotResolveIdent(format!("Cannot find enum {}", enum_name.value), enum_name.span.clone());
                return Err(error)
            }
        } else {
            let error = ConverterError::CannotResolveIdent(format!("IdentChain can only have two idents"), 0..1);
            return Err(error);
        }

        counter += 1;
    };

    Ok(vec)
}

fn link_type(
    ref_type: &imt::Link<Ident, imt::Type>,
    ident_map: &HashMap<String, imt::Root>,
    error_vec: &mut Vec<ConverterError>,
) -> Option<imt::Type> {
    let name = if let imt::Link::Unbound(name) = ref_type { name } else { return None; };

    match ident_map.get(&name.value) {
        Some(root) => {
            match root {
                imt::Root::Enum(r#enum) => {
                    Some(imt::Type::Enum(Rc::clone(&r#enum)))
                }
                imt::Root::Event(event) => {
                    error_vec.push(ConverterError::ResolvedIdentWrongType {
                        message: format!("rules cannot be used as types"),
                        help: format!("{} is an event", event.borrow().name.value),
                        called_span: name.span.clone(),
                        referenced_span: event.borrow().name.span.clone(),
                    });
                    None
                }
                imt::Root::Rule(_) => panic!("Rules have no ident")
            }
        }
        None => {
            error_vec.push(ConverterError::CannotResolveIdent(
                format!("Cannot find type with name {}", name.value),
                name.span.clone(),
            ));
            None
        }
    }
}

fn convert_rule(
    rule_cache: &mut QueryCache<ast::Rule, Rc<RefCell<imt::Rule>>>,
    rule: &ast::Rule,
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
                }
                box ast::Call::Fn { .. } => todo!()
            };
        }

        imt::IdentChain(ident_chain)
    }).collect();

    let imt_rule = imt::Rule {
        title: rule.name.0.clone(),
        event: imt::Link::Unbound(rule.event.clone()),
        arguments: imt::Link::Unbound(arguments),
    };

    let imt_rule = Rc::new(RefCell::new(imt_rule));
    rule_cache.insert(rule.clone(), Rc::clone(&imt_rule));
    imt_rule
}

fn convert_event(
    cache: &mut QueryCache<ast::Event, Rc<RefCell<imt::Event>>>,
    event: &ast::Event,
) -> Rc<RefCell<imt::Event>> {
    if let Some(cached) = cache.get(&event) {
        return Rc::clone(cached);
    }

    let arguments: Vec<_> = event.args.iter()
        .map(|decl_args| {
            imt::DeclaredArgument {
                name: decl_args.name.clone(),
                types: decl_args.types.iter().map(|it| imt::Link::Unbound(it.clone())).collect(),
                default_value: None,
            }
        })
        .map(|it| Rc::new(RefCell::new(it)))
        .collect();

    let im_event = imt::Event { name: event.name.clone(), arguments };
    let im_event = Rc::new(RefCell::new(im_event));
    cache.insert(event.clone(), Rc::clone(&im_event));
    im_event
}

fn convert_enum(
    cache: &mut QueryCache<ast::Enum, Rc<RefCell<imt::Enum>>>,
    r#enum: &ast::Enum,
) -> Rc<RefCell<imt::Enum>> {
    if let Some(cached) = cache.get(r#enum) {
        return Rc::clone(cached);
    }

    let im_enum = imt::Enum {
        name: r#enum.name.clone(),
        is_workshop: r#enum.is_workshop.clone(),
        constants: r#enum.constants.iter()
            .map(|it| Rc::new(imt::EnumConstant { name: it.clone() }))
            .collect(),
    };

    let im_enum = Rc::new(RefCell::new(im_enum));
    cache.insert(r#enum.clone(), Rc::clone(&im_enum));
    im_enum
}