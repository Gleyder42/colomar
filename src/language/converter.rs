use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::language::{ast, Ident, Span};
use crate::language::im;
use crate::language::im::EnumRef;

type QueryCache<K, V> = HashMap<K, V>;

#[derive(Debug)]
pub enum ConverterError {
    CannotResolveIdent(String, Span),
    ResolvedIdentWrongType {
        message: String,
        help: String,
        called_span: Span,
        referenced_span: Span,
    }
}

type IdentMap = HashMap<String, im::Root>;

impl IdentMapExt for IdentMap {
    fn get_event(&self, ident: &Ident) -> Result<im::EventRef, ConverterError> {
        match self.get(&ident.value) {
            Some(im::Root::Event(event)) => Ok(Rc::clone(event)),
            Some(value) => {
                let error = ConverterError::ResolvedIdentWrongType {
                    message: format!("Cannot find event {}", ident.value),
                    help: format!("There is a {} {}, but its not an event", value.name(), ident.value),
                    called_span: ident.span.clone(),
                    referenced_span: value.span(),
                };
                Err(error)
            }
            None => {
                let error = ConverterError::CannotResolveIdent(
                    format!("Cannot find {}", ident.value),
                    ident.span.clone(),
                );
                Err(error)
            }
        }
    }

    fn insert_unique(&mut self, ident: Ident, value: im::Root) -> Result<(), ConverterError> {
        if !self.contains_key(&ident.value) {
            self.insert(ident.value.clone(), value);
            Ok(())
        } else {
            let error = ConverterError::CannotResolveIdent(
                format!("{} already exists in the current scope", ident.value),
                ident.span.clone(),
            );
            Err(error)
        }
    }
}

trait IdentMapExt {
    fn get_event(&self, ident: &Ident) -> Result<im::EventRef, ConverterError>;

    fn insert_unique(&mut self, ident: Ident, value: im::Root) -> Result<(), ConverterError>;
}

pub fn convert(ast: ast::Ast) -> (im::Im, Vec<ConverterError>) {
    let mut enum_cache = QueryCache::new();
    let mut event_cache = QueryCache::new();
    let mut rule_cache = QueryCache::new();
    let mut ident_map = IdentMap::new();

    let mut im = im::Im::new();
    let mut error_vec = Vec::new();

    // Converter Phase
    for root in ast {
        let root = match root {
            ast::Root::Event(event) => {
                let event = convert_event(&mut event_cache, event);
                let event_root = im::Root::Event(Rc::clone(&event));
                let result = ident_map.insert_unique(event.borrow().name.clone(), event_root.clone());
                if let Err(error) = result {
                    error_vec.push(error);
                }
                event_root
            }
            ast::Root::Enum(r#enum) => {
                let r#enum = convert_enum(&mut enum_cache, r#enum);
                let enum_root = im::Root::Enum(Rc::clone(&r#enum));
                let result = ident_map.insert_unique(r#enum.borrow().name.clone(), enum_root.clone());
                if let Err(error) = result {
                    error_vec.push(error);
                }
                enum_root
            }
            ast::Root::Rule(rule) => {
                let rule = convert_rule(&mut rule_cache, rule);
                im::Root::Rule(rule)
            }
        };
        im.push(root);
    }

    for root in &im {
        match root {
            im::Root::Rule(rule) => {
                let event = ident_map.get_event(rule.borrow().event.unbound());

                let event = match event {
                    Ok(event) => {
                        rule.borrow_mut().event = im::Link::Bound(Rc::clone(&event));
                        event
                    }
                    Err(error) => {
                        error_vec.push(error);
                        continue;
                    }
                };

                let ident_chain = link_ident_chain(
                    rule.borrow().arguments.unbound(),
                    &ident_map,
                    |r#enum, enum_constant, index| create_called_argument(r#enum, enum_constant, index, &event),
                );

                match ident_chain {
                    Ok(arguments) => rule.borrow_mut().arguments = im::Link::Bound(arguments),
                    Err(mut error) => error_vec.append(&mut error)
                };
            }
            im::Root::Event(event) => {
                for arguments in &event.borrow().arguments {
                    let types = arguments.borrow().types.iter()
                        .filter_map(|it| link_type(it, &ident_map, &mut error_vec))
                        .map(im::Link::Bound)
                        .collect();

                    arguments.borrow_mut().types = types;
                }
            }
            _ => {}
        }
    }

    (im, error_vec)
}

fn create_called_argument(
    r#enum: &EnumRef,
    enum_constant: &Rc<im::EnumConstant>,
    index: usize,
    event: &im::EventRef
) -> Result<im::CalledArgument, ConverterError> {
    let binding = event.borrow();
    let declared_argument = binding.arguments.get(index);
    println!("{:?}", declared_argument);

    match declared_argument {
        Some(declared_argument) if declared_argument.borrow().contains_type(r#enum) => { },
        Some(declared_argument) => {
            let error = ConverterError::ResolvedIdentWrongType {
                message: format!("Argument types don't match"),
                help: format!("Consider changing the type of the argument"),
                called_span: declared_argument.borrow().name.span.clone(),
                referenced_span: event.borrow().name.span.clone()
            };
            return Err(error);
        }
        None => {
            let error = ConverterError::CannotResolveIdent(
                format!("Too many arguments supplied"),
                enum_constant.name.span.clone()
            );
            return Err(error);
        }
    }

    let argument = im::CalledArgument {
        value: im::ConstValue::EnumConstant(Rc::clone(enum_constant)),
        declared: Rc::clone(&binding.arguments[index]),
    };
    Ok(argument)
}

fn create_const_value(
    enum_constant: &Rc<im::EnumConstant>,
    _index: usize
) -> Result<im::ConstValue, ConverterError>  {
    Ok(im::ConstValue::EnumConstant(Rc::clone(&enum_constant)))
}

fn link_ident_chain<T, F>(
    ident_chains: &Vec<im::IdentChain>,
    ident_map: &IdentMap,
    function: F,
) -> Result<Vec<T>, Vec<ConverterError>>
    where F: Fn(&EnumRef, &Rc<im::EnumConstant>, usize) -> Result<T, ConverterError>
{
    let mut results = Vec::new();
    let mut errors = Vec::new();

    let mut index: usize = 0;
    for ident_chain in ident_chains {
        if ident_chain.0.len() == 2 {
            let enum_name = &ident_chain.0[0];
            let constant_name = &ident_chain.0[1];

            if let Some(im::Root::Enum(r#enum)) = ident_map.get(&enum_name.value) {
                let enum_ref = r#enum.borrow();
                let enum_constant = enum_ref.constants.iter()
                    .find(|it| it.name.value == constant_name.value);

                if let Some(enum_constant) = enum_constant {
                    let result = function(r#enum, enum_constant, index);
                    match result {
                        Ok(value) => results.push(value),
                        Err(error) => errors.push(error)
                    };
                };
            } else {
                let error = ConverterError::CannotResolveIdent(format!("Cannot find enum {}", enum_name.value), enum_name.span.clone());
                errors.push(error);
                return Err(errors);
            }
        } else {
            let error = ConverterError::CannotResolveIdent(format!("IdentChain can only have two idents"), 0..1);
            errors.push(error);
            return Err(errors);
        }

        index += 1;
    };

    if errors.is_empty() {
        Ok(results)
    } else {
        Err(errors)
    }
}

fn link_type(
    ref_type: &im::Link<Ident, im::Type>,
    ident_map: &HashMap<String, im::Root>,
    error_vec: &mut Vec<ConverterError>,
) -> Option<im::Type> {
    let name = if let im::Link::Unbound(name) = ref_type { name } else { return None; };

    match ident_map.get(&name.value) {
        Some(root) => {
            match root {
                im::Root::Enum(r#enum) => {
                    Some(im::Type::Enum(Rc::clone(&r#enum)))
                }
                im::Root::Event(event) => {
                    error_vec.push(ConverterError::ResolvedIdentWrongType {
                        message: format!("rules cannot be used as types"),
                        help: format!("{} is an event", event.borrow().name.value),
                        called_span: name.span.clone(),
                        referenced_span: event.borrow().name.span.clone(),
                    });
                    None
                }
                im::Root::Rule(_) => panic!("Rules have no ident")
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
    rule_cache: &mut QueryCache<ast::Rule, im::RuleRef>,
    rule: ast::Rule,
) -> im::RuleRef {
    if let Some(cached) = rule_cache.get(&rule) {
        return Rc::clone(cached);
    }
    let cloned_rule = rule.clone();

    let arguments = rule.args.into_iter().map(|it| {
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

        im::IdentChain(ident_chain)
    }).collect();

    let im_rule = im::Rule {
        title: rule.name.0,
        event: im::Link::Unbound(rule.event),
        arguments: im::Link::Unbound(arguments),
        span: rule.span,
    };

    let im_rule = Rc::new(RefCell::new(im_rule));
    rule_cache.insert(cloned_rule, Rc::clone(&im_rule));
    im_rule
}

fn convert_event(
    cache: &mut QueryCache<ast::Event, im::EventRef>,
    event: ast::Event,
) -> im::EventRef {
    if let Some(cached) = cache.get(&event) {
        return Rc::clone(cached);
    }
    let cloned_event = event.clone();

    let arguments: Vec<_> = event.args.into_iter()
        .map(|decl_args| {
            im::DeclaredArgument {
                name: decl_args.name,
                types: decl_args.types.into_iter().map(im::Link::Unbound).collect(),
                default_value: None,
            }
        })
        .map(|it| Rc::new(RefCell::new(it)))
        .collect();

    let im_event = im::Event { name: event.name, arguments, span: event.span };
    let im_event = Rc::new(RefCell::new(im_event));
    cache.insert(cloned_event, Rc::clone(&im_event));
    im_event
}

fn convert_enum(
    cache: &mut QueryCache<ast::Enum, im::EnumRef>,
    r#enum: ast::Enum,
) -> im::EnumRef {
    if let Some(cached) = cache.get(&r#enum) {
        return Rc::clone(cached);
    }
    let cloned_enum = r#enum.clone();

    let im_enum = im::Enum {
        name: r#enum.name,
        is_workshop: r#enum.is_workshop,
        span: r#enum.span,
        constants: r#enum.constants.into_iter()
            .map(|name| Rc::new(im::EnumConstant { name }))
            .collect(),
    };

    let im_enum = Rc::new(RefCell::new(im_enum));
    cache.insert(cloned_enum, Rc::clone(&im_enum));
    im_enum
}