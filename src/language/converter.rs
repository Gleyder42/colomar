use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::language::{ast, Ident, Span};
use crate::language::im;
use crate::language::im::{make_ref};

type QueryCache<K, V> = HashMap<K, V>;

#[derive(Debug)]
pub enum ConverterError {
    CannotResolveIdent(String, Span),
    DuplicateIdent {
        message: String,
        first_defined_span: Span,
        second_defined_span: Span
    },
    ResolvedIdentWrongType {
        message: String,
        help: String,
        called_span: Span,
        referenced_span: Span,
    }
}

trait IdentMapExt {
    fn get_event(&self, ident: &Ident) -> Result<im::EventRef, ConverterError>;

    fn insert_unique(&mut self, ident: Ident, value: im::Root) -> Result<(), ConverterError>;
}

#[derive(Clone)]
enum Accessor {
    Enum(im::EnumRef),
    Event(im::EventRef),
    Struct(im::StructRef),
    EnumConstant(Rc<im::EnumConstant>),
}

impl Accessor {

    fn name_span(&self) -> Span {
        match self {
            Accessor::Enum(r#enum) => r#enum.borrow().name.span.clone(),
            Accessor::Struct(r#struct) => r#struct.borrow().name.span.clone(),
            Accessor::Event(event) => event.borrow().name.span.clone(),
            Accessor::EnumConstant(enum_constant) => enum_constant.name.span.clone()
        }
    }

    fn name(&self) -> &'static str {
        match self {
            Accessor::Event(_) => "Event",
            Accessor::Enum(_) => "Enum",
            Accessor::EnumConstant(_) => "Enum Constant",
            Accessor::Struct(_) => "Struct"
        }
    }
}

struct Namespace<'a> {
    parent: Option<&'a Namespace<'a>>,
    map: HashMap<String, Accessor>
}

impl<'a> Namespace<'a> {

    fn new_root() -> Namespace<'a> {
        Namespace { map: HashMap::new(), parent: None }
    }

    fn new(parent: &'a Namespace) -> Namespace<'a> {
        Namespace { map: HashMap::new(), parent: Some(parent) }
    }

    fn get(&self, ident: &Ident) -> Option<Accessor> {
        self.map.get(&ident.value).map(|it| it.clone())
    }

    fn contains(&self, ident: &Ident) -> Option<Accessor> {
        let option = self.map.get(&ident.value).map(|it| it.clone());
        if option.is_some() {
            option
        } else {
            self.parent.map(|it| it.contains(ident)).flatten()
        }
    }

    fn add_enum_constants(&mut self, r#enum: &im::EnumRef) -> Result<(), ConverterError> {
        for enum_constant in &r#enum.borrow().constants {
            let result = self.add(
                enum_constant.name.clone(),
                Accessor::EnumConstant(Rc::clone(enum_constant))
            );
            if result.is_err() {
                return result;
            }
        };
        return Ok(());
    }

    fn add(&mut self, ident: Ident, accessor: Accessor) -> Result<(), ConverterError> {
        match self.contains(&ident) {
            Some(root) => {
                let error = ConverterError::DuplicateIdent {
                    message: format!("{} already exists within in the current scope", ident.value),
                    first_defined_span: root.name_span(),
                    second_defined_span: ident.span
                };
                Err(error)
            },
            None => {
                self.map.insert(ident.value.clone(), accessor);
                Ok(())
            }
        }
    }

    fn get_event(&self, ident: &Ident) -> Result<im::EventRef, ConverterError> {
        match self.get(ident) {
            Some(Accessor::Event(event)) => Ok(Rc::clone(&event)),
            Some(value) => {
                let error = ConverterError::ResolvedIdentWrongType {
                    message: format!("Cannot find event {}", ident.value),
                    help: format!("There is a {} {}, but its not an event", value.name(), ident.value),
                    called_span: ident.span.clone(),
                    referenced_span: value.name_span(),
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
}

pub fn convert(ast: ast::Ast) -> (im::Im, Vec<ConverterError>) {
    let mut root_namespace = Namespace::new_root();
    let mut enum_cache = QueryCache::new();
    let mut event_cache = QueryCache::new();
    let mut rule_cache = QueryCache::new();

    let mut im = im::Im::new();
    let mut error_vec = Vec::new();

    // Converter Phase
    for root in ast {
        let root = match root {
            ast::Root::Struct(r#struct) => {
                let r#struct = convert_struct(r#struct);
                let result = root_namespace.add(
                    r#struct.borrow().name.clone(),
                    Accessor::Struct(Rc::clone(&r#struct))
                );
                if let Err(error) = result {
                    error_vec.push(error);
                }
                im::Root::Struct(Rc::clone(&r#struct))
            }
            ast::Root::Event(event) => {
                let event = convert_event(&mut event_cache, event);
                let result = root_namespace.add(
                    event.borrow().name.clone(),
                    Accessor::Event(Rc::clone(&event))
                );
                if let Err(error) = result {
                    error_vec.push(error);
                }
                im::Root::Event(Rc::clone(&event))
            }
            ast::Root::Enum(r#enum) => {
                let r#enum = convert_enum(&mut enum_cache, r#enum);
                let result = root_namespace.add(
                    r#enum.borrow().name.clone(),
                    Accessor::Enum(Rc::clone(&r#enum))
                );
                if let Err(error) = result {
                    error_vec.push(error);
                }
                im::Root::Enum(Rc::clone(&r#enum))
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
                link_rule(&root_namespace, &mut error_vec, rule);
            }
            im::Root::Event(event) => {
                link_enum(&root_namespace, &mut error_vec, &event);
            }
            _ => {}
        }
    }

    (im, error_vec)
}

fn link_enum(namespace: &Namespace, mut error_vec: &mut Vec<ConverterError>, event: &im::EventRef) {
    for arguments in &event.borrow().arguments {
        let types = arguments.borrow().types.iter()
            .filter_map(|it| link_type(it, &namespace, &mut error_vec))
            .map(im::Link::Bound)
            .collect();

        arguments.borrow_mut().types = types;
    }
}

fn link_rule(namespace: &Namespace, error_vec: &mut Vec<ConverterError>, rule: &im::RuleRef) {
    let event = namespace.get_event(rule.borrow().event.unbound());

    let event = match event {
        Ok(event) => {
            rule.borrow_mut().event = im::Link::Bound(Rc::clone(&event));
            event
        }
        Err(error) => {
            error_vec.push(error);
            return;
        }
    };

    let ident_chain = link_ident_chain(
        rule.borrow().arguments.unbound(),
        &namespace,
        |r#enum, enum_constant, index| create_called_argument(r#enum, enum_constant, index, &event),
    );

    match ident_chain {
        Ok(arguments) => rule.borrow_mut().arguments = im::Link::Bound(arguments),
        Err(mut error) => error_vec.append(&mut error)
    };
}

fn create_called_argument(
    r#enum: &im::EnumRef,
    enum_constant: &Rc<im::EnumConstant>,
    index: usize,
    event: &im::EventRef
) -> Result<im::CalledArgument, ConverterError> {
    let binding = event.borrow();
    let declared_argument = binding.arguments.get(index);
    println!("{:?}", declared_argument);

    match declared_argument {
        Some(declared_argument) if declared_argument.borrow().contains_type(im::Type::Enum(Rc::clone(r#enum))) => { },
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

fn _create_const_value(
    enum_constant: &Rc<im::EnumConstant>,
    _index: usize
) -> Result<im::ConstValue, ConverterError>  {
    Ok(im::ConstValue::EnumConstant(Rc::clone(&enum_constant)))
}

fn link_call(call: Box<ast::Call>, namespace: &Namespace)  {
    let mut current = call;
    let mut current_namespace = Namespace::new_root();
    loop {
        let next = match *current {
            ast::Call::Var { name, next } => {
                match namespace.get(&name) {
                    Some(Accessor::Enum(r#enum)) => current_namespace.add_enum_constants(&r#enum).unwrap(),
                    Some(Accessor::EnumConstant(enum_constant)) => todo!(),
                    _ => todo!()
                };

                next
            },
            ast::Call::Fn { name, args, span, next } => {
                next
            },
            ast::Call::Number { value, next } => {
                next
            },
            ast::Call::String { value, next } => {
                next
            }
        };

        match next {
            Some(next) => current = next,
            None => break,
        }
    }
}

fn link_ident_chain<T, F>(
    ident_chains: &Vec<im::IdentChain>,
    namespace: &Namespace,
    function: F,
) -> Result<Vec<T>, Vec<ConverterError>>
    where F: Fn(&im::EnumRef, &Rc<im::EnumConstant>, usize) -> Result<T, ConverterError>
{
    let mut results = Vec::new();
    let mut errors = Vec::new();

    let mut index: usize = 0;
    for ident_chain in ident_chains {
        if ident_chain.0.len() == 2 {
            let enum_name = &ident_chain.0[0];
            let constant_name = &ident_chain.0[1];

            if let Some(Accessor::Enum(r#enum)) = namespace.get(&enum_name) {
                let enum_ref = r#enum.borrow();
                let enum_constant = enum_ref.constants.iter()
                    .find(|it| it.name.value == constant_name.value);

                if let Some(enum_constant) = enum_constant {
                    let result = function(&r#enum, enum_constant, index);
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
    namespace: &Namespace,
    error_vec: &mut Vec<ConverterError>,
) -> Option<im::Type> {
    let name = if let im::Link::Unbound(name) = ref_type { name } else { return None; };

    match namespace.get(&name) {
        Some(root) => {
            match root {
                Accessor::Enum(r#enum) => {
                    Some(im::Type::Enum(Rc::clone(&r#enum)))
                },
                Accessor::Struct(r#struct) => {
                    Some(im::Type::Struct(Rc::clone(&r#struct)))
                }
                Accessor::Event(event) => {
                    error_vec.push(ConverterError::ResolvedIdentWrongType {
                        message: format!("rules cannot be used as types"),
                        help: format!("{} is an event", event.borrow().name.value),
                        called_span: name.span.clone(),
                        referenced_span: event.borrow().name.span.clone(),
                    });
                    None
                },
                _ => todo!()
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

fn convert_struct(r#struct: ast::Struct) -> im::StructRef {
    let functions = r#struct.functions.into_iter()
        .map(|it| {
            im::Function {
                name: it.name,
                is_workshop: it.is_workshop,
                arguments: convert_declared_argument(it.arguments)
            }
        })
        .map(make_ref)
        .collect();

    let properties = r#struct.properties.into_iter()
        .map(|it| im::StructProperty {
            is_workshop: it.is_workshop,
            name: it.name,
            r#type: im::Link::Unbound(it.r#type),
            desc: it.desc
        })
        .map(make_ref)
        .collect();

    let r#struct = im::Struct {
        name: r#struct.name,
        is_open: r#struct.is_open,
        is_workshop: r#struct.is_workshop,
        span: r#struct.span,
        properties, functions
    };
    make_ref(r#struct)
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
                box ast::Call::Number { .. } => todo!(),
                box ast::Call::String { .. } => todo!(),
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

    let arguments = convert_declared_argument(event.args);

    let im_event = im::Event { name: event.name, arguments, span: event.span };
    let im_event = Rc::new(RefCell::new(im_event));
    cache.insert(cloned_event, Rc::clone(&im_event));
    im_event
}

fn convert_declared_argument(vec: Vec<ast::DeclaredArgument>) -> Vec<Rc<RefCell<im::DeclaredArgument>>> {
    let arguments: Vec<_> = vec.into_iter()
        .map(|decl_args| {
            im::DeclaredArgument {
                name: decl_args.name,
                types: decl_args.types.into_iter().map(im::Link::Unbound).collect(),
                default_value: None,
            }
        })
        .map(|it| Rc::new(RefCell::new(it)))
        .collect();
    arguments
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

