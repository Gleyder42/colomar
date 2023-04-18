use std::cell::{RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use once_cell::unsync::Lazy;
use crate::language::{ast, Ident, Span};
use crate::language::im;
use crate::language::im::{ActualValue, DeclaredArgumentRef, make_ref, Referable, Types};

type QueryCache<K, V> = HashMap<K, V>;

#[derive(Debug)]
pub enum ConverterError {
    CannotResolveIdent(String, Span),
    DuplicateIdent {
        message: String,
        first_defined_span: Span,
        second_defined_span: Span,
    },
    ResolvedIdentWrongType {
        message: String,
        help: String,
        called_span: Span,
        referenced_span: Span,
    },
    MismatchedTypes {
        message: &'static str,
        ident_span: Span,
        ident_message: String,
    }
}

impl ConverterError {

    fn mismatched_types(ident_span: Span, actual_type: im::Type, expected: &im::Types) -> ConverterError {
        ConverterError::MismatchedTypes {
            message: "Mismatched types",
            ident_span,
            ident_message: format!("Is of type {} but {} is expected", actual_type, expected)
        }
    }
}

trait IdentMapExt {
    fn get_event(&self, ident: &Ident) -> Result<im::EventRef, ConverterError>;

    fn insert_unique(&mut self, ident: Ident, value: im::Root) -> Result<(), ConverterError>;
}

struct Namespace {
    parent: Vec<Rc<Namespace>>,
    map: HashMap<String, Referable>,
}

impl Namespace {
    fn new_root() -> Namespace {
        Namespace { map: HashMap::new(), parent: Vec::new() }
    }

    fn try_with_struct(r#struct: &im::StructRef) -> Result<Namespace, ConverterError> {
        let mut namespace = Namespace::new_root();
        namespace.add_struct_properties(r#struct)?;
        namespace.add_struct_functions(r#struct)?;
        Ok(namespace)
    }

    fn try_with_enum(r#enum: &im::EnumRef) -> Result<Namespace, ConverterError> {
        let mut namespace = Namespace::new_root();
        namespace.add_enum_constants(&r#enum)?;
        Ok(namespace)
    }

    fn get_with_str(&self, name: &'static str) -> Option<Referable> {
        self.map.get(name).map(|it| it.clone())
    }

    fn get(&self, ident: &Ident) -> Option<Referable> {
        self.map.get(&ident.value).map(|it| it.clone())
    }

    fn contains(&self, ident: &Ident) -> Option<Referable> {
        let option = self.map.get(&ident.value).map(|it| it.clone());
        if option.is_some() {
            option
        } else {
            self.parent.iter()
                .map(|it| it.contains(&ident))
                .filter(|it| it.is_some())
                .flatten()
                .collect::<Vec<_>>()
                .pop()
        }
    }

    fn add_struct_properties(&mut self, r#struct: &im::StructRef) -> Result<(), ConverterError> {
        for property in &r#struct.borrow().properties {
            self.add(property.borrow().name.clone(), Referable::Property(Rc::clone(&property)))?;
        }
        Ok(())
    }

    fn add_struct_functions(&mut self, r#struct: &im::StructRef) -> Result<(), ConverterError> {
        for function in &r#struct.borrow().functions {
            self.add(function.borrow().name.clone(), Referable::Function(Rc::clone(&function)))?;
        }
        Ok(())
    }

    fn add_enum_constants(&mut self, r#enum: &im::EnumRef) -> Result<(), ConverterError> {
        for enum_constant in &r#enum.borrow().constants {
            self.add(enum_constant.name.clone(), Referable::EnumConstant(Rc::clone(enum_constant)))?;
        };
        return Ok(());
    }

    fn add(&mut self, ident: Ident, referable: Referable) -> Result<(), ConverterError> {
        match self.contains(&ident) {
            Some(root) => {
                let error = ConverterError::DuplicateIdent {
                    message: format!("{} already exists within in the current scope", ident.value),
                    first_defined_span: root.name_span(),
                    second_defined_span: ident.span,
                };
                Err(error)
            }
            None => {
                self.map.insert(ident.value.clone(), referable);
                Ok(())
            }
        }
    }

    fn get_event(&self, ident: &Ident) -> Result<im::EventRef, ConverterError> {
        match self.get(ident) {
            Some(Referable::Event(event)) => Ok(Rc::clone(&event)),
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

struct Predefined {
    string_primitive: im::StructRef,
    num_primitive: im::StructRef,
}

trait ErrorHandler {

    fn add_errors(self, errors: &mut Vec<ConverterError>);
}

impl<T> ErrorHandler for Result<T, ConverterError> {

    fn add_errors(self, errors: &mut Vec<ConverterError>) {
        match self {
            Err(error) => errors.push(error),
            Ok(_) => {}
        }
    }
}

impl<T> ErrorHandler for Result<T, Vec<ConverterError>> {

    fn add_errors(self, errors: &mut Vec<ConverterError>) {
        match self {
            Err(mut error) => errors.append(&mut error),
            Ok(_) => {}
        }
    }
}

pub fn convert(ast: ast::Ast) -> (Option<im::Im>, Vec<ConverterError>) {
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

                root_namespace.add(
                    r#struct.borrow().name.clone(),
                    Referable::Struct(Rc::clone(&r#struct)),
                ).add_errors(&mut error_vec);

                im::Root::Struct(Rc::clone(&r#struct))
            }
            ast::Root::Event(event) => {
                let event = convert_event(&mut event_cache, event);

                root_namespace.add(
                    event.borrow().name.clone(),
                    Referable::Event(Rc::clone(&event)),
                ).add_errors(&mut error_vec);

                im::Root::Event(Rc::clone(&event))
            }
            ast::Root::Enum(r#enum) => {
                let r#enum = convert_enum(&mut enum_cache, r#enum);

                root_namespace.add(
                    r#enum.borrow().name.clone(),
                    Referable::Enum(Rc::clone(&r#enum)),
                ).add_errors(&mut error_vec);

                im::Root::Enum(Rc::clone(&r#enum))
            }
            ast::Root::Rule(rule) => {
                let rule = convert_rule(&mut rule_cache, rule);
                im::Root::Rule(rule)
            }
        };
        im.push(root);
    }

    if !error_vec.is_empty() {
        return (None, error_vec);
    }

    let string_primitive: im::StructRef = root_namespace.get_with_str("string").unwrap().into();
    let num_primitive: im::StructRef = root_namespace.get_with_str("num").unwrap().into();

    let root_namespace = Rc::new(root_namespace);
    let predefined = Predefined { string_primitive, num_primitive };

    for root in &im {
        match root {
            im::Root::Rule(rule) => {
                link_rule(rule, root_namespace.clone(), &predefined).add_errors(&mut error_vec);
            }
            im::Root::Event(event) => {
                link_event(event, root_namespace.clone(), &predefined).add_errors(&mut error_vec);
            },
            _ => {}
        }
    }

    (Some(im), error_vec)
}

fn link_event(event: &im::EventRef, namespace: Rc<Namespace>, predefined: &Predefined) -> Result<(), Vec<ConverterError>> {
    for declared_argument in &event.borrow().arguments {
        let binding = declared_argument.borrow();

        // Try not to move here
        let spanned = binding.types.unbound().clone();
        let split = spanned.types.into_iter()
            .map(|it| resolve_ident(it, Rc::clone(&namespace), predefined))
            .collect::<ResultSplit<ActualValue, ConverterError>>();

        if !split.1.is_empty() {
            return Err(split.1);
        }


        let actual_value = if let Some(default_value) = &binding.default_value {
            //TODO Do not default_value.unbound().clone()
            let actual_value = resolve_call_chain(
                default_value.unbound().clone(),
                namespace.clone(),
                predefined,
            ).map_err(|it| vec![it])?;
            Some(actual_value)
        } else {
            None
        };

        drop(binding);

        if let Some(actual_value) = actual_value {
            declared_argument.borrow_mut().default_value = Some(im::Link::Bound(actual_value));
        }

        let types = split.0.into_iter().map(|it| it.r#type()).collect();
        declared_argument.borrow_mut().types = im::Link::Bound(Types { types, span: spanned.span });
    }
    Ok(())
}

struct ResultSplit<V, E>(Vec<V>, Vec<E>);

impl<V, E> FromIterator<Result<V, E>> for ResultSplit<V, E> {

    fn from_iter<T: IntoIterator<Item=Result<V, E>>>(iter: T) -> Self {
        let mut results = Vec::new();
        let mut errors = Vec::new();
        for result in iter {
            match result {
                Ok(value) => results.push(value),
                Err(error) => errors.push(error)
            }
        }
        ResultSplit(results, errors)
    }
}

impl<V, E> FromIterator<Result<V, Vec<E>>> for ResultSplit<V, E> {

    fn from_iter<T: IntoIterator<Item=Result<V, Vec<E>>>>(iter: T) -> Self {
        let mut results = Vec::new();
        let mut errors = Vec::new();
        for result in iter {
            match result {
                Ok(value) => results.push(value),
                Err(mut error) => errors.append(&mut error)
            }
        }
        ResultSplit(results, errors)
    }
}

fn link_rule(
    rule: &im::RuleRef,
    namespace: Rc<Namespace>,
    predefined: &Predefined
) -> Result<(), Vec<ConverterError>> {
    let event = match namespace.get_event(rule.borrow().event.unbound()) {
        Ok(event) => {
            event
        }
        Err(error) => return Err(vec![error]),
    };
    rule.borrow_mut().event = im::Link::Bound(event.clone());

    let binding = rule.borrow();
    let arguments = binding.arguments.unbound();
    // Todo do not call_chain.clone()
    let split: ResultSplit<ActualValue, ConverterError> = arguments.into_iter()
        .map(|call_chain| resolve_call_chain(call_chain.clone(), namespace.clone(), predefined))
        .collect();

    if !split.1.is_empty() {
        return Err(split.1);
    }

    let split: ResultSplit<im::CalledArgument, ConverterError> = split.0.into_iter()
        .zip(event.borrow().arguments.iter())
        .map(|(actual, declared)| {
            if declared.borrow().types.bound().contains_type(actual.r#type()) {
                let argument = im::CalledArgument {
                    value: actual,
                    declared: declared.clone()
                };
                Ok(argument)
            } else {
                let error = ConverterError::mismatched_types(
                    actual.span(),
                    actual.r#type(),
                    declared.borrow().types.bound()
                );
                Err(vec![error])
            }
        })
        .collect();

    if !split.1.is_empty() {
        return Err(split.1);
    }

    drop(binding);

    rule.borrow_mut().arguments = im::Link::Bound(split.0);
    Ok(())
}

const EMPTY_NAMESPACE: Lazy<Rc<Namespace>> = Lazy::new(|| Rc::new(Namespace::new_root()));

fn resolve_ident(
    ident: Ident,
    namespace: Rc<Namespace>,
    predefined: &Predefined,
) -> Result<ActualValue, ConverterError> {
    resolve_call(Box::new(ast::Call::Ident(ident)), namespace, predefined)
}

fn resolve_call(
    call: Box<ast::Call>,
    namespace: Rc<Namespace>,
    predefined: &Predefined,
) -> Result<ActualValue, ConverterError> {
    resolve_call_chain(vec![call], namespace, predefined)
}

fn resolve_call_chain(
    call_chain: ast::CallChain,
    namespace: Rc<Namespace>,
    predefined: &Predefined,
) -> Result<ActualValue, ConverterError> {
    let mut current_namespace = Rc::clone(&namespace);
    let mut current_value = None;
    for call in call_chain {
        match *call {
            ast::Call::Ident(name) => {
                match current_namespace.get(&name) {
                    Some(Referable::Enum(r#enum)) => {
                        current_namespace = Rc::new(Namespace::try_with_enum(&r#enum)?);
                        current_value = Some(ActualValue::Referable(Referable::Enum(r#enum), name.span));
                    }
                    Some(Referable::EnumConstant(enum_constant)) => {
                        current_namespace = Rc::clone(&EMPTY_NAMESPACE);
                        current_value = Some(ActualValue::Referable(Referable::EnumConstant(enum_constant), name.span));
                    }
                    Some(Referable::Property(property)) => {
                        current_namespace = match property.borrow().r#type.bound() {
                            im::Type::Struct(r#struct) => Rc::new(Namespace::try_with_struct(&r#struct)?),
                            im::Type::Enum(r#enum) => Rc::new(Namespace::try_with_enum(&r#enum)?)
                        };
                        current_value = Some(ActualValue::Referable(Referable::Property(property), name.span));
                    }
                    Some(Referable::Struct(r#struct)) => {
                        current_namespace = Rc::new(Namespace::try_with_struct(&r#struct)?);
                        current_value = Some(ActualValue::Referable(Referable::Struct(r#struct), name.span))
                    }
                    Some(accessor) => {
                        let error = ConverterError::ResolvedIdentWrongType {
                            message: format!("Items of type {} are not supported", accessor.name()),
                            help: String::new(),
                            called_span: name.span.clone(),
                            referenced_span: accessor.name_span(),
                        };
                        return Err(error);
                    }
                    None => {
                        let error = ConverterError::CannotResolveIdent(
                            format!("Cannot find item {}", name.value),
                            name.span.clone(),
                        );
                        return Err(error);
                    }
                };
            }
            ast::Call::IdentArguments { name, args, .. } => {
                match current_namespace.get(&name) {
                    Some(Referable::Function(function)) => {
                        let result = args.into_iter()
                            .map(|it| resolve_call_chain(
                                it,
                                namespace.clone(),
                                predefined
                            ))
                            .collect::<Vec<_>>();
                        let mut args = Vec::new();
                        for result in result {
                            match result {
                                Ok(actual_value) => args.push(actual_value),
                                Err(error) => return Err(error)
                            }
                        }

                        let mut result = function.borrow().arguments.iter()
                            .zip(args.iter())
                            .filter_map(|(declared, actual)| {
                                let declared = declared.borrow();

                                if declared.types.bound().contains_type(actual.r#type()) {
                                    None
                                } else {
                                    let error = ConverterError::mismatched_types(
                                        actual.span(),
                                        actual.r#type(),
                                        declared.types.bound(),
                                    );
                                    Some(error)
                                }
                            })
                            .collect::<Vec<_>>();

                        if let Some(pop) = result.pop() {
                            return Err(pop);
                        }

                        current_namespace = EMPTY_NAMESPACE.clone();
                        current_value = Some(ActualValue::Referable(Referable::Function(function), name.span));
                    }
                    _ => todo!()
                };
            }
            ast::Call::String(string, span) => {
                current_namespace = EMPTY_NAMESPACE.clone();
                current_value = Some(ActualValue::String(string, predefined.string_primitive.clone(), span));
            }
            ast::Call::Number(number, span) => {
                current_namespace = EMPTY_NAMESPACE.clone();
                current_value = Some(ActualValue::Number(number, predefined.num_primitive.clone(), span));
            }
        }
    }

    Ok(current_value.unwrap())
}

fn convert_struct(r#struct: ast::Struct) -> im::StructRef {
    let functions = r#struct.functions.into_iter()
        .map(|it| {
            im::Function {
                name: it.name,
                is_workshop: it.is_workshop,
                arguments: convert_declared_argument(it.arguments),
            }
        })
        .map(make_ref)
        .collect();

    let properties = r#struct.properties.into_iter()
        .map(|it| im::StructProperty {
            is_workshop: it.is_workshop,
            name: it.name,
            r#type: im::Link::Unbound(it.r#type),
            desc: it.desc,
        })
        .map(make_ref)
        .collect();

    let r#struct = im::Struct {
        name: r#struct.name,
        is_open: r#struct.is_open,
        is_workshop: r#struct.is_workshop,
        span: r#struct.span,
        properties,
        functions,
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

    // let arguments = rule.args.into_iter().map(|it| {
    //     let mut current = it;
    //     let mut ident_chain = Vec::new();
    //
    //     loop {
    //         match current {
    //             box ast::Call::Ident { name, next } => {
    //                 ident_chain.push(name.clone());
    //
    //                 match next {
    //                     Some(next) => current = next,
    //                     None => break
    //                 };
    //             }
    //             box ast::Call::Number { .. } => todo!(),
    //             box ast::Call::String { .. } => todo!(),
    //             box ast::Call::ArgumentsIdent { .. } => todo!()
    //         };
    //     }
    //
    //     im::IdentChain(ident_chain)
    // }).collect();

    let im_rule = im::Rule {
        title: rule.name.0,
        event: im::Link::Unbound(rule.event),
        arguments: im::Link::Unbound(Vec::new()),
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

fn convert_declared_argument(vec: Vec<ast::DeclaredArgument>) -> Vec<DeclaredArgumentRef> {
    let arguments: Vec<_> = vec.into_iter()
        .map(|decl_args| {
            im::DeclaredArgument {
                name: decl_args.name,
                types: im::Link::Unbound(decl_args.types),
                default_value: decl_args.default_value.map(|it| im::Link::Unbound(it)),
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

    let im_enum = im::EnumRef::new_cyclic(|weak| {
        let r#enum = im::Enum {
            name: r#enum.name,
            is_workshop: r#enum.is_workshop,
            span: r#enum.span,
            constants: r#enum.constants.into_iter()
                .map(|name| Rc::new(im::EnumConstant { name, r#enum: weak.clone() }))
                .collect(),
        };
        RefCell::new(r#enum)
    });
    cache.insert(cloned_enum, Rc::clone(&im_enum));
    im_enum
}

