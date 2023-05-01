use std::rc::Rc;
use chumsky::text::ident;
use once_cell::unsync::Lazy;
use crate::language::converter::error::ConverterError;
use crate::language::converter::namespace::Namespace;
use crate::language::{ast, Ident, im};
use crate::language::converter::Predefined;
use crate::language::im::{AValue, CValue, RValue};

const EMPTY_NAMESPACE: Lazy<Rc<Namespace>> = Lazy::new(|| Rc::new(Namespace::new_root()));

fn resolve_ident(
    ident: Ident,
    namespace: Rc<Namespace>,
    predefined: &Predefined,
) -> Result<AValue, ConverterError> {
    resolve_call(ident.into(), namespace, predefined)
}

fn resolve_call(
    call: Box<ast::Call>,
    namespace: Rc<Namespace>,
    predefined: &Predefined,
) -> Result<AValue, ConverterError> {
    resolve_call_chain(call.into(), namespace, predefined)
}

pub(in super) fn resolve_call_chain(
    call_chain: ast::CallChain,
    namespace: Rc<Namespace>,
    predefined: &Predefined,
) -> Result<AValue, ConverterError> {
    let mut current_namespace = Rc::clone(&namespace);
    let mut current_value = None;
    for call in call_chain {
        match current_value {
            Some(AValue::RValue(RValue::Enum(r#enum), _)) => {

            },
            _ => {}
        }

        match *call {
            ast::Call::Ident(name) => {
                match current_namespace.get(&name) {
                    Some(RValue::Enum(r#enum))=> {
                        current_namespace = Rc::new(Namespace::try_with_enum(&r#enum)?);
                        current_value = Some(AValue::RValue(RValue::Enum(r#enum), name.span));
                    }
                    Some(rvalue_enum_constant @ RValue::EnumConstant(_)) => {
                        current_namespace = Rc::clone(&EMPTY_NAMESPACE);
                        current_value = Some(AValue::RValue(rvalue_enum_constant, name.span));
                    }
                    Some(RValue::Property(property)) => {
                        current_namespace = match &property.r#type {
                            im::Type::Struct(r#struct) => Rc::new(Namespace::try_with_struct(&r#struct)?),
                            im::Type::Enum(r#enum) => Rc::new(Namespace::try_with_enum(&r#enum)?),
                            im::Type::Event(_) => Rc::clone(&EMPTY_NAMESPACE)
                        };
                        current_value = Some(AValue::RValue(RValue::Property(property), name.span));
                    }
                    Some(RValue::Struct(r#struct)) => {
                        current_namespace = Rc::new(Namespace::try_with_struct(&r#struct)?);
                        current_value = Some(AValue::RValue(RValue::Struct(r#struct), name.span))
                    }
                    Some(rvalue) => {
                        let error = ConverterError::TypeNotSupported {
                            r#type: rvalue.r#type(),
                            span: name.span
                        };
                        return Err(error);
                    }
                    None => {
                        let error = ConverterError::CannotResolveIdent {
                            ident: name
                        };
                        return Err(error);
                    }
                };
            }
            ast::Call::IdentArguments { name, args, .. } => {
                match current_namespace.get(&name) {
                    Some(RValue::Function(function)) => {
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

                        let mut result = function.arguments.iter()
                            .zip(args.iter())
                            .filter_map(|(declared, actual)| {
                                if declared.types.contains_type(actual.r#type()) {
                                    None
                                } else {
                                    let error = ConverterError::MismatchedTypes {
                                        requested_ident: actual.name(),
                                        requested_type: actual.r#type(),
                                        resolved_ident: declared.name.clone(),
                                        resolved_type: declared.types.clone()
                                    };
                                    Some(error)
                                }
                            })
                            .collect::<Vec<_>>();

                        if let Some(pop) = result.pop() {
                            return Err(pop);
                        }

                        current_namespace = Rc::clone(&EMPTY_NAMESPACE);
                        current_value = Some(AValue::RValue(RValue::Function(function), name.span));
                    }
                    _ => todo!()
                };
            }
            ast::Call::String(string, span) => {
                current_namespace = Rc::clone(&EMPTY_NAMESPACE);
                let value = CValue::String(string, predefined.string_primitive.clone(), span);
                current_value = Some(AValue::CValue(value));
            }
            ast::Call::Number(number, span) => {
                current_namespace = Rc::clone(&EMPTY_NAMESPACE);
                let value = CValue::Number(number, predefined.num_primitive.clone(), span);
                current_value = Some(AValue::CValue(value));
            }
        }
    }

    Ok(current_value.unwrap())
}