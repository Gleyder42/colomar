use crate::compiler::cir::{AValueChain, CValue, Type};
use crate::compiler::codegen::{Arg, Caller, Codegen, ASSIGMENT_PLACEHOLDER, CALLER_PLACEHOLDER};
use crate::compiler::error::CompilerError;

use crate::compiler::wst::partial::Placeholder;
use crate::compiler::wst::Ident;
use crate::compiler::{cir, wst, AssignMod, QueryTrisult, Text};
use crate::query_error;
use std::collections::{HashMap, HashSet};

pub(super) fn query_wst_call(
    db: &dyn Codegen,
    caller: Option<Caller>,
    action: cir::Action,
) -> QueryTrisult<wst::Call> {
    let query_by_avalue =
        |avalue_chain: AValueChain, right_operand: Option<(wst::Call, Option<AssignMod>)>| {
            QueryTrisult::Ok(avalue_chain.avalues).fold_flat_map(
                // Caller is cloned here, because in an assignment the left and right side receives a caller
                caller.clone(),
                |acc| acc.unwrap().wst.unwrap(),
                |acc, current| {
                    db.query_wst_call_by_avalue(acc, right_operand.clone(), current.clone())
                        .map(|call| {
                            Some(Caller {
                                wst: call,
                                cir: current,
                            })
                        })
                },
            )
        };

    match action {
        cir::Action::AvalueChain(avalue_chain) => query_by_avalue(avalue_chain, None),
        cir::Action::Assigment(left, right, assign_mod) => query_by_avalue(right, None)
            .flat_map(|right| query_by_avalue(left, Some((right, assign_mod)))),
    }
}

pub(super) fn query_const_eval(_db: &dyn Codegen, call: wst::Call) -> QueryTrisult<wst::Ident> {
    match call {
        wst::Call::Condition(_) => query_error!(CompilerError::CannotEvalAsConst),
        wst::Call::String(_) => query_error!(CompilerError::CannotEvalAsConst),
        wst::Call::Number(_) => query_error!(CompilerError::CannotEvalAsConst),
        wst::Call::Ident(ident) => QueryTrisult::Ok(ident),
        wst::Call::Function(_) => query_error!(CompilerError::CannotEvalAsConst),
    }
}

pub(super) fn query_wst_call_by_avalue(
    db: &dyn Codegen,
    caller: Option<Caller>,
    right_operand: Option<(wst::Call, Option<AssignMod>)>,
    avalue: cir::AValue,
) -> QueryTrisult<Option<wst::Call>> {
    let mut map = HashMap::new();
    if let Some(Caller {
        wst: Some(ref caller),
        ..
    }) = caller
    {
        map.insert(Placeholder::from(CALLER_PLACEHOLDER), caller.clone());
    }
    if let Some(ref right_operand) = right_operand {
        map.insert(
            Placeholder::from(ASSIGMENT_PLACEHOLDER),
            right_operand.0.clone(),
        );
    }

    match avalue {
        cir::AValue::RValue(cir::RValue::Property(property_decl), _)
            if property_decl.is_native.is_some() && caller.is_some() =>
        {
            QueryTrisult::empty().flat_start(|| {
                let caller = caller.unwrap();
                let r#type = caller.cir.return_called_type(db).r#type;

                match r#type {
                    Type::Enum(enum_id) => {
                        let enum_decl: cir::EnumDeclaration = db.lookup_intern_enum_decl(enum_id);
                        db.query_wscript_enum_constant_impl(
                            enum_decl.name.value,
                            property_decl.name.value,
                        )
                        .inner_into_some()
                    }
                    Type::Struct(struct_id) => {
                        let struct_decl: cir::StructDeclaration =
                            db.lookup_intern_struct_decl(struct_id);
                        db.query_wscript_struct_property_impl(
                            struct_decl.name.value,
                            property_decl.name.value,
                        )
                        .flat_map(|partial_call| {
                            partial_call
                                .saturate(&map)
                                .map_err(CompilerError::PlaceholderError)
                                .into()
                        })
                        .inner_into_some()
                    }
                    Type::Event(event_id) => {
                        let event_decl: cir::EventDeclaration =
                            db.lookup_intern_event_decl(event_id);
                        db.query_wscript_event_context_property_impl(
                            event_decl.name.value,
                            property_decl.name.value,
                        )
                        .inner_into_some()
                    }
                    Type::Unit => query_error!(CompilerError::NotImplemented(
                        "Unit as caller is currently not implemented".into(),
                        property_decl.name.span
                    )),
                }
            })
        }
        cir::AValue::RValue(cir::RValue::Property(property_decl), _)
            if property_decl.is_native.is_none() && caller.is_some() && right_operand.is_some() =>
        {
            let right_operand = right_operand.unwrap();

            // TODO Should the map be cloned here?
            let mut map = map.clone();
            map.insert(
                Placeholder::from("$name$"),
                wst::Call::Ident(Ident::from(property_decl.name)),
            );

            use wst::partial;
            let placeholder = |name| partial::Call::Placeholder(Placeholder(Text::from(name)));

            let function = match right_operand.1 {
                Some(assign_mod) => {
                    map.insert(
                        Placeholder::from("$assign_mod$"),
                        wst::Call::Ident(Ident::from(assign_mod.to_string())),
                    );

                    // TODO Is this the right place to define workshop functions?
                    // TODO Decide where to place fundamental workshop functions.
                    const MODIFY_PLAYER_VARIABLE: &str = "Modify Player Variable";
                    partial::Function {
                        name: wst::Ident::from(MODIFY_PLAYER_VARIABLE),
                        args: vec![
                            placeholder("$caller$"),
                            placeholder("$name$"),
                            placeholder("$assign_mod$"),
                            placeholder("$value$"),
                        ],
                    }
                }
                None => {
                    const SET_PLAYER_VARIABLE: &str = "Set Player Variable";
                    partial::Function {
                        name: wst::Ident::from(SET_PLAYER_VARIABLE),
                        args: vec![
                            placeholder("$caller$"),
                            placeholder("$name$"),
                            placeholder("$value$"),
                        ],
                    }
                }
            };

            let call = partial::Call::Function(function);
            let call = call
                .saturate(&map)
                .map_err(|error| CompilerError::PlaceholderError(error));
            QueryTrisult::from(call).inner_into_some()
        }
        cir::AValue::RValue(cir::RValue::EnumConstant(enum_constant_id), ..) => {
            let enum_constant: cir::EnumConstant = db.lookup_intern_enum_constant(enum_constant_id);
            let enum_decl: cir::EnumDeclaration = db.lookup_intern_enum_decl(enum_constant.r#enum);

            db.query_wscript_enum_constant_impl(enum_decl.name.value, enum_constant.name.value)
                .inner_into_some()
        }
        cir::AValue::RValue(cir::RValue::Type(Type::Enum(_)), ..) => QueryTrisult::Ok(None),
        cir::AValue::FunctionCall(func_decl_id, call_arg_ids, _span) => {
            let func_decl: cir::FunctionDecl = db.lookup_intern_function_decl(func_decl_id);
            let called_args = call_arg_ids
                .into_iter()
                .map(|it| db.lookup_intern_called_argument(it))
                .collect();

            let wscript_function: QueryTrisult<wst::partial::Call> = db
                .query_wscript_struct_function_impl(
                    func_decl.instance.unwrap().name(db),
                    func_decl.name.value,
                );

            db.query_wst_call_from_args(func_decl.arguments, called_args)
                .into_iter()
                .map(|arg| {
                    db.query_wst_call(caller.clone(), cir::Action::from(arg.value))
                        .map(|call| (arg.name, call))
                })
                .collect::<QueryTrisult<Vec<_>>>()
                .and_require(wscript_function)
                .flat_map(|(args, wscript_function)| {
                    // TODO Should the map be cloned here?
                    let mut map = map.clone();

                    for (name, call) in args {
                        map.insert(Placeholder::from(format!("${}$", name.value)), call);
                    }

                    wscript_function
                        .saturate(&mut map)
                        .map_err(CompilerError::PlaceholderError)
                        .into()
                })
                .inner_into_some()
        }
        cir::AValue::CValue(CValue::String(string, _, _)) => {
            let string = wst::Call::String(string);
            let custom_string = wst::Function {
                name: "Custom String".into(),
                args: vec![string],
            };
            QueryTrisult::Ok(custom_string.into()).inner_into_some()
        }
        cir::AValue::CValue(CValue::Number(number, _, _)) => {
            let call = wst::Call::Number(number);

            QueryTrisult::Ok(call).inner_into_some()
        }
        avalue => query_error!(CompilerError::NotImplemented(
            format!("Current avalue {:?} is not implemented", avalue).into(),
            avalue.span()
        )),
    }
}

pub fn query_wst_call_from_args(
    db: &dyn Codegen,
    decl_args: cir::DeclaredArgumentIds,
    called_args: cir::CalledArguments,
) -> Vec<Arg> {
    let all_decl_args: HashSet<_> = decl_args.into_iter().collect();

    let supplied_decl_args: HashSet<_> = called_args
        .iter()
        .map(|called_arg| called_arg.declared)
        .collect();

    let defaulted_args = all_decl_args
        .difference(&supplied_decl_args)
        .map(|decl_arg_id| db.lookup_intern_decl_arg(*decl_arg_id))
        .collect::<Vec<cir::DeclaredArgument>>();
    let supplied_args = called_args;

    const ERROR: &str = "Compiler Bug:
            Not supplied arguments (e.g. arguments for a function)\
            should have default values when generating native code";

    let mut args: Vec<_> = defaulted_args
        .into_iter()
        .map(|decl_arg| Arg {
            index: decl_arg.position,
            name: decl_arg.name,
            value: decl_arg.default_value.expect(ERROR),
        })
        .chain(supplied_args.into_iter().map(|called_arg| {
            let decl_arg: cir::DeclaredArgument = db.lookup_intern_decl_arg(called_arg.declared);
            Arg {
                index: decl_arg.position,
                name: decl_arg.name,
                value: called_arg.value,
            }
        }))
        .collect();

    args.sort_by_key(|arg| arg.index);
    args
}
