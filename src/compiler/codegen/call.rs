use crate::compiler::cir::{AValueChain, CValue, RValue, Type};
use crate::compiler::codegen::{
    Arg, Assigner, Caller, Codegen, ASSIGMENT_PLACEHOLDER, CALLER_PLACEHOLDER,
};
use crate::compiler::error::CompilerError;

use crate::compiler::span::Span;
use crate::compiler::wst::partial::Placeholder;
use crate::compiler::wst::Ident;
use crate::compiler::{cir, compiler_todo, wst, Op, QueryTrisult};
use crate::query_error;
use cir::{CalledArguments, FunctionDecl};
use std::collections::{HashMap, HashSet};

type ReplacementMap = HashMap<Placeholder, wst::Call>;

pub(super) fn query_wst_call(
    db: &dyn Codegen,
    caller: Option<Caller>,
    action: cir::Action,
) -> QueryTrisult<wst::Call> {
    let query_by_avalue = |chain: AValueChain, assigner: Option<Assigner>| {
        QueryTrisult::Ok(chain.avalues).fold_flat_map(
            // Caller is cloned here, because in an assignment the left and right side receives a caller
            caller.clone(),
            |acc| acc.unwrap().wst.unwrap(),
            |acc, current| {
                db.query_wst_call_by_avalue(acc, assigner.clone(), current.clone())
                    .map(|call| Some(Caller::new(call, current)))
            },
        )
    };

    match action {
        cir::Action::AvalueChain(avalue_chain)
        | cir::Action::Expr(cir::Expr::Chain(avalue_chain)) => query_by_avalue(avalue_chain, None),
        cir::Action::Assigment(left, right, assign_mod) => {
            let right = query_by_avalue(right, None);
            right.flat_map(|right| query_by_avalue(left, Some((right, assign_mod))))
        }
        cir::Action::Expr(expr) => {
            match expr {
                cir::Expr::Neg(neg) => {
                    // Recursive call
                    let call = db.query_wst_call(caller.clone(), cir::Action::Expr(*neg));
                    let negation = call.map(|call| wst::Condition {
                        right: Box::new(call),
                        op: Op::Equals,
                        left: Box::new(wst::Call::Boolean(false)),
                    });

                    negation.map(wst::Call::from)
                }
                cir::Expr::And(lhs, rhs) | cir::Expr::Or(lhs, rhs) => {
                    // Recursive call
                    let lhs = db.query_wst_call(caller.clone(), cir::Action::Expr(*lhs));
                    // Recursive call
                    let rhs = db.query_wst_call(caller.clone(), cir::Action::Expr(*rhs));

                    let and = lhs.and(rhs).map(|(lhs, rhs)| wst::Condition {
                        right: Box::new(rhs),
                        op: Op::And,
                        left: Box::new(lhs),
                    });

                    and.map(wst::Call::from)
                }
                cir::Expr::Chain(_) => unreachable!("Chain should already be checked"),
            }
        }
    }
}

pub(super) fn query_wst_call_by_avalue(
    db: &dyn Codegen,
    caller: Option<Caller>,
    assigner: Option<Assigner>,
    avalue: cir::AValue,
) -> QueryTrisult<Option<wst::Call>> {
    let mut replacement_map = HashMap::new();
    if let Some(Caller {
        wst: Some(ref caller),
        ..
    }) = caller
    {
        replacement_map.insert(Placeholder::from(CALLER_PLACEHOLDER), caller.clone());
    }
    if let Some(ref right_operand) = assigner {
        replacement_map.insert(
            Placeholder::from(ASSIGMENT_PLACEHOLDER),
            right_operand.0.clone(),
        );
    }

    match avalue {
        cir::AValue::RValue(rvalue, span) => {
            query_wst_call_by_rvalue(db, &replacement_map, rvalue, caller, assigner, span)
        }
        cir::AValue::FunctionCall(func_decl_id, call_arg_ids, span) => {
            let func_decl: FunctionDecl = db.lookup_intern_function_decl(func_decl_id);
            let called_args = call_arg_ids
                .into_iter()
                .map(|it| db.lookup_intern_called_argument(it))
                .collect();

            query_wst_call_by_function_call(
                db,
                &replacement_map,
                func_decl,
                called_args,
                caller,
                span,
            )
        }
        cir::AValue::CValue(cvalue) => query_wst_call_by_cvalue(db, cvalue),
    }
}

fn query_wst_call_by_rvalue(
    db: &dyn Codegen,
    replacement_map: &ReplacementMap,
    rvalue: RValue,
    caller: Option<Caller>,
    assigner: Option<Assigner>,
    span: Span,
) -> QueryTrisult<Option<wst::Call>> {
    match rvalue {
        RValue::Type(Type::Enum(_)) => QueryTrisult::Ok(None),
        RValue::Function(_func_id) => compiler_todo("Functions are not implemented", span),
        RValue::Property(property_id) => {
            let property = db.lookup_intern_property_decl(property_id);

            match (&property.is_native, caller, assigner) {
                (Some(_native), Some(caller), _) => {
                    query_wst_call_by_wscript_impl(db, replacement_map, property, caller)
                }
                (None, Some(_caller), Some(assigner)) => {
                    query_wst_call_by_assignment(db, replacement_map, property, assigner)
                }
                (None, Some(caller), None) => query_const_eval(db, caller.wst.unwrap())
                    .map(|caller_name| {
                        let call =
                            wst::Call::Property(caller_name, Ident::from_ident(property.name, db));
                        call
                    })
                    .inner_into_some(),
                _ => compiler_todo("Properties are not implemented", span),
            }
        }
        RValue::EnumConstant(enum_constant_id) => {
            let enum_constant = db.lookup_intern_enum_constant(enum_constant_id);
            let enum_decl = db.lookup_intern_enum_decl(enum_constant.r#enum);

            db.query_wscript_enum_constant_impl(
                enum_decl.name.value.name(db),
                enum_constant.name.value.name(db),
            )
            .inner_into_some()
        }
        _ => todo!(),
    }
}

fn query_wst_call_by_cvalue(db: &dyn Codegen, cvalue: CValue) -> QueryTrisult<Option<wst::Call>> {
    match cvalue {
        CValue::String(string, ..) => {
            let string = wst::Call::String(string.name(db));
            let custom_string = wst::Function {
                name: "Custom String".into(),
                args: vec![string],
            };
            QueryTrisult::Ok(custom_string.into()).inner_into_some()
        }
        CValue::Number(number, ..) => {
            let call = wst::Call::Number(number.name(db));

            QueryTrisult::Ok(call).inner_into_some()
        }
    }
}

fn query_wst_call_by_function_call(
    db: &dyn Codegen,
    replacement_map: &ReplacementMap,
    func_decl: FunctionDecl,
    called_args: CalledArguments,
    caller: Option<Caller>,
    _span: Span,
) -> QueryTrisult<Option<wst::Call>> {
    let wscript_function: QueryTrisult<wst::partial::Call> = db.query_wscript_struct_function_impl(
        func_decl.instance.unwrap().name(db),
        func_decl.name.value.name(db),
    );

    db.query_wst_call_from_args(func_decl.arguments, called_args)
        .into_iter()
        .map(|arg| {
            db.query_wst_call(caller.clone(), cir::Action::from(arg.value))
                .map(|call| (arg.name, call))
        })
        .collect::<QueryTrisult<Vec<_>>>()
        .and(wscript_function)
        .flat_map(|(args, wscript_function)| {
            // TODO Should the map be cloned here?
            let mut replacement_map = replacement_map.clone();

            for (name, call) in args {
                // TAssumes that .name(db) is correct here.
                replacement_map.insert(
                    Placeholder::from(format!("${}$", name.value.name(db))),
                    call,
                );
            }

            wscript_function
                .saturate(&replacement_map)
                .map_err(CompilerError::PlaceholderError)
                .into()
        })
        .inner_into_some()
}

fn query_wst_call_by_assignment(
    db: &dyn Codegen,
    replacement_map: &ReplacementMap,
    property_decl: cir::PropertyDecl,
    (_call, assign_mod): Assigner,
) -> QueryTrisult<Option<wst::Call>> {
    // TODO Should the map be cloned here?
    let mut replacement_map = replacement_map.clone();
    replacement_map.insert(
        Placeholder::from("$name$"),
        wst::Call::Ident(Ident::from_ident(property_decl.name, db)),
    );

    use wst::partial;
    let placeholder = |name| partial::Call::Placeholder(Placeholder::from(name));

    let function = match assign_mod {
        Some(assign_mod) => {
            replacement_map.insert(
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
        .saturate(&replacement_map)
        .map_err(CompilerError::PlaceholderError);
    QueryTrisult::from(call).inner_into_some()
}

fn query_wst_call_by_wscript_impl(
    db: &dyn Codegen,
    replacement_map: &ReplacementMap,
    property_decl: cir::PropertyDecl,
    caller: Caller,
) -> QueryTrisult<Option<wst::Call>> {
    QueryTrisult::flat_start(|| process_wscript(db, replacement_map, property_decl, caller))
}

fn process_wscript(
    db: &dyn Codegen,
    replacement_map: &ReplacementMap,
    property_decl: cir::PropertyDecl,
    caller: Caller,
) -> QueryTrisult<Option<wst::Call>> {
    let r#type = caller.cir.return_called_type(db).r#type;

    match r#type {
        Type::Enum(enum_id) => {
            let enum_decl: cir::EnumDeclaration = db.lookup_intern_enum_decl(enum_id);
            db.query_wscript_enum_constant_impl(
                enum_decl.name.value.name(db),
                property_decl.name.value.name(db),
            )
            .inner_into_some()
        }
        Type::Struct(struct_id) => {
            let struct_decl: cir::StructDeclaration = db.lookup_intern_struct_decl(struct_id);
            db.query_wscript_struct_property_impl(
                struct_decl.name.value.name(db),
                property_decl.name.value.name(db),
            )
            .flat_map(|partial_call| {
                partial_call
                    .saturate(replacement_map)
                    .map_err(CompilerError::PlaceholderError)
                    .into()
            })
            .inner_into_some()
        }
        Type::Event(event_id) => {
            let event_decl: cir::EventDeclaration = db.lookup_intern_event_decl(event_id);
            db.query_wscript_event_context_property_impl(
                event_decl.name.value.name(db),
                property_decl.name.value.name(db),
            )
            .inner_into_some()
        }
        Type::Unit => query_error!(CompilerError::NotImplemented(
            "Unit as caller is currently not implemented".into(),
            property_decl.name.span
        )),
    }
}

pub fn query_wst_call_from_args(
    db: &dyn Codegen,
    decl_args: cir::DeclaredArgumentIds,
    called_args: CalledArguments,
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

pub(super) fn query_const_eval(_db: &dyn Codegen, call: wst::Call) -> QueryTrisult<Ident> {
    match call {
        wst::Call::Condition(_) => query_error!(CompilerError::CannotEvalAsConst),
        wst::Call::String(_) => query_error!(CompilerError::CannotEvalAsConst),
        wst::Call::Number(_) => query_error!(CompilerError::CannotEvalAsConst),
        wst::Call::Boolean(_) => query_error!(CompilerError::CannotEvalAsConst),
        wst::Call::Ident(ident) => QueryTrisult::Ok(ident),
        wst::Call::Property(_, _) => query_error!(CompilerError::CannotEvalAsConst),
        wst::Call::Function(_) => query_error!(CompilerError::CannotEvalAsConst),
    }
}
