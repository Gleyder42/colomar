use crate::compiler::cir::{CValue, Type};
use crate::compiler::codegen::{Arg, Caller, Codegen};
use crate::compiler::error::CompilerError;
use crate::compiler::wst::partial::Placeholder;
use crate::compiler::wst::Ident;
use crate::compiler::{cir, wst, QueryTrisult};
use crate::query_error;
use chumsky::Parser;
use std::collections::{HashMap, HashSet};

pub(super) fn query_wst_call(
    db: &dyn Codegen,
    caller: Option<Caller>,
    avalue_chain: cir::AValueChain,
) -> QueryTrisult<wst::Call> {
    QueryTrisult::Ok(avalue_chain.avalues).fold_flat_map(
        caller,
        |acc| acc.unwrap().wst.unwrap(),
        |acc, current| {
            db.query_wst_call_by_avalue(acc, current.clone())
                .map(|call| {
                    Some(Caller {
                        wst: call,
                        cir: current,
                    })
                })
        },
    )
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
    avalue: cir::AValue,
) -> QueryTrisult<Option<wst::Call>> {
    match avalue {
        cir::AValue::RValue(cir::RValue::Property(property_decl), _) => QueryTrisult::assume_or(
            property_decl.is_native.is_some() && caller.is_some(),
            "Only native instance properties are implemented",
            property_decl.name.span,
        )
        .flat_start(|| {
            let caller = caller.unwrap();
            let r#type = caller.cir.return_called_type(db).r#type;
            let mut map = HashMap::new();
            if let Some(caller) = caller.wst {
                map.insert(Placeholder::from("%caller%"), caller);
            }

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
                            .saturate(&mut map)
                            .map_err(CompilerError::PlaceholderError)
                            .into()
                    })
                    .inner_into_some()
                }
                Type::Event(event_id) => {
                    let event_decl: cir::EventDeclaration = db.lookup_intern_event_decl(event_id);
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
        }),
        cir::AValue::RValue(cir::RValue::EnumConstant(enum_constant_id), ..) => {
            let enum_constant: cir::EnumConstant = db.lookup_intern_enum_constant(enum_constant_id);
            let enum_decl: cir::EnumDeclaration = db.lookup_intern_enum_decl(enum_constant.r#enum);

            db.query_wscript_enum_constant_impl(enum_decl.name.value, enum_constant.name.value)
                .inner_into_some()
        }
        cir::AValue::RValue(cir::RValue::Type(Type::Enum(_)), ..) => QueryTrisult::Ok(None),
        cir::AValue::FunctionCall(func_decl_id, call_arg_ids, span) => {
            let func_decl: cir::FunctionDecl = db.lookup_intern_function_decl(func_decl_id);
            let called_args = call_arg_ids
                .into_iter()
                .map(|it| db.lookup_intern_called_argument(it))
                .collect();

            db.query_wst_call_from_args(func_decl.arguments, called_args)
                .into_iter()
                .map(|arg| db.query_wst_call(caller.clone(), arg.value))
                .collect::<QueryTrisult<Vec<_>>>()
                .map(|args| {
                    wst::Function {
                        args,
                        name: func_decl.name.into(),
                    }
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
            should have default values when generating workshop code";

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
