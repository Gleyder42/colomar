use crate::compiler::cir::Type;
use crate::compiler::codegen::{Caller, Codegen};
use crate::compiler::error::CompilerError;
use crate::compiler::wst::partial::Placeholder;
use crate::compiler::{cir, wst, QueryTrisult, Text};
use crate::query_error;
use std::collections::HashMap;

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

pub(super) fn query_const_eval(db: &dyn Codegen, call: wst::Call) -> QueryTrisult<wst::Ident> {
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
            property_decl.name.span.clone(),
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
                            .map_err(|reason| CompilerError::PlaceholderError(reason))
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
                    "Unit as caller is currently not implemented",
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
        avalue @ _ => query_error!(CompilerError::NotImplemented(
            "Current avalue is not implemented",
            avalue.span()
        )),
    }
}
