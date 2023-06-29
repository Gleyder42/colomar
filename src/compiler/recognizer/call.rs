use std::collections::HashMap;
use std::thread::current;
use crate::compiler::{cir, QueryTrisult, Span, Text, wir, wst};
use crate::compiler::cir::Type;
use crate::compiler::cst::DeclaredArguments;
use crate::compiler::error::CompilerError;
use crate::compiler::recognizer::{Caller, Recognizer};
use crate::compiler::wst::{Ident, Placeholder};
use crate::query_error;

pub(super) fn query_wir_call(db: &dyn Recognizer, caller: Option<Caller>, avalue_chain: cir::AValueChain) -> QueryTrisult<wst::Call> {
    QueryTrisult::Ok(avalue_chain.avalues)
        .fold_flat_map(caller, |acc| acc.unwrap().wst.unwrap(), |acc, current| {
            db.query_wst_call_by_avalue(acc, current.clone())
                .map(|call| Some(Caller { wst: Some(call), cir: current }) )
        })
}

pub(super) fn query_wst_call_by_avalue(db: &dyn Recognizer, caller: Option<Caller>, avalue: cir::AValue) -> QueryTrisult<wst::Call> {
    match avalue {
        cir::AValue::RValue(cir::RValue::Property(property_decl), _) => {
            QueryTrisult::assume_or(
                property_decl.is_native.is_some() && caller.is_some(),
                "Only native instance properties are implemented",
                property_decl.name.span.clone(),
            ).flat_start(|| {
                let caller = caller.unwrap();
                let r#type = caller.cir.return_called_type(db).r#type;
                let mut map = HashMap::new();
                if let Some(caller) = caller.wst {
                    map.insert(Placeholder(Text::new("%caller%")), caller);
                }

                match r#type {
                    Type::Enum(enum_id) => {
                        let enum_decl: cir::EnumDeclaration = db.lookup_intern_enum_decl(enum_id);
                        db.query_wscript_enum_constant_impl(enum_decl.name.value, property_decl.name.value)
                    }
                    Type::Struct(struct_id) => {
                        let struct_decl: cir::StructDeclaration = db.lookup_intern_struct_decl(struct_id);
                        db.query_wscript_struct_property_impl(struct_decl.name.value, property_decl.name.value)
                            .flat_map(|partial_call| partial_call.saturate(&mut map)
                                .map_err(|reason| CompilerError::PlaceholderError(reason))
                                .into()
                            )
                    }
                    Type::Event(event_id) => {
                        let event_decl: cir::EventDeclaration = db.lookup_intern_event_decl(event_id);
                        db.query_wscript_event_context_property_impl(event_decl.name.value, property_decl.name.value)
                    }
                    Type::Unit => query_error!(CompilerError::NotImplemented("Unit as caller is currently not implemented", property_decl.name.span))
                }
            })
        }
        avalue @ _ => query_error!(CompilerError::NotImplemented("Current avalue is not implemented", avalue.span()))
    }
}
