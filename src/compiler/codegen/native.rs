use crate::compiler::cir::{AValue, PropertyDecl, RValue, Type};
use crate::compiler::codegen::def::LimDefQuery;
use crate::compiler::codegen::wscript_script_impl;
use crate::compiler::error::CompilerError;
use crate::compiler::wir::Owscript;
use crate::compiler::{HashableMap, QueryTrisult, Text};

pub(super) fn query_native_code(db: &dyn LimDefQuery, avalue: AValue) -> QueryTrisult<Owscript> {
    match avalue {
        AValue::FunctionCall(_, _, _) => {
            todo!()
        }
        AValue::RValue(RValue::Property(property_decl), _) => {
            assert!(
                property_decl.is_native.is_some(),
                "Only native properties are currently implemented"
            );
            assert!(
                property_decl.instance.is_some(),
                "Only instance properties are currently implemented"
            );

            // Relies on assert
            let instance = property_decl.instance.unwrap();
            db.query_owscript_event_context_variable_impl(
                instance.name(db),
                property_decl.name.value,
            );
            todo!()
        }
        AValue::RValue(RValue::Type(_), _) => {
            todo!()
        }
        AValue::RValue(RValue::Function(_), _) => {
            todo!()
        }
        AValue::RValue(RValue::EnumConstant(_), _) => {
            todo!()
        }
        AValue::CValue(_) => {
            todo!()
        }
    }
}

pub(super) fn query_owscript_property_impl(
    db: &dyn LimDefQuery,
    property_decl: PropertyDecl,
) -> QueryTrisult<Owscript> {
    assert!(
        property_decl.is_native.is_some(),
        "Only native properties are currently implemented"
    );
    assert!(
        property_decl.instance.is_some(),
        "Only instance properties are currently implemented"
    );

    // Relies on assert
    let instance = property_decl.instance.unwrap();

    let struct_name = instance.name(db);
    let property_name = property_decl.name.value;
    match instance {
        Type::Enum(_) => db.query_owscript_event_context_variable_impl(struct_name, property_name),
        Type::Struct(_) => db.query_owscript_struct_property_impl(struct_name, property_name),
        Type::Event(_) => {
            todo!("Event properties are not yet implemented")
        }
        Type::Unit => {
            todo!("Unit properties are not yet implemented")
        }
    }
}

pub(super) fn query_owscript_struct_property_impl(
    db: &dyn LimDefQuery,
    struct_name: Text,
    property_name: Text,
) -> QueryTrisult<Owscript> {
    db.query_owscript_struct_impl(struct_name)
        .flat_map(|owscript_impl| {
            owscript_impl
                .properties
                .get(property_name.as_str())
                .cloned()
                .ok_or(CompilerError::CannotFindNativeDefinition(property_name))
                .into()
        })
        .map(|script| Owscript::from(script))
}

pub(super) fn query_owscript_event_context_variable_impl(
    db: &dyn LimDefQuery,
    struct_name: Text,
    property_name: Text,
) -> QueryTrisult<Owscript> {
    db.query_owscript_event_impl(struct_name)
        .flat_map(|native_event| {
            native_event
                .context
                .get(property_name.as_str())
                .cloned()
                .ok_or(CompilerError::CannotFindNativeDefinition(property_name))
                .into()
        })
        .map(|script| Owscript::from(script))
}


