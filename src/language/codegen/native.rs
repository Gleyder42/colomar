use crate::language::analysis::{AnalysisError, QueryTrisult};
use crate::language::codegen::def::LimDefQuery;
use crate::language::codegen::{owscript_impl, HashableMap};
use crate::language::im::{AValue, PropertyDecl, RValue, Type};
use crate::language::lim::{LiteralOwscript, Owscript, PlaceholderOwscript};
use crate::language::{im, HashableHashMap, Text};
use crate::query_error;
use lazy_static::lazy_static;
use regex::Regex;
use smol_str::SmolStr;
use std::collections::{BTreeMap, HashMap};

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

    match instance {
        Type::Enum(_) => db
            .query_owscript_event_context_variable_impl(instance.name(db), property_decl.name.value)
            .map(|literal| Owscript::Literal(literal)),
        Type::Struct(_) => db
            .query_owscript_struct_property_impl(instance.name(db), property_decl.name.value)
            .map(|raw_script| Owscript::from(raw_script)),
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
) -> QueryTrisult<String> {
    db.query_owscript_struct_impl(struct_name)
        .flat_map(|owscript_impl| {
            owscript_impl
                .properties
                .get(property_name.as_str())
                .cloned()
                .ok_or(AnalysisError::CannotFindNativeDefinition(property_name))
                .into()
        })
}

pub(super) fn query_owscript_event_context_variable_impl(
    db: &dyn LimDefQuery,
    struct_name: Text,
    property_name: Text,
) -> QueryTrisult<LiteralOwscript> {
    db.query_owscript_event_impl(struct_name)
        .flat_map(|native_event| {
            native_event
                .context
                .get(property_name.as_str())
                .cloned()
                .ok_or(AnalysisError::CannotFindNativeDefinition(property_name))
                .into()
        })
        .flat_map(|script| {
            Owscript::from(script)
                .literal_or_none()
                .ok_or(AnalysisError::InvalidNativeDefinition(
                    "Owscript for event context variables must be literal",
                ))
                .into()
        })
}

macro_rules! impl_owscript_queries {
    ($name:ident, $single_name:ident, $ele_name:ident, $owscript_impl:ty) => {
        pub(super) fn $name(db: &dyn LimDefQuery) -> HashableMap<Text, $owscript_impl> {
            db.input_owscript_impls()
                .into_iter()
                .filter_map(|element| {
                    if let owscript_impl::Element::$ele_name(name, val) = element {
                        Some((Text::from(name), val))
                    } else {
                        None
                    }
                })
                .collect()
        }

        pub(super) fn $single_name(
            db: &dyn LimDefQuery,
            name: Text,
        ) -> QueryTrisult<$owscript_impl> {
            db.$name()
                .get(&name)
                .cloned()
                .ok_or(AnalysisError::CannotFindNativeDefinition(name))
                .into()
        }
    };
}

impl_owscript_queries!(
    query_owscript_struct_impls,
    query_owscript_struct_impl,
    Struct,
    owscript_impl::Struct
);

impl_owscript_queries!(
    query_owscript_event_impls,
    query_owscript_event_impl,
    Event,
    owscript_impl::Event
);

impl_owscript_queries!(
    query_owscript_enum_impls,
    query_owscript_enum_impl,
    Enum,
    owscript_impl::Enum
);
