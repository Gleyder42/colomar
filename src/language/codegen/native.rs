use crate::language::analysis::{AnalysisError, QueryTrisult};
use crate::language::codegen::def::LimDefQuery;
use crate::language::codegen::native_conf;
use crate::language::im::{AValue, RValue, Type};
use crate::language::lim::{NativeCode, TemplateNativeCode};
use crate::language::{im, HashableHashMap, Text};
use smol_str::SmolStr;
use std::collections::{BTreeMap, HashMap};


pub(super) fn query_native_code(db: &dyn LimDefQuery, avalue: AValue) -> QueryTrisult<NativeCode> {
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

pub(super) fn query_native_event_context_code(
    db: &dyn LimDefQuery,
    struct_name: Text,
    property_name: Text,
) -> QueryTrisult<NativeCode> {
    db.query_native_event_conf(struct_name)
        .flat_map(|native_event| {
            native_event
                .context
                .get(property_name.as_str())
                .cloned()
                .ok_or(AnalysisError::CannotFindNativeDefinition(property_name))
                .into()
        })
        // TODO Check code
        .map(|it| NativeCode::Template(TemplateNativeCode(Text::new(it))))
        .into()
}

pub(super) fn query_native_event_conf(
    db: &dyn LimDefQuery,
    name: Text,
) -> QueryTrisult<native_conf::Event> {
    db.query_native_event_code_map()
        .get(&name)
        .cloned()
        .ok_or(AnalysisError::CannotFindNativeDefinition(name))
        .into()
}

// TODO Move this to another package
pub(super) fn query_native_event_code_map(db: &dyn LimDefQuery) -> BTreeMap<Text, native_conf::Event> {
    db.input_native_code()
        .into_iter()
        .filter_map(|element| {
            if let native_conf::Element::Event(name, r#struct) = element {
                Some((SmolStr::from(name), r#struct))
            } else {
                None
            }
        })
        .collect::<BTreeMap<_, _>>()
}
