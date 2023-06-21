use std::collections::HashMap;
use chumsky::prelude::todo;
use smol_str::SmolStr;
use crate::language::analysis::{AnalysisError, QueryTrisult};
use crate::language::{HashableHashMap, im, ImmutableString};
use crate::language::im::{AValue, RValue};
use crate::language::lim::def::LimDefQuery;
use crate::language::lim::native;
use crate::language::lim::tree::{NativeCode, TemplateNativeCode};

pub(super) fn query_native_code(db: &dyn LimDefQuery, avalue: AValue) -> QueryTrisult<NativeCode> {
    match avalue {
        AValue::FunctionCall(_, _, _) => { todo!() }
        AValue::RValue(RValue::Property(property_decl), _) => {
            assert!(property_decl.is_native.is_some(), "Only native properties are currently implemented");
            assert!(property_decl.instance.is_some(), "Only instance properties are currently implemented");

            // Relies on assert
            let instance = property_decl.instance.unwrap();
            db.query_native_struct_property_code(instance.name(db), property_decl.name.value)
        }
        AValue::RValue(RValue::Type(_), _) => { todo!() }
        AValue::RValue(RValue::Function(_), _) => { todo!() }
        AValue::RValue(RValue::EnumConstant(_), _) => { todo!() }
        AValue::CValue(_) => { todo!() }
    }
}

pub(super) fn query_native_struct_property_code(
    db: &dyn LimDefQuery,
    struct_name: ImmutableString,
    property_name: ImmutableString
) -> QueryTrisult<NativeCode> {
    db.query_native_struct_code(struct_name)
        .flat_map(|native_struct| native_struct
            .properties
            .0
            .get(property_name.as_str())
            .ok_or(AnalysisError::CannotFindNativeDefinition(property_name))
            .cloned()
            .into()
        )
        // TODO Check code
        .map(|it| NativeCode::Template(TemplateNativeCode(ImmutableString::new(it))))
        .into()
}

pub(super) fn query_native_struct_code(db: &dyn LimDefQuery, name: ImmutableString) -> QueryTrisult<native::Struct> {
    db.query_native_struct_code_map()
        .0
        .get(&name)
        .cloned()
        .ok_or(AnalysisError::CannotFindNativeDefinition(name))
        .into()
}

// TODO Move this to another package
pub(super) fn query_native_struct_code_map(db: &dyn LimDefQuery) -> HashableHashMap<ImmutableString, native::Struct> {
    let map = db.input_native_code().into_iter()
        .filter_map(|element| {
            if let native::Element::Struct(name, r#struct) = element {
                Some((SmolStr::from(name), r#struct))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();
    HashableHashMap(map)
}