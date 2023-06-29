use crate::compiler::error::CompilerError;
use crate::compiler::trisult::Trisult;
use crate::compiler::{workshop, wst, HashableMap, QueryTrisult, Text};
use chumsky::prelude::end;
use chumsky::Parser;
use serde::Deserialize;
use std::fs;
use std::path::Path;

pub mod wscript_impl;

#[salsa::query_group(WorkshopScriptLoaderDatabase)]
pub trait WorkshopScriptLoader {
    #[salsa::input]
    fn input_wscript_impls(&self) -> Vec<wscript_impl::Element>;

    fn query_wscript_struct_impls(&self) -> HashableMap<Text, wscript_impl::Struct>;

    fn query_wscript_event_impls(&self) -> HashableMap<Text, wscript_impl::Event>;

    fn query_wscript_enum_impls(&self) -> HashableMap<Text, wscript_impl::Enum>;

    fn query_wscript_struct_impl(&self, name: Text) -> QueryTrisult<wscript_impl::Struct>;

    fn query_wscript_event_impl(&self, name: Text) -> QueryTrisult<wscript_impl::Event>;

    fn query_wscript_enum_impl(&self, name: Text) -> QueryTrisult<wscript_impl::Enum>;

    fn query_wscript_enum_constant_impl(
        &self,
        enum_name: Text,
        constant_name: Text,
    ) -> QueryTrisult<wst::Call>;

    fn query_wscript_struct_property_impl(
        &self,
        struct_name: Text,
        function_name: Text,
    ) -> QueryTrisult<wst::partial::Call>;

    fn query_wscript_event_context_property_impl(
        &self,
        event_name: Text,
        property_name: Text,
    ) -> QueryTrisult<wst::Call>;
}

pub fn read_impls(dir: &Path) -> Vec<wscript_impl::Element> {
    let paths = [
        dir.join(Path::new("enum")),
        dir.join(Path::new("struct")),
        dir.join(Path::new("event")),
    ];

    paths
        .into_iter()
        .flat_map(|path| fs::read_dir(path).unwrap())
        .map(|it| it.unwrap())
        .map(|entry| {
            #[derive(Deserialize, Debug)]
            #[serde(untagged)]
            enum Action {
                Struct(wscript_impl::Struct),
                Event(wscript_impl::Event),
                Enum(wscript_impl::Enum),
            }

            let content = fs::read_to_string(entry.path()).unwrap();
            let name = entry
                .path()
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .to_string();
            let result: Action = toml::from_str(&content).expect(&content);
            let element = match result {
                Action::Struct(r#struct) => wscript_impl::Element::Struct(name, r#struct),
                Action::Event(event) => wscript_impl::Element::Event(name, event),
                Action::Enum(r#enum) => wscript_impl::Element::Enum(name, r#enum),
            };
            println!("{:?}", element);
            element
        })
        .collect()
}

fn query_wscript_enum_constant_impl(
    db: &dyn WorkshopScriptLoader,
    enum_name: Text,
    constant_name: Text,
) -> QueryTrisult<wst::Call> {
    query_wscript_impl(
        || db.query_wscript_enum_impl(enum_name).map(|it| it.constants),
        constant_name,
    )
    .flat_map(|partial_call| {
        partial_call
            .complete()
            .map_err(|err| CompilerError::PlaceholderError(err))
            .into()
    })
}

fn query_wscript_struct_property_impl(
    db: &dyn WorkshopScriptLoader,
    struct_name: Text,
    function_name: Text,
) -> QueryTrisult<wst::partial::Call> {
    query_wscript_impl(
        || {
            db.query_wscript_struct_impl(struct_name)
                .map(|it| it.properties)
        },
        function_name,
    )
}

fn query_wscript_event_context_property_impl(
    db: &dyn WorkshopScriptLoader,
    enum_name: Text,
    property_name: Text,
) -> QueryTrisult<wst::Call> {
    query_wscript_impl(
        || db.query_wscript_event_impl(enum_name).map(|it| it.context),
        property_name,
    )
    .flat_map(|partial_call| {
        partial_call
            .complete()
            .map_err(|err| CompilerError::PlaceholderError(err))
            .into()
    })
}

fn query_wscript_impl(
    query: impl FnOnce() -> QueryTrisult<HashableMap<String, String>>,
    selection: Text,
) -> QueryTrisult<wst::partial::Call> {
    query()
        .flat_map(|map| {
            map.get(selection.as_str())
                .ok_or(CompilerError::CannotFindNativeDefinition(selection))
                .cloned()
                .into()
        })
        .flat_map(|wscript| {
            let tokens: Result<Trisult<_, _>, _> = workshop::lexer::lexer()
                .then_ignore(end())
                .parse_recovery(wscript.as_str())
                .try_into();
            let tokens = tokens
                .unwrap()
                .map_errors(|error| CompilerError::WstLexerError);

            tokens.flat_map(|tokens| {
                let trisult: Result<Trisult<_, _>, _> = workshop::parser::call()
                    .then_ignore(end())
                    .parse_recovery(tokens)
                    .try_into();

                trisult
                    .unwrap()
                    .map_errors(|error| CompilerError::WstParserError)
            })
        })
}

macro_rules! impl_wscript_queries {
    ($name:ident, $single_name:ident, $ele_name:ident, $wscript_impl:ty) => {
        fn $name(db: &dyn WorkshopScriptLoader) -> HashableMap<Text, $wscript_impl> {
            db.input_wscript_impls()
                .into_iter()
                .filter_map(|element| {
                    if let wscript_impl::Element::$ele_name(name, val) = element {
                        Some((Text::from(name), val))
                    } else {
                        None
                    }
                })
                .collect()
        }

        fn $single_name(db: &dyn WorkshopScriptLoader, name: Text) -> QueryTrisult<$wscript_impl> {
            db.$name()
                .get(&name)
                .cloned()
                .ok_or(crate::compiler::error::CompilerError::CannotFindNativeDefinition(name))
                .into()
        }
    };
}

impl_wscript_queries!(
    query_wscript_struct_impls,
    query_wscript_struct_impl,
    Struct,
    wscript_impl::Struct
);

impl_wscript_queries!(
    query_wscript_event_impls,
    query_wscript_event_impl,
    Event,
    wscript_impl::Event
);

impl_wscript_queries!(
    query_wscript_enum_impls,
    query_wscript_enum_impl,
    Enum,
    wscript_impl::Enum
);
