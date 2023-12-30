use super::error::CompilerError;
use super::trisult::{Errors, Trisult};
use super::{into_owned, workshop, wst, FullText, HashableMap, QueryTrisult};
use crate::tri;
use chumsky::input::{Input, Stream};
use chumsky::prelude::{end, SimpleSpan};
use chumsky::Parser;
use serde::Deserialize;
use std::fmt::Debug;
use std::fs;
use std::ops::Range;
use std::path::Path;

pub mod error_reporter;
pub mod wscript_impl;

#[salsa::query_group(WorkshopScriptLoaderDatabase)]
pub trait WorkshopScriptLoader {
    #[salsa::input]
    fn input_wscript_impls(&self) -> Vec<wscript_impl::Element>;

    fn query_wscript_struct_impls(&self) -> HashableMap<FullText, wscript_impl::Struct>;

    fn query_wscript_event_impls(&self) -> HashableMap<FullText, wscript_impl::Event>;

    fn query_wscript_enum_impls(&self) -> HashableMap<FullText, wscript_impl::Enum>;

    fn query_wscript_struct_impl(&self, name: FullText) -> QueryTrisult<wscript_impl::Struct>;

    fn query_wscript_event_impl(&self, name: FullText) -> QueryTrisult<wscript_impl::Event>;

    fn query_wscript_event_name_impl(&self, name: FullText) -> QueryTrisult<String>;

    fn query_wscript_enum_impl(&self, name: FullText) -> QueryTrisult<wscript_impl::Enum>;

    fn query_wscript_enum_constant_impl(
        &self,
        enum_name: FullText,
        constant_name: FullText,
    ) -> QueryTrisult<wst::Call>;

    fn query_wscript_struct_property_impl(
        &self,
        struct_name: FullText,
        property_name: FullText,
    ) -> QueryTrisult<wst::partial::Call>;

    /// Queries the workshop code of a colomar struct function.
    fn query_wscript_struct_function_impl(
        &self,
        struct_name: FullText,
        function_name: FullText,
    ) -> QueryTrisult<wst::partial::Call>;

    fn query_wscript_event_context_property_impl(
        &self,
        event_name: FullText,
        property_name: FullText,
    ) -> QueryTrisult<wst::Call>;
}

pub fn read_impls(dir: &Path) -> Vec<wscript_impl::Element> {
    let paths = [dir.join("enum"), dir.join("struct"), dir.join("event")];

    paths
        .into_iter()
        .filter(|path| path.exists() && path.is_dir())
        .flat_map(|path| {
            fs::read_dir(path)
                .expect("Filter already checks for existence and if the path is a dir")
        })
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
            element
        })
        .collect()
}

fn query_wscript_enum_constant_impl(
    db: &dyn WorkshopScriptLoader,
    enum_name: FullText,
    constant_name: FullText,
) -> QueryTrisult<wst::Call> {
    query_wscript_impl(
        || db.query_wscript_enum_impl(enum_name).map(|it| it.constants),
        constant_name,
    )
    .flat_map(|partial_call| {
        partial_call
            .complete()
            .map_err(CompilerError::PlaceholderError)
            .into()
    })
}

fn query_wscript_struct_function_impl(
    db: &dyn WorkshopScriptLoader,
    struct_name: FullText,
    function_name: FullText,
) -> QueryTrisult<wst::partial::Call> {
    query_wscript_impl(
        || {
            db.query_wscript_struct_impl(struct_name)
                .map(|it| it.functions)
        },
        function_name,
    )
}

fn query_wscript_struct_property_impl(
    db: &dyn WorkshopScriptLoader,
    struct_name: FullText,
    property_name: FullText,
) -> QueryTrisult<wst::partial::Call> {
    query_wscript_impl(
        || {
            db.query_wscript_struct_impl(struct_name)
                .map(|it| it.properties)
        },
        property_name,
    )
}

fn query_wscript_event_context_property_impl(
    db: &dyn WorkshopScriptLoader,
    enum_name: FullText,
    property_name: FullText,
) -> QueryTrisult<wst::Call> {
    query_wscript_impl(
        || db.query_wscript_event_impl(enum_name).map(|it| it.context),
        property_name,
    )
    .flat_map(|partial_call| {
        partial_call
            .complete()
            .map_err(CompilerError::PlaceholderError)
            .into()
    })
}

fn query_wscript_event_name_impl(
    db: &dyn WorkshopScriptLoader,
    name: FullText,
) -> QueryTrisult<String> {
    db.query_wscript_event_impl(name).map(|r#event| event.name)
}

fn query_wscript_impl(
    query: impl FnOnce() -> QueryTrisult<HashableMap<String, String>>,
    selection: FullText,
) -> QueryTrisult<wst::partial::Call> {
    let mut errors = Errors::new();
    // wscript_map contains mappings from functions defined in colomar and their workshop counterpart.
    let mut wscript_map = tri!(query(), errors);
    let wscript: Trisult<_, _> = wscript_map
        .remove(selection.as_str())
        .ok_or(CompilerError::CannotFindNativeDef(selection))
        .into();
    let wscript = tri!(wscript, errors);

    let (tokens, lexer_errors) = workshop::lexer::lexer()
        .then_ignore(end())
        .parse(&wscript)
        .into_output_errors();

    // Convert zero or more lexer_errors into a byte vec, which will be printed directly to stdout.
    // Essentially it will already print the errors in a nice readable format.
    // We cannot provide CompilerError::WstLexerError with the lexer_errors directly, because they contain a lifetime.
    // The reason is that errors may either borrow the underlying tokens or own them.
    // Both is possible at compile time.
    // Theoretically we should be able to force a static lifetime with unsafe code, to satisfy the compiler, as
    // we can ensure that all data is owned by the Rich error.
    // However, Rich error does not implement Hash.
    // Therefore the errors are now nicely printed.
    if !lexer_errors.is_empty() {
        // let error = CompilerError::WstLexerError(into_owned(lexer_errors));
        // errors.push(error);
    }

    let (call, parser_errors) = match tokens {
        Some(tokens) => {
            let eoi = SimpleSpan::new(tokens.len(), tokens.len() + 1);
            let stream = Stream::from_iter(tokens.into_iter()).spanned(eoi);

            workshop::parser::call()
                .then_ignore(end())
                .parse(stream)
                .into_output_errors()
        }
        None => (None, Vec::new()),
    };

    if !parser_errors.is_empty() {
        // let error = CompilerError::WstParserError(parser_errors);
        // errors.push(error);
    }

    return match call {
        Some(call) => errors.value(call),
        None => errors.fail_directly(),
    };
}

macro_rules! impl_wscript_queries {
    ($name:ident, $single_name:ident, $ele_name:ident, $wscript_impl:ty) => {
        fn $name(db: &dyn WorkshopScriptLoader) -> HashableMap<FullText, $wscript_impl> {
            db.input_wscript_impls()
                .into_iter()
                .filter_map(|element| {
                    if let wscript_impl::Element::$ele_name(name, val) = element {
                        Some((FullText::from(name), val))
                    } else {
                        None
                    }
                })
                .collect()
        }

        fn $single_name(
            db: &dyn WorkshopScriptLoader,
            name: FullText,
        ) -> QueryTrisult<$wscript_impl> {
            db.$name()
                .get(&name)
                .cloned()
                .ok_or(crate::error::CompilerError::CannotFindNativeDef(name))
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
