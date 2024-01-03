use super::error::PartialCompilerError;
use super::trisult::{Errors, Trisult};
use super::{into_owned, span, workshop, wst, PartialQueryTrisult, Text};
use crate::analysis::interner::Interner;
use crate::tri;
use chumsky::input::{Input, Stream};
use chumsky::prelude::end;
use chumsky::Parser;
use hashlink::LinkedHashMap;
use serde::Deserialize;
use std::fmt::Debug;
use std::fs;
use std::path::Path;
use std::rc::Rc;

pub mod wscript_impl;

use crate::span::Offset;
use wscript_impl::ImplEntry;

#[salsa::query_group(WorkshopScriptLoaderDatabase)]
pub trait WorkshopScriptLoader: Interner {
    #[salsa::input]
    fn input_wscript_impls(&self) -> Vec<ImplEntry<wscript_impl::Element>>;

    fn query_wscript_struct_impls(&self) -> LinkedHashMap<Text, ImplEntry<wscript_impl::Struct>>;

    fn query_wscript_event_impls(&self) -> LinkedHashMap<Text, ImplEntry<wscript_impl::Event>>;

    fn query_wscript_enum_impls(&self) -> LinkedHashMap<Text, ImplEntry<wscript_impl::Enum>>;

    fn query_wscript_struct_impl(
        &self,
        name: Text,
    ) -> PartialQueryTrisult<ImplEntry<wscript_impl::Struct>>;

    fn query_wscript_event_impl(
        &self,
        name: Text,
    ) -> PartialQueryTrisult<ImplEntry<wscript_impl::Event>>;

    fn query_wscript_event_name_impl(&self, name: Text) -> PartialQueryTrisult<String>;

    fn query_wscript_enum_impl(
        &self,
        name: Text,
    ) -> PartialQueryTrisult<ImplEntry<wscript_impl::Enum>>;

    /// Impl [query_wscript_enum_constant_impl]
    fn query_wscript_enum_constant_impl(
        &self,
        enum_name: Text,
        constant_name: Text,
    ) -> PartialQueryTrisult<wst::Call>;

    /// Impl [query_wscript_struct_property_impl]
    fn query_wscript_struct_property_impl(
        &self,
        struct_name: Text,
        property_name: Text,
    ) -> PartialQueryTrisult<wst::partial::Call>;

    /// Queries the workshop code of a colomar struct function.
    /// Impl [query_wscript_struct_function_impl]
    fn query_wscript_struct_function_impl(
        &self,
        struct_name: Text,
        function_name: Text,
    ) -> PartialQueryTrisult<wst::partial::Call>;

    /// Impl [query_wscript_event_context_property_impl]
    fn query_wscript_event_context_property_impl(
        &self,
        event_name: Text,
        property_name: Text,
    ) -> PartialQueryTrisult<wst::Call>;
}

pub fn read_impls(dir: &Path) -> Vec<ImplEntry<wscript_impl::Element>> {
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
            ImplEntry {
                path: entry.path(),
                value: element,
            }
        })
        .collect()
}

fn query_wscript_enum_constant_impl(
    db: &dyn WorkshopScriptLoader,
    enum_name: Text,
    constant_name: Text,
) -> PartialQueryTrisult<wst::Call> {
    query_wscript_impl(
        db,
        || {
            db.query_wscript_enum_impl(enum_name)
                .map_impl_entry(|it| it.constants)
        },
        constant_name,
    )
    .flat_map(|partial_call| {
        partial_call
            .complete()
            .map_err(PartialCompilerError::PlaceholderError)
            .into()
    })
}

fn query_wscript_struct_function_impl(
    db: &dyn WorkshopScriptLoader,
    struct_name: Text,
    function_name: Text,
) -> PartialQueryTrisult<wst::partial::Call> {
    let query = db
        .query_wscript_struct_impl(struct_name)
        .map_impl_entry(|it| it.functions);
    query_wscript_impl(db, || query, function_name)
}

fn query_wscript_struct_property_impl(
    db: &dyn WorkshopScriptLoader,
    struct_name: Text,
    property_name: Text,
) -> PartialQueryTrisult<wst::partial::Call> {
    query_wscript_impl(
        db,
        || {
            db.query_wscript_struct_impl(struct_name)
                .map_impl_entry(|it| it.properties)
        },
        property_name,
    )
}

fn query_wscript_event_context_property_impl(
    db: &dyn WorkshopScriptLoader,
    enum_name: Text,
    property_name: Text,
) -> PartialQueryTrisult<wst::Call> {
    query_wscript_impl(
        db,
        || {
            db.query_wscript_event_impl(enum_name)
                .map_impl_entry(|it| it.context)
        },
        property_name,
    )
    .flat_map(|partial_call| {
        partial_call
            .complete()
            .map_err(PartialCompilerError::PlaceholderError)
            .into()
    })
}

fn query_wscript_event_name_impl(
    db: &dyn WorkshopScriptLoader,
    name: Text,
) -> PartialQueryTrisult<String> {
    db.query_wscript_event_impl(name)
        .map(|entry| entry.value.name)
}

// Todo add this to the interface
fn query_wscript_impl(
    db: &dyn WorkshopScriptLoader,
    query: impl FnOnce() -> PartialQueryTrisult<ImplEntry<LinkedHashMap<String, String>>>,
    selection: Text,
) -> PartialQueryTrisult<wst::partial::Call> {
    let mut errors = Errors::new();
    // wscript_map contains mappings from functions defined in colomar and their workshop counterpart.
    let mut wscript_map = tri!(query(), errors);
    let wscript: Trisult<_, _> = wscript_map
        .value
        .remove(selection.as_str())
        // lazy eval so we only clone on error
        .ok_or_else(|| PartialCompilerError::CannotFindNativeDef(selection.clone()))
        .into();
    let wscript = tri!(wscript, errors);

    let span_source_id = db.intern_span_source(wscript_map.path.clone());
    let (tokens, lexer_errors) = workshop::lexer::lexer()
        .then_ignore(end())
        .parse(workshop::lexer::input_from_str(&wscript, span_source_id))
        .into_output_errors();

    let wscript = Rc::from(wscript);

    if !lexer_errors.is_empty() {
        let error = PartialCompilerError::WstLexerError(
            wscript_map.path.clone(),
            selection.clone(),
            Rc::clone(&wscript),
            into_owned(lexer_errors),
        );
        errors.push(error);
    }

    let (call, parser_errors) = match tokens {
        Some(tokens) => {
            let eoi = span::Span {
                context: span_source_id,
                offset: Offset::from(tokens.len()..tokens.len() + 1),
            };
            let stream = Stream::from_iter(tokens.into_iter()).spanned(eoi);

            workshop::parser::call()
                .then_ignore(end())
                .parse(stream)
                .into_output_errors()
        }
        None => (None, Vec::new()),
    };

    if !parser_errors.is_empty() {
        let error = PartialCompilerError::WstParserError(
            wscript_map.path,
            selection,
            Rc::clone(&wscript),
            into_owned(parser_errors),
        );
        errors.push(error);
    }

    return match call {
        Some(call) => errors.value(call),
        None => errors.fail_directly(),
    };
}

macro_rules! impl_wscript_queries {
    ($name:ident, $single_name:ident, $ele_name:ident, $wscript_impl:ty) => {
        fn $name(db: &dyn WorkshopScriptLoader) -> LinkedHashMap<Text, ImplEntry<$wscript_impl>> {
            db.input_wscript_impls()
                .into_iter()
                .filter_map(|entry| {
                    if let wscript_impl::Element::$ele_name(name, val) = entry.value {
                        Some((
                            Text::from(name),
                            ImplEntry {
                                path: entry.path,
                                value: val,
                            },
                        ))
                    } else {
                        None
                    }
                })
                .collect()
        }

        fn $single_name(
            db: &dyn WorkshopScriptLoader,
            name: Text,
        ) -> PartialQueryTrisult<ImplEntry<$wscript_impl>> {
            use crate::trisult::IntoTrisult;
            db.$name().remove(&name).trisult_ok_or(
                crate::error::PartialCompilerError::CannotFindNativeDef(name),
            )
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
