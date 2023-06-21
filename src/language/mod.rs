use crate::impl_intern_key;
use crate::language::analysis::interner::Interner;
use crate::language::analysis::namespace::Namespace;
use crate::language::ast::SpannedBool;
use smol_str::SmolStr;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ops::Range;

pub mod analysis;
pub mod ast;
pub mod codegen;
pub mod error;
pub mod im;
pub mod lexer;
pub mod lim;
pub mod parser;

pub type InnerSpan = usize;
pub type SpanLocation = Range<InnerSpan>;
pub type SpanSource = SmolStr;

// TODO Rename this type
// Maybe Text?
pub type ImmutableString = SmolStr;

pub const CONDITIONS_LEN: usize = 6;
pub const ACTIONS_LEN: usize = 8;
pub const DECLARED_ARGUMENTS_LEN: usize = 4;
// Give this a more generic name
pub const PROPERTY_DECLS_LEN: usize = 4;
pub const FUNCTIONS_DECLS_LEN: usize = 6;
pub const ENUM_CONSTANTS_LEN: usize = 8;

pub const CALLED_ARGUMENTS_LEN: usize = DECLARED_ARGUMENTS_LEN;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Span {
    pub source: SpanSourceId,
    pub location: SpanLocation,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FatSpan {
    pub source: SpanSource,
    pub location: SpanLocation,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    pub value: ImmutableString,
    pub span: Span,
}

impl Span {
    fn new(source: SpanSourceId, span: SpanLocation) -> Self {
        Span {
            source,
            location: span,
        }
    }
}

impl FatSpan {
    pub fn from_span(db: &(impl Interner + ?Sized), span: Span) -> FatSpan {
        FatSpan {
            location: span.location,
            source: db.lookup_intern_span_source(span.source),
        }
    }
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Spanned { value, span }
    }

    pub fn ignore_value(option: Option<T>, span: Span) -> SpannedBool {
        option.map(|_| Spanned::new((), span))
    }
}

impl<T> Spanned<T> {
    pub fn inner_into<U: From<T>>(self) -> Spanned<U> {
        Spanned {
            value: self.value.into(),
            span: self.span,
        }
    }
}

impl ariadne::Span for FatSpan {
    type SourceId = SpanSource;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.location.start.clone()
    }

    fn end(&self) -> usize {
        self.location.end.clone()
    }
}

impl chumsky::Span for Span {
    type Context = SpanSourceId;
    type Offset = InnerSpan;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            source: context,
            location: range,
        }
    }

    fn context(&self) -> Self::Context {
        self.source.clone()
    }

    fn start(&self) -> Self::Offset {
        self.location.start.clone()
    }

    fn end(&self) -> Self::Offset {
        self.location.end.clone()
    }
}

impl<T, I: IntoIterator<Item = T>> IntoIterator for Spanned<I> {
    type Item = T;
    type IntoIter = <I as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.value.into_iter()
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct SpanSourceId(salsa::InternId);

impl_intern_key!(SpanSourceId);

// TODO Maybe use a proper linked hash map
#[derive(Clone, Debug, Eq)]
pub struct HashableHashMap<K, V>(pub HashMap<K, V>)
where
    K: Hash + PartialEq + Eq + Ord,
    V: Hash + PartialEq;

impl<K, V> HashableHashMap<K, V>
where
    K: Hash + PartialEq + Eq + Ord,
    V: Hash + PartialEq,
{
    pub fn new() -> Self {
        HashableHashMap(HashMap::new())
    }

    fn sorted_map_entries(&self) -> Vec<(&K, &V)> {
        let mut content = self.0.iter().collect::<Vec<_>>();
        content.sort_by_key(|it| it.0);
        content
    }
}

impl<K, V> PartialEq for HashableHashMap<K, V>
where
    K: Hash + PartialEq + Eq + Ord,
    V: Hash + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.sorted_map_entries() == other.sorted_map_entries()
    }
}

impl<K, V> Hash for HashableHashMap<K, V>
where
    K: Hash + PartialEq + Eq + Ord,
    V: Hash + PartialEq,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.sorted_map_entries().hash(state)
    }
}
