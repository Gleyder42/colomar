use crate::compiler::span::{HierarchicalLocation, Segment, Spacer};
use crate::compiler::trisult::Trisult;
use crate::impl_intern_key;
use error::CompilerError;
use hash_hasher::HashedMap;
use smol_str::SmolStr;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::ops::Range;

pub mod analysis;
pub mod cir;
// pub mod codegen;
pub mod codegen;
pub mod cst;
pub mod database;
pub mod error;
pub mod language;
pub mod loader;
pub mod printer;
pub mod span;
pub mod trisult;
pub mod wir;
pub mod workshop;
pub mod wst;

pub type InnerSpan = usize;
pub type SpanLocation = CheapRange; // Rename to numeric location
pub type SpanSource = SmolStr;
pub type Text = SmolStr;
pub type HashableMap<K, V> = BTreeMap<K, V>;
pub type SpannedBool = Option<Spanned<()>>;
pub type QueryTrisult<T> = Trisult<T, CompilerError>;

pub const CONDITIONS_LEN: usize = 6;
pub const ACTIONS_LEN: usize = 8;
pub const DECLARED_ARGUMENTS_LEN: usize = 4;
// TODO Give this a more generic name
pub const PROPERTY_DECLS_LEN: usize = 4;
pub const FUNCTIONS_DECLS_LEN: usize = 6;
pub const ENUM_CONSTANTS_LEN: usize = 8;

pub const CALLED_ARGUMENTS_LEN: usize = DECLARED_ARGUMENTS_LEN;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct CheapRange {
    pub start: InnerSpan,
    pub end: InnerSpan,
}

impl From<Range<InnerSpan>> for CheapRange {
    fn from(value: Range<InnerSpan>) -> Self {
        CheapRange {
            start: value.start,
            end: value.end,
        }
    }
}

impl Span {
    fn new(source: SpanSourceId, span: SpanLocation) -> Self {
        Span {
            source,
            location: span,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct HierarchicalSpan {
    pub source: SpanSourceId,
    pub location: HierarchicalLocationId,
}

impl HierarchicalSpan {
    pub fn overarching_from(
        span: Span,
        interner: &dyn SpanInterner,
        mut parent: HierarchicalLocation,
        segment: Segment,
    ) -> Self {
        parent.push(segment);

        Self::intern_span(interner, span.source, parent)
    }

    pub fn new(
        interner: &dyn SpanInterner,
        span_source_id: SpanSourceId,
        mut parent: HierarchicalLocation,
        spacer: Spacer,
        segment: Segment,
    ) -> Self {
        parent.push_spacer(spacer);
        parent.push(segment);

        Self::intern_span(interner, span_source_id, parent)
    }

    pub fn from_location(
        interner: &dyn SpanInterner,
        span: Span,
        parent: HierarchicalLocation,
    ) -> HierarchicalSpan {
        let hierarchical_id = interner.intern_hierarchical_location(parent);

        HierarchicalSpan {
            source: span.source,
            location: hierarchical_id,
        }
    }

    fn intern_span(
        interner: &dyn SpanInterner,
        span_source_id: SpanSourceId,
        current: HierarchicalLocation,
    ) -> HierarchicalSpan {
        let hierarchical_id = interner.intern_hierarchical_location(current);

        HierarchicalSpan {
            source: span_source_id,
            location: hierarchical_id,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Span {
    pub source: SpanSourceId,
    pub location: SpanLocation,
}

impl chumsky::Span for Span {
    type Context = SpanSourceId;
    type Offset = InnerSpan;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            source: context,
            location: range.into(),
        }
    }

    fn context(&self) -> Self::Context {
        self.source
    }

    fn start(&self) -> Self::Offset {
        self.location.start
    }

    fn end(&self) -> Self::Offset {
        self.location.end
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct HierarchicalLocationId(salsa::InternId);

impl_intern_key!(HierarchicalLocationId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FatSpan {
    pub source: SpanSource,
    pub location: SpanLocation,
}

impl FatSpan {
    pub fn from_span(
        db: &(impl SpanInterner + ?Sized),
        span_map: &mut HashedMap<u64, SpanLocation>,
        span: Span,
    ) -> FatSpan {
        FatSpan {
            source: db.lookup_intern_span_source(span.source),
            location: *span_map
                .get(
                    &db.lookup_intern_abstract_location(span.location)
                        .hash_self(),
                )
                .unwrap(),
        }
    }
}

impl ariadne::Span for FatSpan {
    type SourceId = SpanSource;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.location.start
    }

    fn end(&self) -> usize {
        self.location.end
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    // TODO Rename this field to text
    pub value: Text,
    pub span: HierarchicalSpan,
}

#[salsa::query_group(SpanInternerDatabase)]
pub trait SpanInterner {
    #[salsa::interned]
    fn intern_span_source(&self, span_source: SpanSource) -> SpanSourceId;

    #[salsa::interned]
    fn intern_hierarchical_location(
        &self,
        hierarchical: HierarchicalLocation,
    ) -> HierarchicalLocationId;
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: HierarchicalSpan,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: HierarchicalSpan) -> Self {
        Spanned { value, span }
    }

    pub fn ignore_value(option: Option<T>, span: HierarchicalSpan) -> SpannedBool {
        option.map(|_| Spanned::new((), span))
    }
}

impl<T: Default> Spanned<T> {
    pub fn default_inner(span: HierarchicalSpan) -> Spanned<T> {
        Spanned {
            value: T::default(),
            span,
        }
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

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum UseRestriction {
    /// Can not be assigned directly, but can be accessed.
    GetVar,
    /// Can be assigned, but not be accessed
    SetVar,
    /// Can only be assigned once and be accessed
    Val,
    /// Can be assigned and accessed
    Var,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Op {
    Equals,
    NotEquals,
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Equals => write!(f, "=="),
            Op::NotEquals => write!(f, "!="),
        }
    }
}
