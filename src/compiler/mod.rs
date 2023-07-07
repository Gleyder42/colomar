use crate::compiler::trisult::Trisult;
use crate::impl_intern_key;
use error::CompilerError;
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
pub mod offset;
pub mod printer;
pub mod trisult;
pub mod wir;
pub mod workshop;
pub mod wst;

pub type InnerSpan = usize;
pub type SpanLocation = CheapRange;
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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct PosSpan {
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
    pub span: PosSpan,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    // TODO Rename this field to text
    pub value: Text,
    pub span: PosSpan,
}

#[salsa::query_group(SpanInternerDatabase)]
pub trait SpanInterner {
    #[salsa::interned]
    fn intern_span_source(&self, span_source: SpanSource) -> SpanSourceId;
}

impl PosSpan {
    fn new(source: SpanSourceId, span: SpanLocation) -> Self {
        PosSpan {
            source,
            location: span,
        }
    }
}

impl FatSpan {
    pub fn from_span(db: &(impl SpanInterner + ?Sized), span: PosSpan) -> FatSpan {
        FatSpan {
            location: span.location,
            source: db.lookup_intern_span_source(span.source),
        }
    }
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: PosSpan) -> Self {
        Spanned { value, span }
    }

    pub fn ignore_value(option: Option<T>, span: PosSpan) -> SpannedBool {
        option.map(|_| Spanned::new((), span))
    }
}

impl<T: Default> Spanned<T> {
    pub fn default_inner(span: PosSpan) -> Spanned<T> {
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

impl chumsky::Span for PosSpan {
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
