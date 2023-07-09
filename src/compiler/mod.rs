use crate::compiler::offset::HierOffset;
use crate::compiler::trisult::Trisult;
use crate::impl_intern_key;
use error::CompilerError;
use rustc_hash::FxHashMap;
use smol_str::SmolStr;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, HashMap};
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
pub struct PosOffset {
    pub start: InnerSpan,
    pub end: InnerSpan,
}

impl From<Range<InnerSpan>> for PosOffset {
    fn from(value: Range<InnerSpan>) -> Self {
        PosOffset {
            start: value.start,
            end: value.end,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct HierSpan {
    pub source: SpanSourceId,
    pub offset: HierOffsetId,
}

impl HierSpan {
    fn from_pos_span(
        offset: HierOffset,
        interner: &dyn SpanInterner,
        pos_span: PosSpan,
    ) -> HierSpan {
        let offset_id = interner.intern_hier_offset(offset);
        let hier_span = HierSpan {
            source: pos_span.source,
            offset: offset_id,
        };
        interner.span_table().insert(hier_span, pos_span);
        hier_span
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct HierOffsetId(salsa::InternId);

impl_intern_key!(HierOffsetId);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct PosSpan {
    pub source: SpanSourceId,
    pub offset: PosOffset,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FatSpan {
    pub source: SpanSource,
    pub offset: PosOffset,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: HierSpan,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    // TODO Rename this field to text
    pub value: Text,
    pub span: HierSpan,
}

#[salsa::query_group(SpanInternerDatabase)]
pub trait SpanInterner: SpanInternerContext {
    #[salsa::interned]
    fn intern_span_source(&self, span_source: SpanSource) -> SpanSourceId;

    #[salsa::interned]
    fn intern_hier_offset(&self, offset: HierOffset) -> HierOffsetId;

    fn combine_spans(&self, parent: HierSpan, child: HierSpan) -> HierSpan;
}

fn combine_spans(db: &dyn SpanInterner, parent: HierSpan, child: HierSpan) -> HierSpan {
    let parent_offset: HierOffset = db.lookup_intern_hier_offset(parent.offset);
    let mut child_offset: HierOffset = db.lookup_intern_hier_offset(child.offset);

    let combined_offset = child_offset.append_parent(parent_offset);
    let combined_offset_id = db.intern_hier_offset(combined_offset);
    HierSpan {
        source: parent.source,
        offset: combined_offset_id,
    }
}

pub trait SpanInternerContext {
    fn span_table(&self) -> &SpanTable;

    fn as_hier_span(&self, offset: HierOffset, span: PosSpan) -> HierSpan;
}

#[derive(Default)]
pub struct SpanTable(RefCell<FxHashMap<HierSpan, PosSpan>>);

impl SpanTable {
    pub fn insert(&self, key: HierSpan, value: PosSpan) {
        let mut ref_mut = self.0.borrow_mut();
        ref_mut.insert(key, value);
    }

    pub fn get(&self, key: &HierSpan) -> Option<PosSpan> {
        let ref_mut = self.0.borrow();
        ref_mut.get(&key).copied()
    }
}

impl PosSpan {
    fn new(source: SpanSourceId, span: PosOffset) -> Self {
        PosSpan {
            source,
            offset: span,
        }
    }
}

impl FatSpan {
    pub fn from_span(db: &(impl SpanInterner + ?Sized), span: PosSpan) -> FatSpan {
        FatSpan {
            offset: span.offset,
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
        self.offset.start
    }

    fn end(&self) -> usize {
        self.offset.end
    }
}

impl chumsky::Span for PosSpan {
    type Context = SpanSourceId;
    type Offset = InnerSpan;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            source: context,
            offset: range.into(),
        }
    }

    fn context(&self) -> Self::Context {
        self.source
    }

    fn start(&self) -> Self::Offset {
        self.offset.start
    }

    fn end(&self) -> Self::Offset {
        self.offset.end
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
