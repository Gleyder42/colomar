use super::Text;
use crate::impl_intern_key;
use chumsky::span::SimpleSpan;
use lazy_static::lazy_static;
use std::fmt::Debug;
use std::ops::Range;
use std::path::PathBuf;

pub type OffsetNumber = u32;
pub type Offset = CopyRange;
pub type SpanSource = PathBuf;
pub type SpannedBool = Option<Spanned<()>>;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct CopyRange {
    pub start: OffsetNumber,
    pub end: OffsetNumber,
}

impl CopyRange {
    pub fn start(&self) -> OffsetNumber {
        self.start
    }

    pub fn end(&self) -> OffsetNumber {
        self.end
    }
}

impl From<Range<usize>> for CopyRange {
    fn from(value: Range<usize>) -> Self {
        let error_message: &str = &format!("Line length should not exceed {}", u32::MAX);

        CopyRange {
            start: OffsetNumber::try_from(value.start).expect(error_message),
            end: OffsetNumber::try_from(value.end).expect(error_message),
        }
    }
}

impl From<SimpleSpan> for CopyRange {
    fn from(value: SimpleSpan) -> Self {
        CopyRange {
            start: value.start as OffsetNumber,
            end: value.end as OffsetNumber,
        }
    }
}

impl From<Range<OffsetNumber>> for CopyRange {
    fn from(value: Range<OffsetNumber>) -> Self {
        CopyRange {
            start: value.start,
            end: value.end,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Span {
    pub context: SpanSourceId,
    pub offset: Offset,
}

lazy_static! {
    pub static ref FAKE_SPAN_SOURCE_NAME: PathBuf = PathBuf::from("FakeSpanSource");
}

impl Span {
    pub fn combine(self, other: Span) -> Span {
        assert_eq!(
            self.context, other.context,
            "When combining {:?} with {:?}, both must have the same span but had {:?} and {:?}",
            self.offset, other.offset, self.context, other.context
        );

        Span {
            context: self.context,
            offset: Offset {
                start: self.offset.start,
                end: other.offset.end,
            },
        }
    }

    pub fn fake_span(db: &dyn SpanInterner) -> Span {
        Span {
            context: db.intern_span_source(FAKE_SPAN_SOURCE_NAME.clone()),
            offset: CopyRange { start: 0, end: 1 },
        }
    }
}

impl ariadne::Span for Span {
    type SourceId = SpanSourceId;

    fn source(&self) -> &Self::SourceId {
        &self.context
    }

    fn start(&self) -> usize {
        self.offset.start as usize
    }

    fn end(&self) -> usize {
        self.offset.end as usize
    }
}

impl Span {
    pub fn new(source: SpanSourceId, location: Offset) -> Self {
        Span {
            context: source,
            offset: location,
        }
    }
}

#[salsa::query_group(SpanInternerDatabase)]
pub trait SpanInterner {
    #[salsa::interned]
    fn intern_span_source(&self, span_source: SpanSource) -> SpanSourceId;
}

#[salsa::query_group(StringInternerDatabase)]
pub trait StringInterner {
    #[salsa::interned]
    fn intern_string(&self, string: String) -> StringId;
}

impl StringId {
    pub fn name(&self, interner: &(impl StringInterner + ?Sized)) -> Text {
        interner.lookup_intern_string(*self)
    }
}

impl_intern_key!(StringId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FatSpan {
    pub source: SpanSource,
    pub location: Offset,
}

impl FatSpan {
    pub fn from_span(db: &impl SpanInterner, span: Span) -> FatSpan {
        FatSpan {
            location: span.offset,
            source: db.lookup_intern_span_source(span.context),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Spanned { value, span }
    }

    pub fn ignore_value(option: Option<T>, span: Span) -> SpannedBool {
        option.map(|_| Spanned::new((), span))
    }
}

impl<T: Default> Spanned<T> {
    pub fn default_inner(span: Span) -> Spanned<T> {
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
        self.location.start() as usize
    }

    fn end(&self) -> usize {
        self.location.end() as usize
    }
}

impl chumsky::span::Span for CopyRange {
    type Context = ();
    type Offset = OffsetNumber;

    fn new(_: Self::Context, range: Range<Self::Offset>) -> Self {
        CopyRange {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        ()
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

impl chumsky::span::Span for Span {
    type Context = SpanSourceId;
    type Offset = OffsetNumber;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            context,
            offset: CopyRange::from(range).into(),
        }
    }

    fn context(&self) -> Self::Context {
        self.context
    }

    fn start(&self) -> Self::Offset {
        self.offset.start()
    }

    fn end(&self) -> Self::Offset {
        self.offset.end()
    }
}

impl<T, I: IntoIterator<Item = T>> IntoIterator for Spanned<I> {
    type Item = T;
    type IntoIter = <I as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.value.into_iter()
    }
}

impl_intern_key!(SpanSourceId);
