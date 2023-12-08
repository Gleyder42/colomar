use crate::compiler::Text2;
use crate::impl_intern_key;
use chumsky::span::SimpleSpan;
use smol_str::SmolStr;
use std::fmt::Debug;
use std::ops::Range;

pub type InnerSpan = u32;
pub type SpanLocation = CopyRange;
pub type SpanSource = SmolStr;
pub type SpannedBool = Option<Spanned<()>>;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct CopyRange {
    pub start: InnerSpan,
    pub end: InnerSpan,
}

impl CopyRange {
    pub fn start(&self) -> InnerSpan {
        self.start
    }

    pub fn end(&self) -> InnerSpan {
        self.end
    }
}

impl From<Range<usize>> for CopyRange {
    fn from(value: Range<usize>) -> Self {
        let error_message: &str = &format!("Line length should not exceed {}", u32::MAX);

        CopyRange {
            start: InnerSpan::try_from(value.start).expect(error_message),
            end: InnerSpan::try_from(value.end).expect(error_message),
        }
    }
}

impl From<SimpleSpan> for CopyRange {
    fn from(value: SimpleSpan) -> Self {
        CopyRange {
            start: value.start as InnerSpan,
            end: value.end as InnerSpan,
        }
    }
}

impl From<Range<InnerSpan>> for CopyRange {
    fn from(value: Range<InnerSpan>) -> Self {
        CopyRange {
            start: value.start,
            end: value.end,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Span {
    pub source: SpanSourceId,
    pub location: SpanLocation,
}

impl Span {
    pub fn new(source: SpanSourceId, location: SpanLocation) -> Self {
        Span { source, location }
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
    pub fn name(&self, interner: &(impl StringInterner + ?Sized)) -> Text2 {
        interner.lookup_intern_string(*self)
    }
}

impl_intern_key!(StringId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FatSpan {
    pub source: SpanSource,
    pub location: SpanLocation,
}

impl FatSpan {
    pub fn from_span(db: &(impl SpanInterner + ?Sized), span: Span) -> FatSpan {
        FatSpan {
            location: span.location,
            source: db.lookup_intern_span_source(span.source),
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
    type Offset = InnerSpan;

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
    type Offset = InnerSpan;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            source: context,
            location: CopyRange::from(range).into(),
        }
    }

    fn context(&self) -> Self::Context {
        self.source
    }

    fn start(&self) -> Self::Offset {
        self.location.start()
    }

    fn end(&self) -> Self::Offset {
        self.location.end()
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
