//! # Span
//! A span is a range that locates a word in an array.
//! For example: `Hello World`.
//! - The span of `Hello` is `0..5`
//! - The span of `World` is `6..11`  
//!
//! Spans are used to display helpful error messages, as this enables the compiler
//! to point to the exact error's location.
//!
//! This module knows two spans.
//! Every span has two components:
//! - Source (alias context)  
//! Points to the source file
//! - Location  
//! The range e.g. `0..5`
//! The difference between the spans is how the components are stored.
//!
//! - [Span]
//! This span has an interned source. That means the full file name was replaced by an id.
//! This makes the span cloneable. It also prevent having to duplicate strings.
//! The span's location may or may not point to correct source code position.
//! - [FatSpan]
//! The FatSpan has the the full file name.
//! The span's location will always point to the correct source code position.
//!
//! The span location may not be accurate because the spans are internally converted into
//! relative spans to better work with the compiler.  
//!
//! # Relative Span
//!
//! A span describes a word as a range.
//! For example `Hello World`. `Hello` could have the range `0..5` `6..11`.
//! If we now append a word at the front `No Hello World` the spans change to `0..2`, `3..8` and `9..14`
//! Now the spans have shifted. In code a word could be an ident. An ident contains the span and value.
//! Colomar's compiler uses a query system, which caches result. Adding one word shift all ranges,
//! and therefore all ident related things have to be redone because the span changed.
//! This is unnecessary because the functionality of the code might not have changed.
//! Take the following rust code. For simplicity lets assume every function has an associated line.
//! ```text
//! 1. fn c() { ... }
//! 2.
//! 3. fn d() { ... }
//! ```
//! If we now add a function `fn a()` all functions have other lines.
//! This would mean the whole code would need to be recompiled although the code didn't really change
//! functionally wise.
//! ```text
//! 1. fn a() { ... }
//! 2.
//! 3. fn c() { ... }
//! 4.
//! 5. fn d() { ... }
//! ```
//!
//! Relative spans reduce this problem by counting from top to bottom and bottom to top.
//! ```text
//! 1. fn c() { ... }
//! 2.
//! -1. fn d() { ... }
//! ```
//! If we now add add a function `fn a()` to the top. The bottom function does not change its line.
//! ```text
//! 1. fn a() { ... }
//! 2.
//! 3. fn c() { ... }
//! -2.
//! -1. fn d() { ... }
//! ```
//! By splitting the file in half, we reduce the chance that all spans are changed.
//! This is not an ideal solution.
//! The ideal span would locate the code by only relying on hierarchical information, like what function
//! belongs to what class and what parameter belongs to what function.
//! The span would be hierarchical based.
//! However, if a better cached hit rate would outweigh the overhead introduced by a hierarchical spans needs to
//! be benchmarked first.
//! For now its enough to prevent span change propagating through the whole file.

use crate::impl_intern_key;
use smol_str::SmolStr;
use std::fmt::Debug;
use std::mem::swap;
use std::ops::Range;

pub type InnerSpan = u32;
pub type SpanLocation = AbstractSpanLocation;
pub type SpanSource = SmolStr;
pub type SpannedBool = Option<Spanned<()>>;
pub type OffsetTable = Vec<InnerSpan>;

#[derive(Debug, Default, Copy, Clone, Hash, Eq, PartialEq)]
struct Anchor(u8);

impl Anchor {
    fn as_usize(&self) -> usize {
        self.0 as usize
    }

    fn with_start(&self) -> Anchor {
        Anchor(self.0 << 1)
    }

    fn with_end(&self) -> Anchor {
        Anchor((self.0 << 1) + 1)
    }

    fn is_end(&self) -> bool {
        self.0 & 0b1 == 1
    }

    fn is_start(&self) -> bool {
        !self.is_end()
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ChunkSpanLocation {
    anchor: Anchor,
    start: InnerSpan,
    end: InnerSpan,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum AbstractSpanLocation {
    Chunk(ChunkSpanLocation),
    Simple(SimpleSpanLocation),
}

impl From<SimpleSpanLocation> for AbstractSpanLocation {
    fn from(value: SimpleSpanLocation) -> Self {
        AbstractSpanLocation::Simple(value)
    }
}

impl AbstractSpanLocation {
    fn unwrap_chunked(self) -> ChunkSpanLocation {
        match self {
            AbstractSpanLocation::Simple(simple) => panic!("{:?} is simple", simple),
            AbstractSpanLocation::Chunk(chunk) => chunk,
        }
    }

    pub fn start(&self) -> InnerSpan {
        match self {
            AbstractSpanLocation::Chunk(span) => span.start,
            AbstractSpanLocation::Simple(span) => span.start,
        }
    }

    pub fn end(&self) -> InnerSpan {
        match self {
            AbstractSpanLocation::Chunk(span) => span.end,
            AbstractSpanLocation::Simple(span) => span.end,
        }
    }
}

fn decode_in_place(span: &mut AbstractSpanLocation, offset_table: &[InnerSpan]) {
    match span {
        AbstractSpanLocation::Chunk(chunk) => {
            let mut location = AbstractSpanLocation::Simple(decode(*chunk, offset_table));
            swap(span, &mut location);
        }
        AbstractSpanLocation::Simple(simple) => {
            eprintln!("Tried to decode {simple:?} but was already simple")
        }
    };
}

fn decode(span: ChunkSpanLocation, table: &[InnerSpan]) -> SimpleSpanLocation {
    let offset = table[span.anchor.as_usize()];
    if span.anchor.is_start() {
        SimpleSpanLocation {
            start: span.start + offset,
            end: span.end + offset,
        }
    } else {
        SimpleSpanLocation {
            start: offset - span.start,
            end: offset - span.end,
        }
    }
}

fn encode(
    level: u16,
    mut spans: Vec<AbstractSpanLocation>,
) -> (Vec<ChunkSpanLocation>, Vec<InnerSpan>) {
    let mut references: Vec<_> = spans.iter_mut().collect();

    let table = encode_in_place(level, &mut references);

    let chunked_spans = spans.into_iter().map(|it| it.unwrap_chunked()).collect();
    (chunked_spans, table)
}

pub fn encode_in_place(
    level: u16,
    mut references: &mut [&mut AbstractSpanLocation],
) -> Vec<InnerSpan> {
    let mut table = vec![0; 2_i32.pow(level as u32) as usize];
    let offset = references.last().unwrap().end();
    inner_encode(level, offset, Anchor::default(), references, &mut table);
    table
}

fn inner_encode(
    level: u16,
    offset: InnerSpan,
    anchor: Anchor,
    spans: &mut [&mut AbstractSpanLocation],
    table: &mut [InnerSpan],
) {
    if spans.is_empty() {
        return;
    }

    if level == 0 {
        for span in spans {
            let start = (offset as i64 - span.start() as i64).abs() as InnerSpan;
            let end = (offset as i64 - span.end() as i64).abs() as InnerSpan;

            let mut chunked = AbstractSpanLocation::Chunk(ChunkSpanLocation { anchor, start, end });
            table[anchor.as_usize()] = offset;

            swap(*span, &mut chunked);
        }
    } else {
        let new_level = level - 1;
        let mid = (spans.len() as f64 / 2_f64).round() as usize;
        let end = spans.len();
        let start_offset = spans[0].start();
        let start_anchor = anchor.with_start();

        let end_anchor = anchor.with_end();
        let end_offset = spans[spans.len() - 1].end();

        inner_encode(
            new_level,
            start_offset,
            start_anchor,
            &mut spans[0..mid],
            table,
        );
        inner_encode(
            new_level,
            end_offset,
            end_anchor,
            &mut spans[mid..end],
            table,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_iterator;

    /// Generates a vec of numeric spans.  
    ///
    /// Generates a vec of [AbstractSpanLocation::Simple].
    /// Every span has a start and end value.
    /// The range parameter describes the range of a span (end - start = range)
    /// The len parameter describes how many spans should be generated.
    fn gen_spans(len: usize, range: InnerSpan) -> Vec<AbstractSpanLocation> {
        let mut current = 0;

        (0..len)
            .map(|_| {
                let start = current;
                current += range;
                let end = current;
                AbstractSpanLocation::Simple(SimpleSpanLocation { start, end })
            })
            .collect()
    }

    #[test]
    fn test_spans_with_equal_range() {
        let original_spans = gen_spans(5, 10);

        let (encode_spans, table) = encode(1, original_spans.clone());

        let decoded_spans: Vec<_> = encode_spans
            .into_iter()
            .map(|it| AbstractSpanLocation::Simple(decode(it, &table)))
            .collect();
        assert_iterator!(original_spans, decoded_spans);
    }

    #[test]
    fn test_spans_with_unequal_range() {
        let original_spans = vec![
            AbstractSpanLocation::Simple(SimpleSpanLocation { start: 0, end: 15 }),
            AbstractSpanLocation::Simple(SimpleSpanLocation { start: 15, end: 50 }),
            AbstractSpanLocation::Simple(SimpleSpanLocation { start: 60, end: 77 }),
            AbstractSpanLocation::Simple(SimpleSpanLocation {
                start: 85,
                end: 100,
            }),
        ];

        let (encode_spans, table) = encode(1, original_spans.clone());

        let decoded_spans: Vec<_> = encode_spans
            .into_iter()
            .map(|it| AbstractSpanLocation::Simple(decode(it, &table)))
            .collect();
        assert_iterator!(original_spans, decoded_spans);
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct SimpleSpanLocation {
    pub start: InnerSpan,
    pub end: InnerSpan,
}

impl From<Range<usize>> for SimpleSpanLocation {
    fn from(value: Range<usize>) -> Self {
        let error_message: &str = &format!("Line length should not exceed {}", u32::MAX);

        SimpleSpanLocation {
            start: InnerSpan::try_from(value.start).expect(error_message),
            end: InnerSpan::try_from(value.end).expect(error_message),
        }
    }
}

impl From<Range<InnerSpan>> for SimpleSpanLocation {
    fn from(value: Range<InnerSpan>) -> Self {
        SimpleSpanLocation {
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

#[salsa::query_group(SpanInternerDatabase)]
pub trait SpanInterner {
    #[salsa::interned]
    fn intern_span_source(&self, span_source: SpanSource) -> SpanSourceId;
}

impl Span {
    pub fn new(source: SpanSourceId, location: impl Into<SpanLocation>) -> Self {
        Span {
            source,
            location: location.into(),
        }
    }
}

impl FatSpan {
    pub fn from_span(
        db: &(impl SpanInterner + ?Sized),
        offset_table: &[InnerSpan],
        mut span: Span,
    ) -> FatSpan {
        FatSpan {
            location: {
                decode_in_place(&mut span.location, offset_table);
                span.location
            },
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

impl chumsky::Span for Span {
    type Context = SpanSourceId;
    type Offset = InnerSpan;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            source: context,
            location: SimpleSpanLocation::from(range).into(),
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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct SpanSourceId(salsa::InternId);

impl_intern_key!(SpanSourceId);
