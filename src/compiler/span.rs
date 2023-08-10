//! # Relative Span
//!
//! A span describes a word as a range.
//! For example `Hello World`. `Hello` could have the range `0..5` `6..11`.
//! If we now append a word at the front `No Hello World` the spans change to `0..2`, `3..8` and `9..14`
//! Know the spans have shifted. In code a word could be an ident. An ident contains the span and value.
//! Colomar uses a query system of the compiler, which caches result. Adding one word shift all ranges,
//! and therefore all ident related things have to be recalculated because the span changed.
//! This is unnecessary because the meaning of the code might not have changed.
//! Take the following rust code. For simplicity lets assume every function has an associated line.
//! ```
//! 1. fn c() { ... }
//! 2.
//! 3. fn d() { ... }
//! ```
//! If we now add a function `fn a()` all functions have other lines.
//! This would mean the whole code would need to be recompiled although the code didn't really change
//! functionally wise.
//! ```
//! 1. fn a() { ... }
//! 2.
//! 3. fn c() { ... }
//! 4.
//! 5. fn d() { ... }
//! ```
//!
//! Relative spans reduce this problem by counting from top to bottom and bottom to top.
//! ```
//! 1. fn c() { ... }
//! 2.
//! -1. fn d() { ... }
//! ```
//! If we now add add a function `fn a()` to the top. The bottom function does not change its line.
//! ```
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

use std::fmt::Debug;
use std::mem::swap;

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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct ChunkSpan {
    anchor: Anchor,
    start: u32,
    end: u32,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
struct SimpleSpan {
    start: u32,
    end: u32,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum AbstractSpan {
    Chunk(ChunkSpan),
    Simple(SimpleSpan),
}

impl AbstractSpan {
    fn unwrap_chunked(self) -> ChunkSpan {
        match self {
            AbstractSpan::Simple(simple) => panic!("{:?} is simple", simple),
            AbstractSpan::Chunk(chunk) => chunk,
        }
    }

    fn start(&self) -> u32 {
        match self {
            AbstractSpan::Chunk(span) => span.start,
            AbstractSpan::Simple(span) => span.start,
        }
    }

    fn end(&self) -> u32 {
        match self {
            AbstractSpan::Chunk(span) => span.end,
            AbstractSpan::Simple(span) => span.end,
        }
    }
}

fn decode(span: ChunkSpan, table: &[u32]) -> SimpleSpan {
    let offset = table[span.anchor.as_usize()];
    if span.anchor.is_start() {
        SimpleSpan {
            start: span.start + offset,
            end: span.end + offset,
        }
    } else {
        SimpleSpan {
            start: offset - span.start,
            end: offset - span.end,
        }
    }
}

fn priv_encode(level: u16, mut spans: Vec<AbstractSpan>) -> (Vec<ChunkSpan>, Vec<u32>) {
    let mut table = vec![0; 2_i32.pow(level as u32) as usize];
    let offset = spans.last().unwrap().end();
    inner_encode(level, offset, Anchor::default(), &mut spans, &mut table);
    let chunked_spans = spans.into_iter().map(|it| it.unwrap_chunked()).collect();
    (chunked_spans, table)
}

fn inner_encode(
    level: u16,
    offset: u32,
    anchor: Anchor,
    spans: &mut [AbstractSpan],
    table: &mut [u32],
) {
    if spans.is_empty() {
        return;
    }

    if level == 0 {
        for span in spans {
            let start = (offset as i64 - span.start() as i64).abs() as u32;
            let end = (offset as i64 - span.end() as i64).abs() as u32;

            let mut chunked = AbstractSpan::Chunk(ChunkSpan { anchor, start, end });
            table[anchor.as_usize()] = offset;

            swap(span, &mut chunked);
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
    /// Generates a vec of [AbstractSpan::Simple].
    /// Every span has a start and end value.
    /// The range parameter describes the range of a span (end - start = range)
    /// The len parameter describes how many spans should be generated.
    fn gen_spans(len: usize, range: u32) -> Vec<AbstractSpan> {
        let mut current = 0;

        (0..len)
            .into_iter()
            .map(|_| {
                let start = current;
                current += range;
                let end = current;
                AbstractSpan::Simple(SimpleSpan { start, end })
            })
            .collect()
    }

    #[test]
    fn test_spans_with_equal_range() {
        let original_spans = gen_spans(5, 10);

        let (encode_spans, table) = priv_encode(1, original_spans.clone());

        let decoded_spans: Vec<_> = encode_spans
            .into_iter()
            .map(|it| AbstractSpan::Simple(decode(it, &table)))
            .collect();
        assert_iterator!(original_spans, decoded_spans);
    }

    #[test]
    fn test_spans_with_unequal_range() {
        let original_spans = vec![
            AbstractSpan::Simple(SimpleSpan { start: 0, end: 15 }),
            AbstractSpan::Simple(SimpleSpan { start: 15, end: 50 }),
            AbstractSpan::Simple(SimpleSpan { start: 60, end: 77 }),
            AbstractSpan::Simple(SimpleSpan {
                start: 85,
                end: 100,
            }),
        ];

        let (encode_spans, table) = priv_encode(1, original_spans.clone());

        let decoded_spans: Vec<_> = encode_spans
            .into_iter()
            .map(|it| AbstractSpan::Simple(decode(it, &table)))
            .collect();
        assert_iterator!(original_spans, decoded_spans);
    }
}
