use std::fmt::{Debug, Display};
use std::mem::swap;
use std::ops::Range;

#[derive(Debug, Default, Copy, Clone, Hash, Eq, PartialEq)]
struct Anchor(u16);

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

#[derive(Debug, Default, Copy, Clone)]
struct SimpleSpan {
    start: u32,
    end: u32,
}

#[derive(Debug, Copy, Clone)]
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

fn encode(level: u16, mut spans: Vec<AbstractSpan>) -> (Vec<ChunkSpan>, Vec<u32>) {
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
    use crate::compiler::language::lexer::{lexer, Token};
    use crate::compiler::language::parser::parser;
    use crate::compiler::{Span, SpanSourceId};
    use chumsky::Stream;
    use hashlink::LinkedHashMap;
    use std::convert::identity;
    use std::fs;
    use std::path::Path;
    use std::time::{Duration, Instant};

    fn gen_spans(len: usize, size: u32) -> Vec<AbstractSpan> {
        let mut current = 0;

        (0..len)
            .into_iter()
            .map(|_| {
                let start = current;
                current += size;
                let end = current;
                AbstractSpan::Simple(SimpleSpan { start, end })
            })
            .collect()
    }

    #[test]
    fn test() {
        let spans = gen_spans(5, 10);
        println!("{:#?}", spans);

        let (vec, mut table) = encode(1, spans);

        println!("{:#?}", vec);
        println!("{:#?}", table);

        let x: Vec<_> = vec.into_iter().map(|it| decode(it, &mut table)).collect();
        println!("{:#?}", x);
    }

    #[test]
    fn test2() {
        let spans = vec![
            AbstractSpan::Simple(SimpleSpan { start: 0, end: 15 }),
            AbstractSpan::Simple(SimpleSpan { start: 15, end: 50 }),
            AbstractSpan::Simple(SimpleSpan { start: 60, end: 77 }),
            AbstractSpan::Simple(SimpleSpan {
                start: 85,
                end: 100,
            }),
        ];

        let (vec, table) = encode(1, spans);

        println!("{:#?}", vec);
        println!("{:#?}", table);

        let x: Vec<_> = vec.into_iter().map(|it| decode(it, &table)).collect();
        println!("{:#?}", x);
    }

    fn read_file<P: AsRef<Path>>(size: i32, path: P) -> (Vec<(ChunkSpan, Token)>, Statistics) {
        use chumsky::Parser;

        let content = fs::read_to_string(path).unwrap();
        let tokens = lexer(SpanSourceId(0_usize.into()))
            .parse(content.as_str())
            .unwrap();
        let spans: Vec<_> = tokens
            .iter()
            .map(|(_, span)| span)
            .cloned()
            .map(|it| {
                AbstractSpan::Simple(SimpleSpan {
                    start: it.location.start as u32,
                    end: it.location.end as u32,
                })
            })
            .collect();
        let tokens: Vec<_> = tokens.into_iter().map(|(token, span)| token).collect();
        let len = tokens.len() as f64;

        let level = (len / size as f64).sqrt().round() as u16;

        let encode_time = Instant::now();
        let (chunks, table) = encode(level, spans);
        let encode_time = encode_time.elapsed();

        let x = chunks.into_iter().zip(tokens.into_iter()).collect();
        (
            x,
            Statistics {
                len: len as usize,
                level,
                encode_time,
            },
        )
    }

    fn write<P: AsRef<Path>>(size: i32, in_path: P, out_path: P) {
        let (a, statistics) = read_file(size, in_path);
        fs::write(out_path, format!("{statistics:?} \n{:#?}", a)).unwrap();
    }

    #[derive(Debug, Copy, Clone)]
    struct Statistics {
        len: usize,
        level: u16,
        encode_time: Duration,
    }

    #[test]
    fn test3() {
        let size = 10;

        write(
            size,
            Path::new("res/test/span/main.co"),
            Path::new("res/test/span/out/a.txt"),
        );
        write(
            size,
            Path::new("res/test/span/main2.co"),
            Path::new("res/test/span/out/b.txt"),
        );
    }
}
