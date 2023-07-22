use colomar_macros::SpanLink;
use compiler::span::{HierSpan, Sid, SpanKey};
use smallvec::SmallVec;
use std::fmt::{Display, Formatter, Write};

struct Spanned<T: ToString> {
    value: T,
    span: HierSpan,
}

impl<T: ToString> Spanned<T> {
    pub fn new(value: impl Into<T>) -> Self {
        let value = value.into();
        let span = SpanKey::new(Sid::new(value.to_string().into())).into();

        Spanned { value, span }
    }
}

struct Ident {
    name: Sid,
    span: HierSpan,
}

impl Ident {
    pub fn new(name: impl Into<Sid>) -> Ident {
        let name = name.into();
        let span = SpanKey::new(name.clone()).into();

        Ident { name, span }
    }
}

enum Answer {
    Yes,
    No,
}

impl Display for Answer {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Answer::Yes => f.write_str("yes"),
            Answer::No => f.write_str("no"),
        }
    }
}

#[derive(SpanLink)]
struct Person {
    name: Ident,
    second_name: Option<Ident>,
    amount: i32,
    answer: Spanned<Answer>,
    span: HierSpan,
}

#[derive(SpanLink)]
struct CompilerPart {
    name: Ident,
    persons: Vec<Person>,
    smaller_persons: SmallVec<[Person; 2]>,
    span: HierSpan,
}

#[test]
fn test() {}
