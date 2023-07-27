use colomar_macros::NewSpanLink;
use compiler::span::{HierSpan, Sid, SpanKey, SpanKeyLinker};
use compiler::Text;
use smallvec::{Array, SmallVec};
use std::fmt::{Display, Formatter, Write};

struct Spanned<T> {
    value: T,
    span: HierSpan,
}

impl<T> Spanned<T> {
    pub fn new(root: impl Into<Text>, value: impl Into<T>) -> Self {
        let value = value.into();
        let span = SpanKey::new(Sid::new(root.into())).into();

        Spanned { value, span }
    }
}

impl<T> SpanKeyLinker for Spanned<T> {
    fn link_self(&self, span_name: HierSpan) {}

    fn span_name(&self) -> Option<HierSpan> {
        Some(self.span.clone())
    }
}

#[derive(Clone)]
struct Ident {
    value: Sid,
    span: HierSpan,
}

impl Ident {
    pub fn from_str(name: &str) -> Ident {
        Ident::new(Text::new(name))
    }

    pub fn new(name: impl Into<Sid>) -> Ident {
        let name = name.into();
        let span = SpanKey::new(name.clone()).into();

        Ident { value: name, span }
    }
}

impl SpanKeyLinker for Ident {
    fn link_self(&self, span_name: HierSpan) {}

    fn span_name(&self) -> Option<HierSpan> {
        Some(self.span.clone())
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

#[derive(NewSpanLink)]
struct Person {
    name: Ident,
    second_name: Option<Ident>,
    amount: Spanned<i32>,
    answer: Spanned<Answer>,
    span: HierSpan,
}

#[derive(NewSpanLink)]
struct Persons {
    #[no_span]
    values: Vec<Person>,
    span: HierSpan,
}

#[derive(NewSpanLink)]
struct SmallerPersons {
    #[no_span]
    values: SmallVec<[Person; 2]>,
    span: HierSpan,
}

#[derive(NewSpanLink)]
struct CompilerPart {
    name: Ident,
    persons: Persons,
    smaller_persons: SmallerPersons,
    span: HierSpan,
}

#[test]
fn test() {
    let person = Person::new(
        Ident::from_str("First Name"),
        None,
        Spanned::new("amount", 10),
        Spanned::new("answer", Answer::Yes),
    );

    let part = CompilerPart::new(
        Ident::from_str("part"),
        Persons::new(vec![person]),
        SmallerPersons::new(SmallVec::new()),
    );

    let hier_span = SpanKey::new(Text::from("root")).into();

    part.link_to(hier_span);

    println!("{}", part.persons.span.borrow());
}
