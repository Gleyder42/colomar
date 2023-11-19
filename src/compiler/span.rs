use crate::compiler::{Text, Text2};
use crate::impl_intern_key;
use chumsky::span::SimpleSpan;
use smol_str::SmolStr;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;
use std::rc::Rc;

pub type InnerSpan = u32;
pub type SpanLocation = CopyRange;
pub type SpanSource = SmolStr;
pub type SpannedBool = Option<Spanned<()>>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SpanName {
    Pointer(Text),
    Literal(&'static str),
}

impl SpanName {
    fn as_str(&self) -> &str {
        match self {
            SpanName::Pointer(name) => todo!(),
            SpanName::Literal(literal) => literal,
        }
    }
}

impl Display for SpanName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SpanName::Pointer(name) => write!(f, "{name:?}"),
            SpanName::Literal(literal) => write!(f, "{literal}"),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct SpanNodeId(salsa::InternId);

impl_intern_key!(SpanNodeId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum AbstractSpan2 {
    Node(Rc<SpanNode>, Span),
    Interned(SpanNodeId),
}

impl AbstractSpan2 {
    pub fn from_literal(name: &'static str, span: Span) -> Self {
        todo!()
    }

    pub fn from_name(name: Text, span: Span) -> Self {
        let node = SpanNode {
            parent: None,
            name: SpanName::Pointer(name),
        };

        AbstractSpan2::Node(Rc::new(node), span)
    }
}

#[derive(Debug)]
pub struct SpanNodeTable(HashMap<SpanNodeId, Span>);

impl SpanNodeTable {
    pub fn new() -> SpanNodeTable {
        SpanNodeTable(HashMap::new())
    }
}

impl Default for SpanNodeTable {
    fn default() -> Self {
        Self::new()
    }
}

impl AbstractSpan2 {
    pub fn intern(self, interner: &dyn SpanInterner, table: &mut SpanNodeTable) -> Self {
        match self {
            AbstractSpan2::Node(node, span) => {
                let id = interner.intern_span_node(node);
                table.0.insert(id, span);
                AbstractSpan2::Interned(id)
            }
            interned @ AbstractSpan2::Interned(_) => interned,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SpanNode {
    pub parent: Option<Rc<SpanNode>>,
    pub name: SpanName,
}

impl SpanNode {
    pub fn prepend(&mut self, offset: Rc<SpanNode>) {
        match &self.parent {
            None => self.parent = Some(offset),
            Some(parent) => panic!(
                "'{}' already has a parent '{}'",
                self.as_string(),
                parent.name
            ),
        }
    }

    pub fn iter(&self) -> HierOffsetIter {
        HierOffsetIter(Some(self))
    }

    pub fn as_string(&self) -> String {
        self.iter()
            .map(|it| it.name.as_str().to_owned())
            .collect::<Vec<String>>()
            .join("/")
    }
}

pub struct HierOffsetIter<'a>(Option<&'a SpanNode>);

impl<'a> Iterator for HierOffsetIter<'a> {
    type Item = &'a SpanNode;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            Some(offset) => {
                let next = offset.parent.as_ref().map(|it| it.as_ref());
                self.0 = next;
                Some(offset)
            }
            None => None,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct RelativeOffset {
    pub parent: Rc<AbstractOffset>,
    pub offset: u16,
    pub length: u16,
}

impl RelativeOffset {
    pub fn absolute_offset(&self) -> CopyRange {
        let mut offset: u32 = 0;
        let mut current = Some(self.parent.as_ref());
        loop {
            match current {
                Some(parent) => {
                    offset += parent.length() as u32;
                    current = parent.parent();
                }
                None => break,
            }
        }
        let start = offset;
        let end = start + self.length as u32;
        CopyRange { start, end }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum AbstractOffset {
    Absolute(CopyRange),
    Relative(RelativeOffset),
}

impl AbstractOffset {
    fn parent(&self) -> Option<&AbstractOffset> {
        match self {
            AbstractOffset::Absolute(_) => None,
            AbstractOffset::Relative(relative) => Some(relative.parent.as_ref()),
        }
    }

    fn length(&self) -> u16 {
        match self {
            // TODO check for overflow
            AbstractOffset::Absolute(range) => (range.end - range.start) as u16,
            AbstractOffset::Relative(relative) => relative.length,
        }
    }

    fn range(&self) -> CopyRange {
        match self {
            AbstractOffset::Absolute(range) => *range,
            AbstractOffset::Relative(relative) => relative.absolute_offset(),
        }
    }
}

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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AbstractSpan {
    pub source: SpanSourceId,
    pub location: AbstractOffset,
}

impl AbstractSpan {
    fn absolute(span: Span) -> AbstractSpan {
        AbstractSpan {
            source: span.source,
            location: AbstractOffset::Absolute(span.location),
        }
    }
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

    #[salsa::interned]
    fn intern_span_node(&self, node: Rc<SpanNode>) -> SpanNodeId;
}

#[salsa::query_group(StringInternerDatabase)]
pub trait StringInterner {
    #[salsa::interned]
    fn intern_string(&self, string: String) -> StringId;
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct StringId(salsa::InternId);

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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct SpanSourceId(salsa::InternId);

impl_intern_key!(SpanSourceId);

#[cfg(test)]
mod tests {
    use super::*;

    fn offset(name: &'static str) -> SpanNode {
        SpanNode {
            parent: None,
            name: SpanName::Literal(name),
        }
    }

    #[should_panic]
    #[test]
    fn test_existing_parent() {
        let func = offset("function");
        let mut arg_name = offset("arg_name");
        let arg_type = offset("arg_type");

        arg_name.prepend(Rc::new(func));

        arg_name.prepend(Rc::new(arg_type));
    }

    #[test]
    fn test_prepend_offset() {
        let func = offset("function");
        let mut arg_name = offset("arg_name");

        arg_name.prepend(Rc::new(func));

        assert_eq!(arg_name.as_string(), "arg_name/function");
    }
}
