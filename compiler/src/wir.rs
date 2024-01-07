use super::{Op, Text, TextId};
use colomar_macros::Interned;
use hashlink::LinkedHashMap;
use std::hash::Hash;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Category {
    Enum,
    Event,
    Struct,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Wscript {
    Literal(LiteralWscript),
    Placeholder(PlaceholderWscript),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LiteralWscript(pub Text);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PlaceholderWscript(Text);

#[derive(Clone, Debug, Hash, PartialEq, Eq, Interned)]
pub struct NativeFunc {
    pub script: PlaceholderWscript,
    pub default_args: LinkedHashMap<String, Call>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Interned)]
pub struct NativeEvent {
    event_name: LiteralWscript,
    default_args: Option<[LiteralWscript; 2]>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CallNativeEvent {
    event: NativeEventId,
    args: Option<[LiteralWscript; 2]>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Rule {
    name: TextId,
    event: CallNativeEvent,
    conditions: Vec<Condition>,
    actions: Vec<Call>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Condition {
    pub left: Box<Call>,
    pub op: Op,
    pub right: Box<Call>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Call {
    Call {
        name: NativeFuncId,
        args: LinkedHashMap<String, Box<Call>>,
    },
    Property(TextId),
    Variable(TextId),
    Condition(Condition),
    String(TextId),
    Number(TextId),
    Boolean(bool),
}

impl LiteralWscript {
    pub fn new(text: impl Into<Text>) -> Self {
        LiteralWscript(text.into())
    }
}

impl Wscript {
    pub fn saturate(self, caller: LiteralWscript) -> LiteralWscript {
        match self {
            Wscript::Literal(literal) => literal,
            Wscript::Placeholder(placeholder) => {
                let string = placeholder.0.replace("caller", caller.0.as_str());
                LiteralWscript::new(string)
            }
        }
    }
}

impl From<Text> for LiteralWscript {
    fn from(value: Text) -> Self {
        LiteralWscript(value)
    }
}

impl From<LiteralWscript> for Text {
    fn from(value: LiteralWscript) -> Self {
        Text::from(value.0)
    }
}
