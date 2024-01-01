use super::{Op, Text, TextId};
use colomar_macros::Interned;
use hashlink::LinkedHashMap;
use lazy_static::lazy_static;
use regex::Regex;
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

lazy_static! {
    static ref TEMPLATE_REGEX: Regex = Regex::new(r"\$\w*\$").unwrap();
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
                let string = placeholder.0.replace("$caller$", caller.0.as_str());
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

impl From<String> for Wscript {
    fn from(value: String) -> Self {
        if TEMPLATE_REGEX.is_match(&value) {
            Wscript::Placeholder(PlaceholderWscript(value.into()))
        } else {
            Wscript::Literal(LiteralWscript(value.into()))
        }
    }
}

impl From<LiteralWscript> for Text {
    fn from(value: LiteralWscript) -> Self {
        Text::from(value.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_iterator;

    #[test]
    fn test_regex_capture() {
        let code = "Small Message($caller$, $message$)";
        let expected_names = ["$caller$", "$message$"];

        let actual_name: Vec<_> = TEMPLATE_REGEX
            .find_iter(code)
            .map(|mat| mat.as_str())
            .collect();

        assert_iterator!(actual_name, expected_names);
    }
}
