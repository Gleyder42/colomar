use crate::compiler::{HashableMap, Text};
use crate::impl_intern_key;
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
pub enum Owscript {
    Literal(LiteralOwscript),
    Placeholder(PlaceholderOwscript),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LiteralOwscript(pub Text);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PlaceholderOwscript(Text);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NativeFunc {
    pub script: PlaceholderOwscript,
    pub default_args: HashableMap<String, Call>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NativeFuncId(salsa::InternId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NativeEvent {
    event_name: LiteralOwscript,
    default_args: Option<[LiteralOwscript; 2]>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NativeEventId(salsa::InternId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CallNativeEvent {
    event: NativeEventId,
    args: Option<[LiteralOwscript; 2]>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Rule {
    name: Text,
    event: CallNativeEvent,
    conditions: Vec<Condition>,
    actions: Vec<Call>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Op {
    Equals,
    NotEquals,
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
        args: HashableMap<String, Box<Call>>,
    },
    Property(Text),
    Variable(Text),
    Condition(Condition),
    String(Text),
    Number(Text),
    Boolean(bool),
}

impl_intern_key!(NativeFuncId);

impl_intern_key!(NativeEventId);

lazy_static! {
    static ref TEMPLATE_REGEX: Regex = Regex::new(r#"\$\w*\$"#).unwrap();
}

impl LiteralOwscript {
    pub fn new(text: impl Into<Text>) -> Self {
        LiteralOwscript(text.into())
    }
}

impl Owscript {
    pub fn saturate(self, caller: LiteralOwscript) -> LiteralOwscript {
        match self {
            Owscript::Literal(literal) => literal,
            Owscript::Placeholder(placeholder) => {
                let string = placeholder.0.replace("$caller$", caller.0.as_str());
                LiteralOwscript::new(string)
            }
        }
    }
}

impl From<Text> for LiteralOwscript {
    fn from(value: Text) -> Self {
        LiteralOwscript(value)
    }
}

impl From<String> for Owscript {
    fn from(value: String) -> Self {
        if TEMPLATE_REGEX.is_match(&value) {
            Owscript::Placeholder(PlaceholderOwscript(value.into()))
        } else {
            Owscript::Literal(LiteralOwscript(value.into()))
        }
    }
}

impl From<LiteralOwscript> for Text {
    fn from(value: LiteralOwscript) -> Self {
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
            .into_iter()
            .map(|mat| mat.as_str())
            .collect();

        assert_iterator!(actual_name, expected_names);
    }
}
