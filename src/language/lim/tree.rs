use crate::impl_intern_key;
use crate::language::PROPERTY_DECLS_LEN;
use crate::language::{HashableHashMap, Text, CALLED_ARGUMENTS_LEN};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Category {
    Enum,
    Event,
    Struct,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum NativeCode {
    Literal(LiteralNativeCode),
    Template(TemplateNativeCode),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LiteralNativeCode(pub Text);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplateNativeCode(pub Text);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NativeFunc {
    pub code: TemplateNativeCode,
    pub default_args: HashableHashMap<String, Call>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NativeFuncId(salsa::InternId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NativeEvent {
    event_name: LiteralNativeCode,
    default_args: Option<[LiteralNativeCode; 2]>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NativeEventId(salsa::InternId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CallNativeEvent {
    event: NativeEventId,
    args: Option<[LiteralNativeCode; 2]>,
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
        args: HashableHashMap<String, Box<Call>>,
    },
    Condition(Condition),
    String(Text),
    Number(Text),
    Boolean(bool),
}

impl_intern_key!(NativeFuncId);
impl_intern_key!(NativeEventId);
