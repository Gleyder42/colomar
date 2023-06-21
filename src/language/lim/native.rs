use crate::language::{HashableHashMap, Text};
use serde::Deserialize;
use std::collections::HashMap;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Element {
    Event(String, Event),
    Struct(String, Struct),
    Enum(String, Enum),
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Event {
    pub name: String,
    pub args: HashableHashMap<String, String>,
    pub context: HashableHashMap<String, String>,
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    #[serde(rename(serialize = "self"))]
    pub me: String,
    pub properties: HashableHashMap<String, String>,
    pub functions: HashableHashMap<String, String>,
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Enum {
    pub constants: HashableHashMap<String, String>,
}
