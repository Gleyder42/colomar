use crate::language::{HashableHashMap, Text};
use serde::Deserialize;
use std::collections::{BTreeMap, HashMap};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Element {
    Event(String, Event),
    Struct(String, Struct),
    Enum(String, Enum),
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Event {
    pub name: String,
    pub args: BTreeMap<String, String>,
    pub context: BTreeMap<String, String>,
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    #[serde(rename(serialize = "self"))]
    pub selff: String,
    pub properties: BTreeMap<String, String>,
    pub functions: BTreeMap<String, String>,
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Enum {
    pub constants: BTreeMap<String, String>,
}
