use crate::HashableMap;
use serde::Deserialize;

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Element {
    Event(String, Event),
    Struct(String, Struct),
    Enum(String, Enum),
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Event {
    pub name: String,
    pub args: Vec<String>,
    pub context: HashableMap<String, String>,
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    pub me: String,
    pub properties: HashableMap<String, String>,
    pub functions: HashableMap<String, String>,
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Enum {
    pub constants: HashableMap<String, String>,
}
