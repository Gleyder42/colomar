use hashlink::LinkedHashMap;
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
    pub context: LinkedHashMap<String, String>,
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    pub properties: LinkedHashMap<String, String>,
    pub functions: LinkedHashMap<String, String>,
}

#[derive(Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Enum {
    pub constants: LinkedHashMap<String, String>,
}
