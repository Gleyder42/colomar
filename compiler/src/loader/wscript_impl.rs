use crate::trisult::Trisult;
use hashlink::LinkedHashMap;
use serde::Deserialize;
use std::path::PathBuf;

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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ImplEntry<T> {
    pub path: PathBuf,
    pub value: T,
}

impl<T, E> Trisult<ImplEntry<T>, E> {
    pub fn map_impl_entry<U>(self, func: impl FnOnce(T) -> U) -> Trisult<ImplEntry<U>, E> {
        self.map(|entry| ImplEntry {
            path: entry.path,
            value: func(entry.value),
        })
    }
}
