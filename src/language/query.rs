use std::any::Any;
use std::collections::HashMap;
use std::ffi::c_void;
use std::hash::Hash;
use crate::language::im;

pub struct QueryCache<K, V> {
    map: HashMap<K, V>
}

impl<K: Eq + Hash, V: Clone> QueryCache<K, V> {

    pub fn contains(&self, key: &K) -> Option<V> {
        self.map.get(key).map(|it| it.clone())
    }

    pub fn add(&mut self, key: K, value: V) -> V {
        value
    }
}