use std::collections::HashMap;
use std::hash::Hash;
use std::rc::{Rc, Weak};
use petgraph::Graph;
use petgraph::graph::DiGraph;
use crate::language::{ast, Ident, im};
use crate::language::im::EventRef;

struct QueryCache<K: Eq + Hash, V> {
    map: HashMap<K, V>
}

impl<K: Eq + Hash, V> QueryCache<K, V> {

    fn add(&self, key: K, value: V) {
    }

    fn get(&self, key: &K) -> Option<V> {
        todo!()
    }
}

struct RuleDeclarationQuery;

impl RuleDeclarationQuery {

    fn query(&mut self) {

    }
}