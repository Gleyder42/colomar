use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
use derivative::Derivative;
use petgraph::Graph;
use petgraph::graph::DiGraph;
use petgraph::graphmap::DiGraphMap;
use petgraph::prelude::GraphMap;
use petgraph::visit::Visitable;
use crate::language::{ast, im};

#[derive(Hash, PartialEq, Eq, Copy, Clone, PartialOrd, Ord)]
enum ValidityGuarantee {
    /// Guarantees that the fulls struct can be read from cache
    Deep,
    /// Guarantees that only struct itself can be read from the cache.
    /// Links to other structs may not be valid anymore
    Shallow
}

#[derive(Hash, Eq, PartialEq)]
enum CacheKey {
    RuleDefinition(ast::RuleDeclaration)
}

enum CacheValue {
    RuleDefinition(im::RuleDeclaration)
}


struct QueryCacheNode {
    validity_guarantee: ValidityGuarantee,
    value: CacheValue
}

fn query() {

}

fn query_enum(r#enum: ast::Enum) -> im::EnumRef {
    todo!()
}