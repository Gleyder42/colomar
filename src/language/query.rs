use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::Map;
use std::marker::PhantomData;
use std::rc::{Rc, Weak};
use derivative::Derivative;
use petgraph::Graph;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::graphmap::DiGraphMap;
use petgraph::prelude::GraphMap;
use petgraph::visit::Visitable;
use crate::language::{ast, Ident, im};

#[derive(Hash, PartialEq, Eq, Copy, Clone, PartialOrd, Ord)]
enum ValidityGuarantee {
    /// Guarantees that the fulls struct can be read from cache
    Deep,
    /// Guarantees that only struct itself can be read from the cache.
    /// Links to other structs may not be valid anymore
    Shallow
}

#[derive(Hash, Eq, PartialEq, Clone)]
enum CacheKey {
    Ast(ast::Ast),
    RuleDeclaration(ast::RuleDeclaration),
    CallChain(ast::CallChain)
}

enum CacheValue {
    Im(im::Im),
    RuleDeclaration(im::RuleDeclaration),
    AValue(im::AValue)
}

struct QueryCacheNode {
    validity_guarantee: ValidityGuarantee,
    value: CacheValue
}

pub struct QueryGraph {
    graph: DiGraph<Rc<QueryCacheNode>, ()>,
    map: HashMap<CacheKey, Weak<QueryCacheNode>>
}

trait NodeAdder<K, V> {

    fn add(&mut self, parent: NodeIndex, key: K, value: V) -> NodeIndex;

    fn retrieve(&self, key: K, validity: ValidityGuarantee) -> (Option<V>, K);
}

impl NodeAdder<ast::Ast, im::Im> for QueryGraph {

    fn add(&mut self, parent: NodeIndex, key: ast::Ast, value: im::Im) -> NodeIndex {
        self.add_node(Some(parent), CacheKey::Ast(key), CacheValue::Im(value))
    }

    /// Gets the value from the cache and clones it.
    ///
    /// # Implementation details
    /// Because cached keys are of type [CacheKey], we need ownership of the wrapped value and
    /// cannot pass a borrowed value, although ownership is functionality wise not required.
    /// The solution is take ownership and give it back through the return value.
    fn retrieve(&self, key: ast::Ast, validity: ValidityGuarantee) -> (Option<im::Im>, ast::Ast) {
        let node: CacheKey = CacheKey::Ast(key);
        let option = self.get_node(&node)
            .filter(|it| it.validity_guarantee == validity)
            .map(|it| match &it.value {
                CacheValue::Im(im) => Some(im.clone()),
                _ => None
            }).flatten();

        let ast = match node {
            CacheKey::Ast(ast) => ast,
            _ => unreachable!()
        };

        (option, ast)
    }
}

impl QueryGraph {

    pub fn new() -> QueryGraph {
        QueryGraph { graph: DiGraph::new(), map: HashMap::new() }
    }

    fn add_node(&mut self, parent: Option<NodeIndex>, key: CacheKey, value: CacheValue) -> NodeIndex {
        let node: Rc<_> = QueryCacheNode {
            value, validity_guarantee: ValidityGuarantee::Deep
        }.into();

        self.map.insert(key, Rc::downgrade(&node));
        let index = self.graph.add_node(node);
        if let Some(parent) = parent {
            self.graph.add_edge(parent, index, ());
        }
        index
    }

    fn get_node(&self, key: &CacheKey) -> Option<Rc<QueryCacheNode>> {
        self.map.get(key)
            .map(|it| it.upgrade())
            .flatten()
    }
}


fn query_ast(ast: ast::Ast, parent: NodeIndex, graph: &mut QueryGraph) -> im::Im {
    let (cached, ast) = graph.retrieve(ast, ValidityGuarantee::Deep);
    if let Some(cached) = cached {
        return cached;
    }

    let mut vec = Vec::new();
    for root in ast.clone().into_iter() {
        match root {
            ast::Root::Event(event) => {
                let declaration = query_event_declaration(event.declaration);
                vec.push(im::Root::Event(declaration));
            },
            _ => todo!()
        }
    }

    let im = im::Im(vec);
    graph.add(parent, ast, im.clone());
    im
}

fn query_type(ident: Ident) -> im::Type {
    todo!()
}

fn query_types(types: ast::Types) -> im::Types {
    im::Types {
        types: types.values.into_iter()
            .map(|r#type| query_type(r#type))
            .collect::<Vec<_>>(),
        span: types.span
    }
}

fn query_avalue(call_chain: ast::CallChain) -> im::AValue {
    todo!()
}

fn query_event_declaration(event: ast::EventDeclaration) -> im::EventRef {
    let arguments = event.arguments.into_iter()
        .map(|argument| {
            im::DeclaredArgument {
                name: argument.name,
                types: query_types(argument.types),
                default_value: argument.default_value.map(query_avalue)
            }.into()
        })
        .collect();

    im::EventDeclaration {
        name: event.name,
        span: event.span,
        arguments
    }.into()
}

fn query_enum(r#enum: ast::Enum) -> im::EnumRef {
    todo!()
}