use crate::{impl_intern_key, CheapRange, SpanInterner, Text};
use petgraph::data::DataMap;
use petgraph::prelude::*;
use smallvec::SmallVec;
use std::fmt::{Display, Formatter};

struct HierSpan(SpanData);

impl HierSpan {
    fn become_complete(&mut self, func: impl FnOnce(NodeIndex) -> SpanPathId) {
        let index = self.0.unwrap_intermediate();
        let path = func(index);
        self.0 = SpanData::Complete(path);
    }

    fn become_intermediate(&mut self, func: impl FnOnce(CheapRange) -> NodeIndex) -> NodeIndex {
        let range = self.0.unwrap_initial();
        let index = func(range);
        self.0 = SpanData::Intermediate(index);
        index
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct SpanPathId(salsa::InternId);

impl_intern_key!(SpanPathId);

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct SpanPath {
    path: SmallVec<[Text; 8]>,
}

impl SpanPath {
    pub fn new(path: impl Into<SmallVec<[Text; 8]>>) -> Self {
        SpanPath { path: path.into() }
    }
}

impl Display for SpanPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let display = self
            .path
            .iter()
            .map(|it| it.as_str())
            .collect::<Vec<_>>()
            .join("/");
        write!(f, "{}", display)
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum SpanData {
    Initial(CheapRange),
    Intermediate(NodeIndex),
    Complete(SpanPathId),
}

impl SpanData {
    fn unwrap_intermediate(&self) -> NodeIndex {
        match self {
            SpanData::Initial(range) => {
                panic!("{range:?} is in initial state, but expected intermediate")
            }
            SpanData::Intermediate(node) => *node,
            SpanData::Complete(path) => {
                panic!("{path:?} is in complete state, but expected intermediate")
            }
        }
    }

    fn unwrap_initial(&self) -> CheapRange {
        match self {
            SpanData::Initial(range) => *range,
            SpanData::Intermediate(node) => {
                panic!("{node:?} is in intermediate state but expected inital")
            }
            SpanData::Complete(path) => {
                panic!("{path:?} is in complete state, but expected inital")
            }
        }
    }
}

pub struct SpanNode {
    segment: Text,
    range: CheapRange,
}

impl SpanNode {
    pub fn new(segment: impl Into<Text>, range: impl Into<CheapRange>) -> Self {
        Self {
            segment: segment.into(),
            range: range.into(),
        }
    }
}

pub struct SpanGraphBuilder(DiGraph<SpanNode, ()>);

impl SpanGraphBuilder {
    fn add_node(&mut self, node: SpanNode) -> NodeIndex {
        self.0.add_node(node)
    }

    fn connect(&mut self, child: NodeIndex, parent: NodeIndex) {
        self.0.add_edge(child, parent, ());
    }

    fn get_path(&self, index: NodeIndex) -> SpanPath {
        let node = self
            .0
            .node_weight(index)
            .expect("Node should be added to the graph");
        let path = vec![node]
            .into_iter()
            .chain(
                self.0
                    .neighbors(index)
                    .map(|neighbor| self.0.node_weight(neighbor).unwrap()),
            )
            .map(|node| node.segment.clone()) // TODO do not clone
            .collect();
        SpanPath { path }
    }
}

pub trait SpanGraphLinker {
    fn build_graph(&mut self, parent: NodeIndex, graph: &mut SpanGraphBuilder);

    fn intern_graph(&mut self, interner: &dyn SpanInterner, graph: &mut SpanGraphBuilder);
}

#[cfg(test)]
mod tests {
    use crate::analysis::interner::InternerDatabase;
    use crate::database::CompilerDatabase;
    use crate::span::{HierSpan, SpanData, SpanGraphBuilder, SpanGraphLinker, SpanNode, SpanPath};
    use crate::{CheapRange, SpanInterner, SpanInternerDatabase, Text};
    use petgraph::data::DataMap;
    use petgraph::graph::NodeIndex;
    use petgraph::Graph;
    use smallvec::{smallvec, SmallVec};

    #[test]
    fn test_display_empty_segment() {
        let segment = SpanPath::new(SmallVec::new());

        assert_eq!("", segment.to_string())
    }

    #[test]
    fn test_display_segment() {
        let segment = SpanPath::new(smallvec![
            Text::from("root"),
            Text::from("function"),
            Text::from("name")
        ]);

        assert_eq!("root/function/name", segment.to_string())
    }

    struct Ident {
        name: Text,
        span: HierSpan,
    }

    struct Function {
        name: Ident,
        ty: Ident,
        params: Vec<Ident>,
    }

    impl SpanGraphLinker for Function {
        fn build_graph(&mut self, parent: NodeIndex, graph: &mut SpanGraphBuilder) {
            let anchor = &self.name.span;
        }

        fn intern_graph(&mut self, interner: &dyn SpanInterner, graph: &mut SpanGraphBuilder) {
            todo!()
        }
    }

    impl SpanGraphLinker for Ident {
        fn build_graph(&mut self, parent: NodeIndex, graph: &mut SpanGraphBuilder) {
            let anchor = self.span.become_intermediate(|range| {
                graph.add_node(SpanNode::new(self.name.clone(), range))
            });
            graph.connect(anchor, parent);
        }

        fn intern_graph(&mut self, interner: &dyn SpanInterner, graph: &mut SpanGraphBuilder) {
            self.span.become_complete(|node| {
                let path = graph.get_path(node);
                interner.intern_span_path(path)
            });
        }
    }

    #[test]
    fn test_linker() {
        let mut function = Function {
            name: Ident {
                name: Text::from("isReloading"),
                span: HierSpan(SpanData::Initial(CheapRange::from(1..2))),
            },
            ty: Ident {
                name: Text::from("bool"),
                span: HierSpan(SpanData::Initial(CheapRange::from(2..3))),
            },
            params: vec![
                Ident {
                    name: Text::from("a"),
                    span: HierSpan(SpanData::Initial(CheapRange::from(4..5))),
                },
                Ident {
                    name: Text::from("b"),
                    span: HierSpan(SpanData::Initial(CheapRange::from(5..6))),
                },
            ],
        };

        let mut graph = SpanGraphBuilder(Graph::new());

        let root = graph.0.add_node(Text::from("root"));
        let database = CompilerDatabase::default();

        function.build_graph(root, &mut graph);
    }
}
