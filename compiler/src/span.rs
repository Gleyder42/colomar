use crate::Text;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

/// Span Id
pub type Sid = Rc<Text>;

#[derive(Debug)]
struct Ident {
    value: Rc<Text>,
    span: HierSpan,
}

impl From<&'static str> for Ident {
    fn from(value: &'static str) -> Self {
        let value: Rc<Text> = Rc::new(value.into());

        Ident {
            value: value.clone(),
            span: SpanKey::new(value).into(),
        }
    }
}

impl SpanKeyLinker for Ident {
    fn link_self(&self, _span_name: HierSpan) {}

    fn span_name(&self) -> Option<HierSpan> {
        Some(self.span.clone())
    }
}

#[derive(Debug)]
struct Parameter {
    name: Ident,
    ty: Ident,
    span: HierSpan,
}

impl Parameter {
    fn new(name: Ident, ty: Ident) -> Parameter {
        let value = name.value.clone();

        Parameter {
            name,
            ty,
            span: SpanKey::new(value).into(),
        }
    }
}

impl SpanKeyLinker for Parameter {
    fn link_self(&self, span_name: HierSpan) {
        self.ty.link_to(span_name.clone());
        self.span.link_to(span_name.clone());
    }

    fn span_name(&self) -> Option<HierSpan> {
        Some(self.name.span.clone())
    }
}

#[derive(Debug)]
struct Function {
    name: Ident,
    parameters: (Vec<Parameter>, HierSpan),
    return_value: Ident,
    span: HierSpan,
}

impl Function {
    fn new(name: Ident, parameters: (Vec<Parameter>, HierSpan), return_value: Ident) -> Self {
        let value = name.value.clone();

        Function {
            name,
            parameters,
            return_value,
            span: SpanKey::new(value).into(),
        }
    }
}

impl SpanKeyLinker for Function {
    fn link_self(&self, span_name: HierSpan) {
        self.return_value.link_to(span_name.clone());
        self.span.link_to(span_name.clone());
        self.parameters.link_to(span_name.clone());
    }

    fn span_name(&self) -> Option<HierSpan> {
        Some(self.name.span.clone())
    }
}

pub type HierSpan = Rc<RefCell<SpanKey>>;

impl SpanKeyLinker for HierSpan {
    fn link_self(&self, span_name: HierSpan) {}

    fn span_name(&self) -> Option<HierSpan> {
        Some(self.clone())
    }
}

#[derive(Debug)]
pub struct SpanKey {
    value: Rc<Text>,
    parent: Option<Rc<RefCell<SpanKey>>>,
}

impl SpanKey {
    pub fn new(sid: impl Into<Sid>) -> Self {
        SpanKey {
            value: sid.into(),
            parent: None,
        }
    }

    pub fn set_parent(&mut self, span: HierSpan) {
        self.parent = Some(span);
    }

    fn as_span_path(&self) -> SpanPath {
        let mut paths = Vec::new();
        paths.push(self.value.clone());
        let mut current = self.parent.clone();
        loop {
            match current {
                Some(next) => {
                    paths.push(next.borrow().value.clone());
                    current = next.borrow().parent.clone()
                }
                None => break,
            }
        }
        SpanPath(paths)
    }
}

impl Into<HierSpan> for SpanKey {
    fn into(self) -> HierSpan {
        Rc::new(RefCell::new(self))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct SpanPath(Vec<Rc<Text>>);

impl Display for SpanKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let display = self
            .as_span_path()
            .0
            .into_iter()
            .map(|it| it.to_string())
            .rev()
            .collect::<Vec<_>>()
            .join("/");
        write!(f, "{}", display)
    }
}

pub trait SpanKeyLinker {
    fn link_to(&self, root: HierSpan) {
        if let Some(span_name) = self.span_name() {
            print!("Combine {} and {} ", root.borrow(), span_name.borrow());
            span_name.borrow_mut().parent = Some(root);
            println!("into {}", span_name.borrow());
            self.link_self(span_name);
        }
    }

    fn link_self(&self, span_name: HierSpan);

    fn span_name(&self) -> Option<HierSpan>;
}

impl<T: SpanKeyLinker> SpanKeyLinker for (Vec<T>, HierSpan) {
    fn link_self(&self, span_name: HierSpan) {
        self.0.iter().for_each(|it| it.link_to(span_name.clone()))
    }

    fn span_name(&self) -> Option<HierSpan> {
        Some(self.1.clone())
    }
}

impl<T: SpanKeyLinker> SpanKeyLinker for Option<T> {
    fn link_to(&self, root: HierSpan) {
        if let Some(value) = self {
            value.link_to(root)
        }
    }

    fn link_self(&self, span_name: HierSpan) {
        unimplemented!()
    }

    fn span_name(&self) -> Option<HierSpan> {
        unimplemented!()
    }
}

#[test]
fn test() {
    let root: HierSpan = SpanKey::new(Rc::new("root".into())).into();

    let param_is_running = Parameter::new("isRunning".into(), "bool".into());
    let parameters = (
        vec![param_is_running],
        SpanKey::new(Rc::new("parameters".into())).into(),
    );
    let function = Function::new("sendMessage".into(), parameters, "unit".into());

    function.link_to(root);

    println!("{}", function.return_value.span.borrow());
    println!("{}", function.parameters.0[0].ty.span.borrow());
}
