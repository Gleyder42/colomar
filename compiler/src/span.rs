use crate::Text;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Debug)]
struct Ident {
    value: Rc<Text>,
    span: SpanKey,
}

impl From<&'static str> for Ident {
    fn from(value: &'static str) -> Self {
        let value: Rc<Text> = Rc::new(value.into());

        Ident {
            value: value.clone(),
            span: InnerSpanKey::new(value).into(),
        }
    }
}

impl SpanKeyLinker for Ident {
    fn link_self(&self) {}

    fn span_name(&self) -> SpanKey {
        self.span.clone()
    }
}

#[derive(Debug)]
struct Parameter {
    name: Ident,
    ty: Ident,
    span: SpanKey,
}

impl Parameter {
    fn new(name: Ident, ty: Ident) -> Parameter {
        let value = name.value.clone();

        Parameter {
            name,
            ty,
            span: InnerSpanKey::new(value).into(),
        }
    }
}

impl SpanKeyLinker for Parameter {
    fn link_self(&self) {
        self.ty.link_to(self.span_name());
        self.span.link_to(self.span_name());
    }

    fn span_name(&self) -> SpanKey {
        self.name.span.clone()
    }
}

#[derive(Debug)]
struct Function {
    name: Ident,
    parameters: (Vec<Parameter>, SpanKey),
    return_value: Ident,
    span: SpanKey,
}

impl Function {
    fn new(name: Ident, parameters: (Vec<Parameter>, SpanKey), return_value: Ident) -> Self {
        let value = name.value.clone();

        Function {
            name,
            parameters,
            return_value,
            span: InnerSpanKey::new(value).into(),
        }
    }
}

impl SpanKeyLinker for Function {
    fn link_self(&self) {
        self.return_value.link_to(self.span_name());
        self.span.link_to(self.span_name());
        self.parameters.link_to(self.span_name());
    }

    fn span_name(&self) -> SpanKey {
        self.name.span.clone()
    }
}

type SpanKey = Rc<RefCell<InnerSpanKey>>;

impl SpanKeyLinker for SpanKey {
    fn link_self(&self) {}

    fn span_name(&self) -> SpanKey {
        self.clone()
    }
}

#[derive(Debug)]
struct InnerSpanKey {
    value: Rc<Text>,
    parent: Option<Rc<RefCell<InnerSpanKey>>>,
}

impl InnerSpanKey {
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

impl InnerSpanKey {
    fn new(value: Rc<Text>) -> InnerSpanKey {
        InnerSpanKey {
            value,
            parent: None,
        }
    }
}

impl Into<SpanKey> for InnerSpanKey {
    fn into(self) -> SpanKey {
        Rc::new(RefCell::new(self))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct SpanPath(Vec<Rc<Text>>);

impl Display for InnerSpanKey {
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

trait SpanKeyLinker {
    fn link_to(&self, root: SpanKey) {
        self.span_name().borrow_mut().parent = Some(root);
        self.link_self();
    }

    fn link_self(&self);

    fn span_name(&self) -> SpanKey;
}

impl<T: SpanKeyLinker> SpanKeyLinker for (Vec<T>, SpanKey) {
    fn link_self(&self) {
        self.0.iter().for_each(|it| it.link_to(self.span_name()))
    }

    fn span_name(&self) -> SpanKey {
        self.1.clone()
    }
}

#[test]
fn test() {
    let root: SpanKey = InnerSpanKey::new(Rc::new("root".into())).into();

    let param_is_running = Parameter::new("isRunning".into(), "bool".into());
    let parameters = (
        vec![param_is_running],
        InnerSpanKey::new(Rc::new("parameters".into())).into(),
    );
    let function = Function::new("sendMessage".into(), parameters, "unit".into());

    function.link_to(root);

    println!("{}", function.return_value.span.borrow());
    println!("{}", function.parameters.0[0].ty.span.borrow());
}
