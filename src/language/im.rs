use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use crate::language::ast::Spanned;
use crate::language::{Ident, Span};

pub type RuleRef = Rc<RefCell<Rule>>;
pub type EnumRef = Rc<RefCell<Enum>>;
pub type EventRef = Rc<RefCell<Event>>;

// Intermediate
pub type Im = Vec<Root>;

#[derive(Debug, Clone)]
pub struct IdentChain(pub Vec<Ident>);

#[derive(Debug, Clone)]
pub enum Root {
    Rule(RuleRef),
    Enum(EnumRef),
    Event(EventRef),
}

impl Root {

    pub fn name(&self) -> &'static str{
        match self {
            Root::Event(_) => "Event",
            Root::Enum(_) => "Enum",
            Root::Rule(_) => "Rule"
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Root::Rule(rule) => rule.borrow().span.clone(),
            Root::Enum(r#enum) => r#enum.borrow().span.clone(),
            Root::Event(event) => event.borrow().span.clone()
        }
    }
}

#[derive(Debug)]
pub struct EnumConstant {
    pub name: Ident
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: Ident,
    pub is_workshop: Spanned<bool>,
    pub constants: Vec<Rc<EnumConstant>>,
    pub span: Span
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    EnumConstant(Rc<EnumConstant>)
}

#[derive(Debug, Clone)]
pub enum Type {
    Enum(EnumRef)
}

#[derive(Debug, Clone)]
pub enum Link<T: Debug + Clone, V: Debug + Clone> {
    Unbound(T),
    Bound(V)
}

impl<T: Debug + Clone, V: Debug + Clone> Link<T, V> {

    pub fn unbound_or_panic(&self) -> &T {
        match self {
            Link::Unbound(value) => value,
            Link::Bound(_) => panic!("Link {self:?} was expected to be unbound, but was bound")
        }
    }

    pub fn bound_or_panic(&self) -> &V {
        match self {
            Link::Unbound(_) => panic!("Link {self:?} was expected to be bound, but was unbound"),
            Link::Bound(value) => value,
        }
    }

}

#[derive(Debug, Clone)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub types: Vec<Link<Ident, Type>>,
    pub default_value: Option<Link<IdentChain, ConstValue>>
}

#[derive(Debug, Clone)]
pub struct CalledArgument {
    pub declared: Rc<RefCell<DeclaredArgument>>,
    pub value: ConstValue
}

#[derive(Debug, Clone)]
pub struct Event {
    pub name: Ident,
    pub arguments: Vec<Rc<RefCell<DeclaredArgument>>>,
    pub span: Span
}

#[derive(Debug)]
pub struct Rule {
    pub title: String,
    pub event: Link<Ident, EventRef>,
    pub arguments: Link<Vec<IdentChain>, Vec<CalledArgument>>,
    pub span: Span
}