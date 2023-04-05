use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use crate::language::ast::Spanned;
use crate::language::Ident;

pub type RefRule = Rc<RefCell<Rule>>;

// Intermediate Tree
#[derive(Debug)]
pub struct Imt(pub Vec<Root>);

#[derive(Debug, Clone)]
pub struct IdentChain(pub Vec<Ident>);

#[derive(Debug, Clone)]
pub enum Root {
    Rule(Rc<RefCell<Rule>>),
    Enum(Rc<RefCell<Enum>>),
    Event(Rc<RefCell<Event>>),
}

#[derive(Debug)]
pub struct EnumConstant {
    pub name: Ident
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: Ident,
    pub is_workshop: Spanned<bool>,
    pub constants: Vec<Rc<EnumConstant>>
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    EnumConstant(Rc<EnumConstant>)
}

#[derive(Debug, Clone)]
pub enum Type {
    Enum(Rc<RefCell<Enum>>)
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
    pub arguments: Vec<Rc<RefCell<DeclaredArgument>>>
}

#[derive(Debug)]
pub struct Rule {
    pub title: String,
    pub event: Link<Ident, Rc<RefCell<Event>>>,
    pub arguments: Link<Vec<IdentChain>, Vec<CalledArgument>>
}