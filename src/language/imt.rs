use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use crate::language::ast::Spanned;
use crate::language::Ident;
use crate::Span;

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
pub enum Ref<T: Debug + Clone, V: Debug + Clone> {
    Unbound(T),
    Bound(V)
}

#[derive(Debug, Clone)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub types: Vec<Ref<Ident, Type>>,
    pub default_value: Option<Ref<IdentChain, ConstValue>>
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
    pub event: Ref<Ident, Rc<RefCell<Event>>>,
    pub arguments: Ref<Vec<IdentChain>, Vec<CalledArgument>>
}

impl<'a> Named<'a> for String {
    fn name(&'a self) -> &'a str {
        self.as_str()
    }
}

pub trait Named<'a> {

    fn name(&'a self) -> &'a str;
}