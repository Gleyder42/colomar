use std::rc::Rc;
use crate::language::ast::Spanned;
use crate::language::Ident;
use crate::Span;

// Intermediate Tree
#[derive(Debug)]
pub struct Imt(pub Vec<Root>);

#[derive(Debug)]
pub enum Root {
    Rule(Rule),
    Enum(Rc<Enum>),
    Event(Rc<Event>),
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
    Enum(Rc<Enum>)
}

#[derive(Debug)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub types: Vec<Type>,
    pub default_values: Option<ConstValue>
}

#[derive(Debug, Clone)]
pub struct CalledArgument {
    pub declared: Rc<DeclaredArgument>,
    pub value: ConstValue
}

#[derive(Debug, Clone)]
pub struct Event {
    pub name: Ident,
    pub arguments: Vec<Rc<DeclaredArgument>>,
}

#[derive(Debug)]
pub struct Rule {
    pub title: String,
    pub event: Option<Rc<Event>>,
    pub arguments: Vec<CalledArgument>
}

impl<'a> Named<'a> for String {
    fn name(&'a self) -> &'a str {
        self.as_str()
    }
}

pub trait Named<'a> {

    fn name(&'a self) -> &'a str;
}