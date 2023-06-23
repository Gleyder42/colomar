use crate::language::Text;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident(pub Text);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Event {
    pub name: Ident,
    pub team: Ident,
    pub hero_slot: Ident,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Element {
    Condition(Condition),
    Ident(Ident),
    Function(Function),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub args: Vec<Element>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Op {
    Equals,
    NotEquals,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Condition {
    left: Function,
    op: Op,
    right: Function,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rule {
    event: Event,
    conditions: Vec<Condition>,
    actions: Vec<Function>,
}
