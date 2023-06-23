use crate::language::Text;

pub struct Ident(pub Text);

pub struct Event {
    pub name: Ident,
    pub team: Ident,
    pub hero_slot: Ident,
}

pub enum Element {
    Condition(Condition),
    Ident(Ident),
    Function(Function),
}

pub struct Function {
    pub name: Ident,
    pub args: Vec<Element>,
}

pub enum Op {
    Equals,
    NotEquals,
}

pub struct Condition {
    left: Function,
    op: Op,
    right: Function,
}

pub struct Rule {
    event: Event,
    conditions: Vec<Condition>,
    actions: Vec<Function>,
}
