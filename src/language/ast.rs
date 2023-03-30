use crate::language::Ident;
use crate::language::imt::Named;
use crate::Span;

pub type Action = Box<Call>;
pub type Condition = Box<Call>;
pub type CallArgs = Vec<Box<Call>>;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Spanned<T>(pub T, pub Span);

// Abstract Syntax Tree
#[derive(Debug)]
pub struct Ast(pub Vec<Root>);

#[derive(Debug)]
pub enum Root {
    Event(Event),
    Rule(Rule),
    Enum(Enum),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Event {
    pub name: Ident,
    pub by: Option<(Ident, Vec<Box<Call>>)>,
    pub args: Vec<DeclaredArgument>,
    pub span: Span
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Enum {
    pub is_workshop: Spanned<bool>,
    pub name: Ident,
    pub constants: Vec<Ident>,
    pub span: Span
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub types: Vec<Ident>,
    pub default_value: Option<Box<Call>>,
    pub span: Span
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Rule {
    pub name: Spanned<String>,
    pub event: Ident,
    pub args: Vec<Box<Call>>,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>,
    pub span: Span
}

#[derive(Debug)]
pub struct Block {
    pub actions: Vec<Action>,
    pub conditions: Vec<Condition>,
    pub span: Span
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Call {
    Fn {
        name: Ident,
        args: CallArgs,
        next: Option<Box<Call>>,
        span: Span
    },
    Var {
        name: Ident,
        next: Option<Box<Call>>
    },
}

#[cfg(test)]
impl Call {
    pub fn new_var(name: Ident) -> Box<Self> {
        Box::new(Call::Var { name, next: None })
    }

    pub fn new_var_next(name: Ident, next: Box<Call>) -> Box<Self> {
        Box::new(Call::Var { name, next: Some(next)  })
    }

    pub fn new_fn(name: Ident, span: Span) -> Box<Self> {
        Box::new(Call::Fn { name, args: Vec::new(), next: None, span  })
    }

    pub fn new_fn_next(name: Ident, span: Span, next: Box<Call>) -> Box<Self> {
        Box::new(Call::Fn { name, args: Vec::new(), next: Some(next), span  })
    }

    pub fn new_fn_args(name: Ident, span: Span, args: CallArgs) -> Box<Self> {
        Box::new(Call::Fn { name, args, next: None, span  })
    }

    pub fn new_fn_args_next(name: Ident, span: Span, args: CallArgs, next: Box<Call>) -> Box<Self> {
        Box::new(Call::Fn { name, args, next: Some(next), span  })
    }
}