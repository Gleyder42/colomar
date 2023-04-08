use derivative::Derivative;
use crate::language::Ident;
use crate::Span;

pub type Action = Box<Call>;
pub type Condition = Box<Call>;
pub type CallArgs = Vec<Box<Call>>;
pub type Types = Vec<Ident>;

#[derive(Derivative, Debug, Hash, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Spanned<T>(
    pub T,
    #[derivative(PartialEq = "ignore")] pub Span
);

// Abstract Syntax Tree
pub type Ast = Vec<Root>;

#[derive(Debug)]
pub enum Root {
    Event(Event),
    Rule(Rule),
    Enum(Enum),
    Struct(Struct)
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Event {
    pub name: Ident,
    pub by: Option<(Ident, Vec<Box<Call>>)>,
    pub args: Vec<DeclaredArgument>,
    pub conditions: Vec<Condition>,
    pub span: Span
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum PropertyDesc {
    GetVal,
    SetVar,
    Val,
    Var
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct StructProperty {
    pub is_workshop: Spanned<bool>,
    pub desc: PropertyDesc,
    pub name: Ident,
    pub r#type: Ident
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Function {
    pub is_workshop: Spanned<bool>,
    pub name: Ident,
    pub arguments: Vec<DeclaredArgument>
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Struct {
    pub is_open: Spanned<bool>,
    pub is_workshop: Spanned<bool>,
    pub name: Ident,
    pub properties: Vec<StructProperty>,
    pub functions: Vec<Function>,
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
    pub types: Types,
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
    String {
        value: String,
        next: Option<Box<Call>>
    },
    Number {
        value: String,
        next: Option<Box<Call>>
    }
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