pub type Action = Box<Call>;
pub type Condition = Box<Call>;
pub type CallArgs = Vec<Box<Call>>;

#[derive(Debug)]
pub struct Ast(pub Vec<Root>);

#[derive(Debug)]
pub enum Root {
    Event(Event),
    Rule(Rule),
    Enum(Enum),
}

#[derive(Debug)]
pub struct Event {
    pub event: String,
    pub by: Option<(String, Vec<Box<Call>>)>,
    pub args: Vec<DeclaredArgument>,
}

#[derive(Debug)]
pub struct Enum {
    pub is_workshop: bool,
    pub name: String,
    pub constants: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub struct DeclaredArgument {
    pub name: String,
    pub types: Vec<String>,
    pub default_value: Option<Box<Call>>,
}

#[derive(Debug)]
pub struct Rule {
    pub name: String,
    pub event: String,
    pub args: Vec<Box<Call>>,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>,
}

#[derive(Debug)]
pub struct Block {
    pub actions: Vec<Action>,
    pub conditions: Vec<Condition>,
}

#[derive(Debug, PartialEq)]
pub enum Call {
    Fn {
        name: String,
        args: CallArgs,
        next: Option<Box<Call>>,
    },
    Var {
        name: String,
        next: Option<Box<Call>>,
    },
}

impl Call {
    pub fn new_var(name: impl Into<String>) -> Box<Self> {
        Box::new(Call::Var { name: name.into(), next: None })
    }

    pub fn new_var_next(name: impl Into<String>, next: Box<Call>) -> Box<Self> {
        Box::new(Call::Var { name: name.into(), next: Some(next) })
    }

    pub fn new_fn(name: impl Into<String>) -> Box<Self> {
        Box::new(Call::Fn { name: name.into(), args: Vec::new(), next: None })
    }

    pub fn new_fn_next(name: impl Into<String>, next: Box<Call>) -> Box<Self> {
        Box::new(Call::Fn { name: name.into(), args: Vec::new(), next: Some(next) })
    }

    pub fn new_fn_args(name: impl Into<String>, args: CallArgs) -> Box<Self> {
        Box::new(Call::Fn { name: name.into(), args, next: None })
    }

    pub fn new_fn_args_next(name: impl Into<String>, args: CallArgs, next: Box<Call>) -> Box<Self> {
        Box::new(Call::Fn { name: name.into(), args, next: Some(next) })
    }
}