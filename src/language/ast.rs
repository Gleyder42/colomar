use derivative::Derivative;
use crate::language::Ident;
use crate::Span;

pub type Condition = CallChain;
pub type CallArgs = CallArguments;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Types {
    pub types: Vec<Ident>,
    pub span: Span
}

#[cfg(test)]
impl From<Vec<Ident>> for Types {
    fn from(value: Vec<Ident>) -> Self {
        Types { types: value, span: 0..1 }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Action {
    CallChain(CallChain),
    Property(Property)
}

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
    pub by: Option<(Ident, CallArguments)>,
    pub args: Vec<DeclaredArgument>,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>,
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
pub struct Property {
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
    pub properties: Vec<Property>,
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
    pub default_value: Option<CallChain>,
    pub span: Span
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Rule {
    pub name: Spanned<String>,
    pub event: Ident,
    pub args: Vec<CallChain>,
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

/// Idents chain with dots
///
/// Team.All
/// Hello.World
/// player.heal()
pub type CallChain = Vec<Box<Call>>;

/// Multiple ident chains
///
/// (Team.All, Hero.Ana)
/// (player, 100)
pub type CallArguments = Vec<CallChain>;

/// Single ident.
///
/// Team
/// All
/// hello
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Call {
    IdentArguments {
        name: Ident,
        args: CallArguments,
        span: Span
    },
    Ident(Ident),
    String(String, Span),
    Number(String, Span),
}