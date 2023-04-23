use derivative::Derivative;
use crate::language::{Ident, Spanned};
use crate::Span;

pub type Condition = CallChain;

pub type SpannedBool = Option<Spanned<()>>;

// Abstract Syntax Tree
#[derive(Debug)]
pub struct Ast(pub Vec<Root>);

impl IntoIterator for Ast {
    type Item = Root;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug)]
pub enum Root {
    Event(Event),
    Rule(Rule),
    Enum(Enum),
    Struct(Struct)
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Action {
    CallChain(CallChain),
    Property(PropertyDeclaration)
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Types {
    pub values: Vec<Ident>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

impl IntoIterator for Types {
    type Item = Ident;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

#[cfg(test)]
impl From<Vec<Ident>> for Types {
    fn from(value: Vec<Ident>) -> Self {
        Types { values: value, span: 0..1 }
    }
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct EventDeclaration {
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub by: Option<(Ident, CallArguments)>,
    pub arguments: Spanned<Vec<DeclaredArgument>>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct EventDefinition {
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>,
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Event {
    pub declaration: EventDeclaration,
    pub definition: EventDefinition,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub enum UseRestriction {
    GetVal,
    SetVar,
    Val,
    Var
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct PropertyDeclaration {
    pub is_workshop: SpannedBool,
    pub use_restriction: Spanned<UseRestriction>,
    pub name: Ident,
    pub r#type: Ident
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub arguments: Spanned<Vec<DeclaredArgument>>
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct StructDeclaration {
    pub is_open: SpannedBool,
    pub is_workshop: SpannedBool,
    pub name: Ident,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct StructDefinition {
    pub properties: Vec<PropertyDeclaration>,
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Struct {
    pub declaration: StructDeclaration,
    pub definition: StructDefinition,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct EnumDeclaration {
    pub is_workshop: SpannedBool,
    pub name: Ident,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct EnumDefinition {
    pub constants: Vec<Ident>,
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Enum {
    pub declaration: EnumDeclaration,
    pub definition: EnumDefinition,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub types: Types,
    pub default_value: Option<CallChain>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct RuleDeclaration {
    pub name: Spanned<String>,
    pub event: Ident,
    pub arguments: CallArguments,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct RuleDefinition {
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>,
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Rule {
    pub declaration: RuleDeclaration,
    pub definition: RuleDefinition,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

#[derive(Derivative, Debug, Hash, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Block {
    pub actions: Vec<Action>,
    pub conditions: Vec<Condition>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

/// Multiple [CallChain]s form arguments.
/// ## Example
/// - (Team.All, Hero.Ana)
/// - (player, 100)
pub type CallArguments = Spanned<Vec<CallChain>>;

/// Multiple idents form a call chain.
/// ## Example
/// - Team.All
/// - Hello.World
/// - player.heal()
pub type CallChain = Spanned<Vec<Box<Call>>>;

/// Represent a single ident with the intention to use it as a call.
/// A call here means like referencing a variable or function.
/// A call us usually not alone and is mostly user within [CallChain] and [CallArguments]
/// ## Example
/// - Team
/// - All
/// - hello
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Call {
    /// An ident followed by call arguments.
    /// [CallArguments] might is recursive to [Call] so call must be used behind a pointer.
    /// ## Example
    /// - heal(player, 100)
    /// - display(player, message)
    IdentArguments {
        name: Ident,
        args: CallArguments,
        span: Span
    },
    /// An ident.
    /// ## Example
    /// - player
    /// - value
    Ident(Ident),
    /// A string literal.
    /// ## Example
    /// - "Hello World"
    /// - "Greetings"
    String(String, Span),
    /// A number literal
    /// ## Example
    /// - 12
    /// - 1.5
    /// - 0
    Number(String, Span),
}