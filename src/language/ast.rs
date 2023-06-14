use crate::language::{
    Ident, ImmutableString, Span, Spanned, ACTIONS_LEN, CONDITIONS_LEN, DECLARED_ARGUMENTS_LEN,
    FUNCTIONS_DECLS_LEN, PROPERTY_DECLS_LEN,
};
use smallvec::SmallVec;

pub type Condition = CallChain;

pub type Conditions = SmallVec<[Condition; CONDITIONS_LEN]>;
pub type Actions = SmallVec<[Action; ACTIONS_LEN]>;
pub type DeclaredArguments = SmallVec<[DeclaredArgument; DECLARED_ARGUMENTS_LEN]>;
pub type PropertyDecls = SmallVec<[PropertyDeclaration; PROPERTY_DECLS_LEN]>;
pub type FunctionDecls = SmallVec<[FunctionDeclaration; FUNCTIONS_DECLS_LEN]>;

pub type SpannedBool = Option<Spanned<()>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Definition {
    Event(EventDefinition),
    Enum(EnumDefinition),
    Struct(StructDefinition),
}

impl From<EventDefinition> for Definition {
    fn from(value: EventDefinition) -> Self {
        Definition::Event(value)
    }
}

impl From<EnumDefinition> for Definition {
    fn from(value: EnumDefinition) -> Self {
        Definition::Enum(value)
    }
}

impl From<StructDefinition> for Definition {
    fn from(value: StructDefinition) -> Self {
        Definition::Struct(value)
    }
}

/// Abstract Syntax Tree
/// Stores elements not in a [SmallVec] because it is expected to have many elements.
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ast(pub Vec<Root>);

impl IntoIterator for Ast {
    type Item = Root;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Root {
    Event(Event),
    Rule(Rule),
    Enum(Enum),
    Struct(Struct),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Action {
    CallChain(CallChain),
    Property(PropertyDeclaration),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Types {
    pub values: SmallVec<[Ident; 2]>,
    pub span: Span,
}

impl IntoIterator for Types {
    type Item = Ident;
    type IntoIter = smallvec::IntoIter<[Ident; 2]>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct EventDeclaration {
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct EventDefinition {
    pub by: Option<(Ident, CallArguments)>,
    pub arguments: Spanned<DeclaredArguments>,
    pub conditions: Conditions,
    pub actions: Actions,
}

impl TryFrom<Definition> for EventDefinition {
    type Error = &'static str;

    fn try_from(value: Definition) -> Result<Self, Self::Error> {
        match value {
            Definition::Event(event) => Ok(event),
            Definition::Enum(_) => Err("Cannot convert enum to event definition"),
            Definition::Struct(_) => Err("Cannot convert struct to event definition"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Event {
    pub declaration: EventDeclaration,
    pub definition: EventDefinition,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum UseRestriction {
    GetVal,
    SetVar,
    Val,
    Var,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct PropertyDeclaration {
    pub is_workshop: SpannedBool,
    pub use_restriction: Spanned<UseRestriction>,
    pub name: Ident,
    pub r#type: Ident,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct FunctionDeclaration {
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub arguments: Spanned<DeclaredArguments>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct StructDeclaration {
    pub is_open: SpannedBool,
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct StructDefinition {
    pub properties: PropertyDecls,
    pub functions: FunctionDecls,
}

impl TryFrom<Definition> for StructDefinition {
    type Error = &'static str;

    fn try_from(value: Definition) -> Result<Self, Self::Error> {
        match value {
            Definition::Event(_) => Err("Cannot convert event to struct definition"),
            Definition::Enum(_) => Err("Cannot convert enum to struct definition"),
            Definition::Struct(r#struct) => Ok(r#struct),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Struct {
    pub declaration: StructDeclaration,
    pub definition: StructDefinition,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct EnumDeclaration {
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct EnumDefinition {
    pub constants: Vec<Ident>,
}

impl TryFrom<Definition> for EnumDefinition {
    type Error = &'static str;

    fn try_from(value: Definition) -> Result<Self, Self::Error> {
        match value {
            Definition::Event(_) => Err("Cannot convert event to enum definition"),
            Definition::Enum(r#enum) => Ok(r#enum),
            Definition::Struct(_) => Err("Cannot convert struct to enum definition"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Enum {
    pub declaration: EnumDeclaration,
    pub definition: EnumDefinition,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub types: Types,
    pub default_value: Option<CallChain>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Rule {
    pub name: Spanned<ImmutableString>,
    pub event: Ident,
    pub arguments: CallArguments,
    pub conditions: Conditions,
    pub actions: Actions,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Block {
    pub actions: Actions,
    pub conditions: Conditions,
    pub span: Span,
}

/// Multiple [CallChain]s form arguments.
/// ## Example
/// - (Team.All, Hero.Ana)
/// - (player, 100)
pub type CallArguments = Spanned<Vec<CallChain>>;

/// Multiple idents form a call chain.
///
/// ## Example
/// - Team.All
/// - Hello.World
/// - player.heal()
pub type CallChain = Spanned<Vec<Box<Call>>>;

impl From<Box<Call>> for CallChain {
    fn from(value: Box<Call>) -> Self {
        let span = match *value {
            Call::Ident(ref ident) => ident.span.clone(),
            Call::String(_, ref span) => span.clone(),
            Call::Number(_, ref span) => span.clone(),
            Call::IdentArguments { ref span, .. } => span.clone(),
        };
        Spanned {
            value: vec![value],
            span,
        }
    }
}

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
    /// [CallArguments] might be recursive, so [Call] must be used behind a pointer.
    /// ## Example
    /// - heal(player, 100)
    /// - display(player, message)
    IdentArguments {
        name: Ident,
        args: CallArguments,
        span: Span,
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
    String(ImmutableString, Span),
    /// A number literal
    /// ## Example
    /// - 12
    /// - 1.5
    /// - 0
    Number(ImmutableString, Span),
}

impl From<Ident> for Box<Call> {
    fn from(value: Ident) -> Self {
        Box::new(Call::Ident(value))
    }
}
