use crate::compiler::span::{Span, Spanned, SpannedBool};
use crate::compiler::trisult::Trisult;
use crate::compiler::{
    AssignMod, Ident, Text, UseRestriction, ACTIONS_LEN, CONDITIONS_LEN, DECL_ARGS_LEN,
    FUNCTIONS_DECLS_LEN, PROPERTY_DECLS_LEN,
};
use smallvec::SmallVec;

pub type Condition = Expr;
pub type Conditions = SmallVec<[Condition; CONDITIONS_LEN]>;
pub type Actions = SmallVec<[Action; ACTIONS_LEN]>;
pub type DeclArgs = SmallVec<[DeclArg; DECL_ARGS_LEN]>;
pub type PropertyDecls = SmallVec<[PropertyDecl; PROPERTY_DECLS_LEN]>;
pub type FunctionDecls = SmallVec<[FunctionDecl; FUNCTIONS_DECLS_LEN]>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Def {
    Event(EventDef),
    Enum(EnumDef),
    Struct(StructDef),
}

impl Def {
    pub fn expect_struct(self, message: &'static str) -> StructDef {
        match self {
            Def::Struct(def) => def,
            Def::Enum(_) | Def::Event(_) => panic!("{message}"),
        }
    }
}

impl From<EventDef> for Def {
    fn from(value: EventDef) -> Self {
        Def::Event(value)
    }
}

impl From<EnumDef> for Def {
    fn from(value: EnumDef) -> Self {
        Def::Enum(value)
    }
}

impl From<StructDef> for Def {
    fn from(value: StructDef) -> Self {
        Def::Struct(value)
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
    Import(Import),
}

impl Root {
    pub fn visibility(&self) -> Visibility {
        match self {
            Root::Event(event) => event.decl.visibility,
            Root::Rule(rule) => rule.visibility,
            Root::Enum(r#enum) => r#enum.decl.visibility,
            Root::Struct(r#struct) => r#struct.decl.visibility,
            Root::Import(_) => Visibility::Private, // imports should not appear as public
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum TypeRoot {
    Event(Event),
    Enum(Enum),
    Struct(Struct),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Action {
    CallChain(CallChain),
    Assignment(CallChain, CallChain, Option<AssignMod>),
    Property(PropertyDecl),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Path {
    pub segments: Vec<Text>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Import {
    pub path: Path,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Type {
    pub ident: Ident,
    pub generics: Vec<BoundGeneric>,
}

impl Type {}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Types {
    pub values: SmallVec<[Type; 2]>,
    pub span: Span,
}

impl IntoIterator for Types {
    type Item = Type;
    type IntoIter = smallvec::IntoIter<[Type; 2]>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct EventDecl {
    pub visibility: Visibility,
    pub is_native: SpannedBool,
    pub name: Ident,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct EventDef {
    pub by: Option<(Ident, CallArgs)>,
    pub args: Spanned<DeclArgs>,
    pub conditions: Conditions,
    pub actions: Actions,
}

impl TryFrom<Def> for EventDef {
    type Error = &'static str;

    fn try_from(value: Def) -> Result<Self, Self::Error> {
        match value {
            Def::Event(event) => Ok(event),
            Def::Enum(_) => Err("Cannot convert enum to event definition"),
            Def::Struct(_) => Err("Cannot convert struct to event definition"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Event {
    pub decl: EventDecl,
    pub def: EventDef,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct PropertyDecl {
    pub is_native: SpannedBool,
    pub use_restriction: Spanned<UseRestriction>,
    pub name: Ident,
    pub r#type: Type,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct FunctionDecl {
    pub is_native: SpannedBool,
    pub name: Ident,
    pub args: Spanned<DeclArgs>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct StructDecl {
    pub visibility: Visibility,
    pub is_partial: SpannedBool,
    pub is_native: SpannedBool,
    pub name: Ident,
    pub span: Span,
    pub generics: DeclGenerics,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct DeclGenerics(pub Vec<Ident>);

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct BoundGeneric {
    pub ident: Ident,
    pub bound_generics: Vec<BoundGeneric>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct StructDef {
    pub properties: PropertyDecls,
    pub functions: FunctionDecls,
}

impl TryFrom<Def> for StructDef {
    type Error = &'static str;

    fn try_from(value: Def) -> Result<Self, Self::Error> {
        match value {
            Def::Event(_) => Err("Cannot convert event to struct definition"),
            Def::Enum(_) => Err("Cannot convert enum to struct definition"),
            Def::Struct(r#struct) => Ok(r#struct),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Default)]
pub enum Visibility {
    Public,
    #[default]
    Private,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Struct {
    pub decl: StructDecl,
    pub def: StructDef,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct EnumDecl {
    pub visibility: Visibility,
    pub is_native: SpannedBool,
    pub name: Ident,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct EnumDef {
    pub constants: Vec<Ident>,
}

impl TryFrom<Def> for EnumDef {
    type Error = &'static str;

    fn try_from(value: Def) -> Result<Self, Self::Error> {
        match value {
            Def::Event(_) => Err("Cannot convert event to enum definition"),
            Def::Enum(r#enum) => Ok(r#enum),
            Def::Struct(_) => Err("Cannot convert struct to enum definition"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Enum {
    pub decl: EnumDecl,
    pub def: EnumDef,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct DeclArg {
    pub position: usize,
    pub name: Ident,
    pub types: Types,
    pub default_value: Option<CallChain>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Rule {
    pub visibility: Visibility,
    pub name: Spanned<Text>,
    pub event: Ident,
    pub args: CallArgs,
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
pub type CallArgs = Spanned<Vec<CallArg>>;

/// A call argument is either identified by name or position.
///
/// ### By name
///  - (named = "Hello World", amount = 10)
///
/// ### By position
///  - (1, 2, 3)
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum CallArg {
    Named(Ident, CallChain, Span),
    Pos(CallChain),
}

pub type Expr = crate::compiler::Expr<CallChain>;

impl CallArg {
    pub fn call_chain(self) -> CallChain {
        match self {
            CallArg::Named(_, call_chain, _) | CallArg::Pos(call_chain) => call_chain,
        }
    }
}

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
            Call::Ident(ref ident) => ident.span,
            Call::String(_, ref span) => *span,
            Call::Number(_, ref span) => *span,
            Call::IdentArgs { ref span, .. } => *span,
        };
        Spanned {
            value: vec![value],
            span,
        }
    }
}

/// Represent a single ident with the intention to use it as a call.
/// A call here means like referencing a variable or function.
/// A call us usually not alone and is mostly user within [CallChain] and [CallArgs]
/// ## Example
/// - Team
/// - All
/// - hello
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Call {
    // TODO Use doctests here to ensure valid examples
    /// An ident followed by call arguments.
    /// [CallArgs] might be recursive, so [Call] must be used behind a pointer.
    /// ## Example
    /// - heal(player, 100)
    /// - display(player, message)
    /// - heal(player, message = 100)
    IdentArgs {
        name: Ident,
        args: CallArgs,
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
    String(Text, Span),
    /// A number literal
    /// ## Example
    /// - 12
    /// - 1.5
    /// - 0
    Number(Text, Span),
}

impl From<Ident> for Box<Call> {
    fn from(value: Ident) -> Self {
        Box::new(Call::Ident(value))
    }
}

impl<E> From<CallChain> for Trisult<CallChain, E> {
    fn from(value: CallChain) -> Self {
        Trisult::Ok(value)
    }
}
