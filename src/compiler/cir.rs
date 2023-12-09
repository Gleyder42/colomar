use crate::compiler::span::{CopyRange, Span, Spanned, SpannedBool};
use crate::compiler::{AssignMod, UseRestriction};
use crate::compiler::{
    Ident, Text, CALLED_ARGS_LEN, CONDITIONS_LEN, DECL_ARGS_LEN, ENUM_CONSTANTS_LEN,
    FUNCTIONS_DECLS_LEN, PROPERTY_DECLS_LEN,
};
use colomar_macros::Interned;
use hashlink::LinkedHashSet;
use smallvec::SmallVec;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

pub type DeclArgIds = SmallVec<[DeclArgId; DECL_ARGS_LEN]>;
pub type FunctionDeclIds = SmallVec<[FunctionDeclId; FUNCTIONS_DECLS_LEN]>;
pub type PropertyDecls = SmallVec<[PropertyDecl; PROPERTY_DECLS_LEN]>;
pub type PropertyDeclIds = SmallVec<[PropertyDeclId; PROPERTY_DECLS_LEN]>;
pub type EnumConstantIds = SmallVec<[EnumConstantId; ENUM_CONSTANTS_LEN]>;
pub type CalledArgs = SmallVec<[CalledArg; CALLED_ARGS_LEN]>;
pub type CalledArgIds = SmallVec<[CalledArgId; CALLED_ARGS_LEN]>;
pub type Predicates = SmallVec<[Predicate; CONDITIONS_LEN]>;

pub type EnumConstants = SmallVec<[EnumConstant; ENUM_CONSTANTS_LEN]>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cir(pub Vec<Root>);

impl FromIterator<Root> for Cir {
    fn from_iter<T: IntoIterator<Item = Root>>(iter: T) -> Self {
        let mut vec = Vec::new();

        for x in iter {
            vec.push(x);
        }

        Cir(vec)
    }
}

impl IntoIterator for Cir {
    type Item = Root;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Root {
    Rule(Rule),
    Enum(Enum),
    Event(Event),
    Struct(Struct),
}

impl Root {
    pub fn name(&self) -> &'static str {
        match self {
            Root::Event(_) => "Event",
            Root::Enum(_) => "Enum",
            Root::Rule(_) => "Rule",
            Root::Struct(_) => "Struct",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Interned)]
pub struct FunctionDecl {
    pub instance: Option<Type>,
    pub is_native: SpannedBool,
    pub name: Ident,
    pub args: DeclArgIds,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Interned)]
pub struct PropertyDecl {
    pub instance: Option<Type>,
    pub is_native: SpannedBool,
    pub name: Ident,
    pub desc: Spanned<UseRestriction>,
    pub r#type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub decl: StructDeclId,
    pub def: StructDef,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Interned)]
pub struct StructDecl {
    pub is_open: SpannedBool,
    pub is_native: SpannedBool,
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub functions: FunctionDeclIds,
    pub properties: PropertyDeclIds,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Interned)]
pub struct EnumDecl {
    pub name: Ident,
    pub is_native: SpannedBool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    pub constants: EnumConstantIds,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Interned)]
pub struct EnumConstant {
    pub name: Ident,
    pub r#enum: EnumDeclId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
    pub decl: EnumDeclId,
    pub def: EnumDef,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Enum(EnumDeclId),
    Struct(StructDeclId),
    Event(EventDeclId),
    Unit,
}

impl From<StructDeclId> for Type {
    fn from(value: StructDeclId) -> Self {
        Type::Struct(value)
    }
}

impl From<EventDeclId> for Type {
    fn from(value: EventDeclId) -> Self {
        Type::Event(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Predicate(pub Expr);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CalledType {
    pub r#type: Type,
    pub span: Span,
}

pub trait TypeComparison<Rhs> {
    fn has_same_return_type(&self, rhs: &Rhs) -> bool;
}

impl TypeComparison<StructDeclId> for CalledType {
    fn has_same_return_type(&self, rhs: &StructDeclId) -> bool {
        match self.r#type {
            Type::Struct(r#struct) => r#struct == *rhs,
            _ => false,
        }
    }
}

impl TypeComparison<CalledType> for CalledType {
    fn has_same_return_type(&self, rhs: &CalledType) -> bool {
        self.r#type == rhs.r#type
    }
}

impl PartialEq<StructDeclId> for CalledType {
    fn eq(&self, other: &StructDeclId) -> bool {
        match self.r#type {
            Type::Enum(_) => false,
            Type::Struct(id) => id == *other,
            Type::Event(_) => false,
            Type::Unit => false,
        }
    }
}

impl Display for CalledType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.r#type)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Enum(_) => write!(f, "Enum"),
            Type::Struct(_) => write!(f, "Struct"),
            Type::Event(_) => write!(f, "Event"),
            Type::Unit => write!(f, "Unit"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CalledTypes {
    pub types: LinkedHashSet<CalledType>,
    pub span: Span,
}

impl From<CalledType> for CalledTypes {
    fn from(value: CalledType) -> Self {
        let span = value.span;

        CalledTypes {
            types: LinkedHashSet::from_iter(vec![value]),
            span,
        }
    }
}

impl CalledTypes {
    pub fn contains_type(&self, r#type: &Type) -> bool {
        self.types.iter().any(|it| it.r#type == *r#type)
    }
}

impl Display for CalledTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let output = self
            .types
            .iter()
            .map(|it| it.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}", output)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Interned)]
pub struct CalledArg {
    /// The [DeclArgId] has not necessarily the same type as the value.
    /// It may be therefore not the 'correct' declared argument, rather the argument which was
    /// inputted by the user
    pub declared: DeclArgId,
    pub value: AValueChain,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Interned)]
pub struct DeclArg {
    pub name: Ident,
    pub position: usize,
    pub types: CalledTypes,
    pub default_value: Option<AValueChain>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Interned)]
pub struct EventDecl {
    pub name: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Event {
    pub decl: EventDeclId,
    pub def: EventDef,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EventDef {
    pub args: DeclArgIds,
    pub properties: PropertyDecls,
}

pub type Actions = Vec<Action>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rule {
    pub title: Text,
    pub event: EventDeclId,
    pub args: CalledArgs,
    pub conditions: Predicates,
    pub actions: Actions,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Action {
    Expr(Expr),
    AvalueChain(AValueChain),
    Assigment(AValueChain, AValueChain, Option<AssignMod>),
}

impl Action {
    pub fn ghost_span(&self) -> Span {
        match self {
            Action::Expr(expr) => expr.span(),
            Action::AvalueChain(avalue_chain) => avalue_chain.ghost_span(),
            Action::Assigment(left, ..) => left.ghost_span(),
        }
    }
}

impl From<Predicate> for Action {
    fn from(value: Predicate) -> Self {
        Action::Expr(value.0)
    }
}

impl From<Expr> for Action {
    fn from(value: Expr) -> Self {
        Action::Expr(value)
    }
}

impl From<AValueChain> for Action {
    fn from(value: AValueChain) -> Self {
        Action::AvalueChain(value)
    }
}

pub type Expr = crate::compiler::Expr<AValueChain>;

#[derive(Debug)]
pub struct ExprReturnValueError(pub AValue, pub AValue);

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Chain(chain) => chain.span,
            // TODO Use correct span here
            Expr::Neg(neg) => neg.span(),
            // TODO Use correct span here
            Expr::And(_lhs, rhs) | Expr::Or(_lhs, rhs) => rhs.span(),
        }
    }
}

/// Represents a value which is known at runtime time or compile time and it refers
/// to some other code
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum RValue {
    Type(Type),
    Function(FunctionDeclId),
    Property(PropertyDeclId),
    EnumConstant(EnumConstantId),
}

impl<T: Into<Type>> From<T> for RValue {
    fn from(value: T) -> Self {
        RValue::Type(value.into())
    }
}

// TODO Rename to AvalueChain
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AValueChain {
    pub avalues: Vec<AValue>,
    pub span: Span,
}

impl AValueChain {
    pub fn is_constant(&self) -> bool {
        self.avalues.iter().all(|avalue| avalue.is_constant())
    }

    pub fn new(avalues: Vec<AValue>, span: Span) -> Self {
        debug_assert!(!avalues.is_empty(), "Tried to create an empty AValueChain");
        AValueChain { avalues, span }
    }

    /// The ghost span starts and ends just before the first avalue inside the span.
    /// It is used when the [AValueChain] has an implicit caller.
    pub fn ghost_span(&self) -> Span {
        let start = self.span.location.start();
        let end = self.span.location.start() + 1;
        Span::new(self.span.source, CopyRange::from(start..end))
    }

    pub fn returning_avalue(&self) -> AValue {
        self.avalues.last().unwrap().clone()
    }
}

/// Represents a value which is known at runtime time or compile time
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AValue {
    FunctionCall(FunctionDeclId, CalledArgIds, Span),
    RValue(RValue, Span),
    CValue(CValue),
}

impl From<AValue> for AValueChain {
    fn from(value: AValue) -> Self {
        let span = value.span();
        AValueChain::new(vec![value], span)
    }
}

impl AValue {
    pub fn is_constant(&self) -> bool {
        match self {
            AValue::FunctionCall(_, _, _) => false,
            AValue::RValue(_, _) => false,
            AValue::CValue(_) => true,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            AValue::FunctionCall(_, _, span) => *span,
            AValue::RValue(_, span) => *span,
            AValue::CValue(cvalue) => cvalue.span(),
        }
    }
}

/// Represent a value which is known at compile time
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CValue {
    String(Text, StructDeclId, Span),
    Number(Text, StructDeclId, Span),
}

impl CValue {
    fn struct_decl_id(&self) -> StructDeclId {
        match self {
            CValue::String(_, id, _) => *id,
            CValue::Number(_, id, _) => *id,
        }
    }
}

impl TypeComparison<CValue> for CValue {
    fn has_same_return_type(&self, rhs: &CValue) -> bool {
        self.struct_decl_id() == rhs.struct_decl_id()
    }
}

impl CValue {
    pub fn span(&self) -> Span {
        match self {
            CValue::String(_, _, span) | CValue::Number(_, _, span) => *span,
        }
    }

    pub fn r#type(&self) -> Type {
        match self {
            CValue::String(_, struct_decl_id, _) => Type::Struct(*struct_decl_id),
            CValue::Number(_, struct_decl_id, _) => Type::Struct(*struct_decl_id),
        }
    }
}
