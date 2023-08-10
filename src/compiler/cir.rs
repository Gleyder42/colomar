use crate::compiler::span::{SimpleSpanLocation, Span, Spanned, SpannedBool};
use crate::compiler::UseRestriction;
use crate::compiler::{
    Ident, Text, CALLED_ARGUMENTS_LEN, CONDITIONS_LEN, DECLARED_ARGUMENTS_LEN, ENUM_CONSTANTS_LEN,
    FUNCTIONS_DECLS_LEN, PROPERTY_DECLS_LEN,
};
use crate::impl_intern_key;
use smallvec::SmallVec;
use std::fmt::{Debug, Display, Formatter};

pub type DeclaredArgumentIds = SmallVec<[DeclaredArgumentId; DECLARED_ARGUMENTS_LEN]>;
pub type FunctionDeclIds = SmallVec<[FunctionDeclId; FUNCTIONS_DECLS_LEN]>;
pub type PropertyDecls = SmallVec<[PropertyDecl; PROPERTY_DECLS_LEN]>;
pub type PropertyDeclIds = SmallVec<[PropertyDeclId; PROPERTY_DECLS_LEN]>;
pub type EnumConstantIds = SmallVec<[EnumConstantId; ENUM_CONSTANTS_LEN]>;
pub type CalledArguments = SmallVec<[CalledArgument; CALLED_ARGUMENTS_LEN]>;
pub type CalledArgumentIds = SmallVec<[CalledArgumentId; CALLED_ARGUMENTS_LEN]>;
pub type Predicates = SmallVec<[Predicate; CONDITIONS_LEN]>;

pub type EnumConstants = SmallVec<[EnumConstant; ENUM_CONSTANTS_LEN]>;

pub type Actions = Vec<AValueChain>;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDecl {
    pub instance: Option<Type>,
    pub is_native: SpannedBool,
    pub name: Ident,
    pub arguments: DeclaredArgumentIds,
    pub return_type: Type,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionDeclId(salsa::InternId);

impl_intern_key!(FunctionDeclId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PropertyDecl {
    pub instance: Option<Type>,
    pub is_native: SpannedBool,
    pub name: Ident,
    pub desc: Spanned<UseRestriction>,
    pub r#type: Type,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct PropertyDeclId(salsa::InternId);

impl_intern_key!(PropertyDeclId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub decl: StructDeclarationId,
    pub def: StructDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDeclaration {
    pub is_open: SpannedBool,
    pub is_native: SpannedBool,
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDefinition {
    pub functions: FunctionDeclIds,
    pub properties: PropertyDeclIds,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructDeclarationId(salsa::InternId);

impl_intern_key!(StructDeclarationId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumDeclaration {
    pub name: Ident,
    pub is_native: SpannedBool,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumDeclarationId(salsa::InternId);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumConstantId(salsa::InternId);

impl_intern_key!(EnumDeclarationId);

impl_intern_key!(EnumConstantId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDefinition {
    // TODO Maybe use a hashset or hashmap to show that names are unique
    // TODO hashsets have no hash value (?)
    pub constants: EnumConstantIds,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumConstant {
    pub name: Ident,
    pub r#enum: EnumDeclarationId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
    pub declaration: EnumDeclarationId,
    pub definition: EnumDefinition,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Enum(EnumDeclarationId),
    Struct(StructDeclarationId),
    Event(EventDeclarationId),
    Unit,
}

impl From<StructDeclarationId> for Type {
    fn from(value: StructDeclarationId) -> Self {
        Type::Struct(value)
    }
}

impl From<EventDeclarationId> for Type {
    fn from(value: EventDeclarationId) -> Self {
        Type::Event(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Predicate(pub AValueChain);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CalledType {
    pub r#type: Type,
    pub span: Span,
}

impl Display for CalledType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.r#type.to_string())
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
    // TODO Use small_vec here
    pub types: Vec<CalledType>,
    pub span: Span,
}

impl From<CalledType> for CalledTypes {
    fn from(value: CalledType) -> Self {
        let span = value.span;

        CalledTypes {
            types: vec![value],
            span,
        }
    }
}

impl CalledTypes {
    // TODO This should not be O(n)
    // TODO Either make it a map or sort the vec before
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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct CalledArgumentId(salsa::InternId);

impl_intern_key!(CalledArgumentId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CalledArgument {
    /// The [DeclaredArgumentId] has not necessarily the same type as the value.
    /// It may be therefore not the 'correct' declared argument, rather the argument which was
    /// inputted by the user
    pub declared: DeclaredArgumentId,
    pub value: AValueChain,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub position: usize,
    pub types: CalledTypes,
    pub default_value: Option<AValueChain>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct DeclaredArgumentId(salsa::InternId);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct EventDeclarationId(salsa::InternId);

impl_intern_key!(DeclaredArgumentId);
impl_intern_key!(EventDeclarationId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EventDeclaration {
    pub name: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Event {
    pub declaration: EventDeclarationId,
    pub definition: EventDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EventDefinition {
    pub arguments: DeclaredArgumentIds,
    pub properties: PropertyDecls,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rule {
    pub title: Text,
    pub event: EventDeclarationId,
    pub arguments: CalledArguments,
    pub conditions: Predicates,
    pub actions: Actions,
}

/// Represents a value which is known at runtime time or compile time and it refers
/// to some other code
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RValue {
    Type(Type),
    Function(FunctionDecl),
    Property(PropertyDecl),
    EnumConstant(EnumConstantId),
}

impl<T: Into<Type>> From<T> for RValue {
    fn from(value: T) -> Self {
        RValue::Type(value.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AValueChain {
    pub avalues: Vec<AValue>,
    pub span: Span,
}

impl AValueChain {
    pub fn new(avalues: Vec<AValue>, span: Span) -> Self {
        debug_assert!(!avalues.is_empty(), "Tried to create an empty AValueChain");
        AValueChain { avalues, span }
    }

    /// The ghost span starts and ends just before the first avalue inside the span.
    /// It is used when the [AValueChain] has an implicit caller.
    pub fn ghost_span(&self) -> Span {
        let start = self.span.location.start();
        let end = self.span.location.start() + 1;
        Span::new(self.span.source, SimpleSpanLocation::from(start..end))
    }

    pub fn returning_avalue(&self) -> AValue {
        self.avalues.last().unwrap().clone()
    }
}

/// Represents a value which is known at runtime time or compile time
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AValue {
    // TODO Should this be FunctionDeclId or FunctionDecl?
    FunctionCall(FunctionDeclId, CalledArgumentIds, Span),
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
    String(Text, StructDeclarationId, Span),
    Number(Text, StructDeclarationId, Span),
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
