use super::analysis::interner::Interner;
use super::span::{CopyRange, Span, Spanned, SpannedBool, StringInterner};
use super::{AssignMod, UseRestriction, DECL_GENERICS_LEN};
use super::{
    Ident, TextId, CALLED_ARGS_LEN, CONDITIONS_LEN, DECL_ARGS_LEN, ENUM_CONSTANTS_LEN,
    FUNCTIONS_DECLS_LEN, PROPERTY_DECLS_LEN,
};
use colomar_macros::Interned;
use hashlink::{LinkedHashMap, LinkedHashSet};
use smallvec::SmallVec;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

pub type DeclGenerics = SmallVec<[Ident; DECL_GENERICS_LEN]>;
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
    pub instance: Option<TypeDesc>,
    pub is_native: SpannedBool,
    pub name: Ident,
    pub args: DeclArgIds,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Interned)]
pub struct PropertyDecl {
    pub instance: Option<TypeDesc>,
    pub is_native: SpannedBool,
    pub name: Ident,
    pub desc: Spanned<UseRestriction>,
    pub r#type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub decl: SmallVec<[StructDeclId; 1]>,
    pub def: SmallVec<[StructDef; 1]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Interned)]
pub struct StructDecl {
    pub is_partial: SpannedBool,
    pub is_native: SpannedBool,
    pub name: Ident,
    pub generics: DeclGenerics,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoundGeneric {
    pub r#type: TypeDesc,
    pub generics: Vec<BoundGeneric>,
}

// TODO Virtual type is not the best description
// Rename Type to TypeDesc
// Rename VirtualType to Type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub desc: TypeDesc,
    pub generics: Vec<BoundGeneric>,
}

impl PartialEq<StructDeclId> for CalledType {
    // TODO this functions should not exist in its current state
    fn eq(&self, other: &StructDeclId) -> bool {
        let r#type = match &self.r#type {
            VirtualTypeKind::Type(it) => it,
            VirtualTypeKind::Generic(_) => return false,
        };

        if !r#type.generics.is_empty() {
            return false;
        }

        // TODO how to compare generic structs
        match r#type.desc {
            TypeDesc::Enum(_) => false,
            TypeDesc::Struct(id) => id == *other,
            TypeDesc::Event(_) => false,
            TypeDesc::Unit => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VirtualTypeKind {
    Type(Type),
    Generic(Ident),
}

impl From<TypeDesc> for VirtualTypeKind {
    fn from(value: TypeDesc) -> Self {
        VirtualTypeKind::Type(Type::from(value))
    }
}

impl From<Type> for VirtualTypeKind {
    fn from(value: Type) -> Self {
        VirtualTypeKind::Type(value)
    }
}

impl VirtualTypeKind {
    /// Checks if the virtual type has generics, but of if the type itself is generic
    pub fn has_generics(&self) -> bool {
        match self {
            VirtualTypeKind::Type(r#type) => !r#type.generics.is_empty(),
            VirtualTypeKind::Generic(_) => false,
        }
    }

    // TODO Put method name maybe in a trait?
    pub fn name(&self, db: &(impl Interner + StringInterner + ?Sized)) -> String {
        match self {
            VirtualTypeKind::Type(r#type) => r#type.name(db),
            VirtualTypeKind::Generic(ident) => ident.value.name(db),
        }
    }
}

impl From<TypeDesc> for Type {
    fn from(value: TypeDesc) -> Self {
        Type {
            desc: value,
            generics: Vec::new(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TypeDesc {
    Enum(EnumDeclId),
    Struct(StructDeclId),
    Event(EventDeclId),
    Unit,
}

impl TypeDesc {
    pub fn is_partial(&self, db: &(impl Interner + ?Sized)) -> Option<bool> {
        match self {
            TypeDesc::Enum(_) => None,
            TypeDesc::Struct(r#struct) => {
                Some(db.lookup_intern_struct_decl(*r#struct).is_partial.is_some())
            }
            TypeDesc::Event(_) => None,
            TypeDesc::Unit => None,
        }
    }
}

impl From<StructDeclId> for TypeDesc {
    fn from(value: StructDeclId) -> Self {
        TypeDesc::Struct(value)
    }
}

impl From<EventDeclId> for TypeDesc {
    fn from(value: EventDeclId) -> Self {
        TypeDesc::Event(value)
    }
}

impl Display for TypeDesc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDesc::Enum(_) => write!(f, "Enum"),
            TypeDesc::Struct(_) => write!(f, "Struct"),
            TypeDesc::Event(_) => write!(f, "Event"),
            TypeDesc::Unit => write!(f, "Unit"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Predicate(pub Expr);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CalledType {
    pub r#type: VirtualTypeKind,
    pub span: Span,
}

impl TypeComparison<CalledType> for CalledType {
    fn has_same_return_type(&self, rhs: &CalledType) -> bool {
        self.r#type == rhs.r#type
    }
}

pub trait TypeComparison<Rhs> {
    fn has_same_return_type(&self, rhs: &Rhs) -> bool;
}

impl Display for CalledType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO Write a proper display
        write!(f, "{:?}", self.r#type)
    }
}

impl TypeComparison<StructDeclId> for CalledType {
    fn has_same_return_type(&self, rhs: &StructDeclId) -> bool {
        let r#type = match &self.r#type {
            VirtualTypeKind::Type(r#type) => r#type,
            VirtualTypeKind::Generic(_) => return false,
        };

        match r#type.desc {
            TypeDesc::Struct(r#struct) if r#type.generics.is_empty() => r#struct == *rhs,
            _ => false,
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

pub type GenericTypeBoundMap = LinkedHashMap<TextId, Type>;

impl CalledTypes {
    pub fn contains_type(&self, r#type: &Type, bound_map: &GenericTypeBoundMap) -> bool {
        self.types
            .iter()
            .any(|called_type| match &called_type.r#type {
                VirtualTypeKind::Type(other_type) => r#type == other_type,
                VirtualTypeKind::Generic(ident) => bound_map
                    .get(&ident.value)
                    .map(|it| it == r#type)
                    .unwrap_or(false),
            })
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
    pub is_vararg: bool,
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
    pub title: TextId,
    pub event: EventDeclId,
    pub args: CalledArgs,
    pub conditions: Predicates,
    pub actions: Actions,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Action {
    Expr(Expr),
    AValueChain(AValueChain),
    Assigment(AValueChain, AValueChain, Option<AssignMod>),
}

impl Action {
    pub fn ghost_span(&self) -> Span {
        match self {
            Action::Expr(expr) => expr.span(),
            Action::AValueChain(avalue_chain) => avalue_chain.ghost_span(),
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
        Action::AValueChain(value)
    }
}

pub type Expr = super::Expr<AValueChain>;

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
    Type(TypeDesc),
    Function(FunctionDeclId),
    Property(PropertyDeclId),
    EnumConstant(EnumConstantId),
}

impl<T: Into<TypeDesc>> From<T> for RValue {
    fn from(value: T) -> Self {
        RValue::Type(value.into())
    }
}

/// Represents a value which is known at runtime time or compile time
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AValue {
    FunctionCall(FunctionDeclId, CalledArgIds, Span),
    RValue(RValue, Span),
    CValue(CValue),
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
        let start = self.span.offset.start();
        let end = self.span.offset.start() + 1;
        Span::new(self.span.context, CopyRange::from(start..end))
    }

    pub fn returning_avalue(&self) -> AValue {
        self.avalues.last().unwrap().clone()
    }
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
    String(TextId, StructDeclId, Span),
    Number(TextId, StructDeclId, Span),
}

impl CValue {
    fn struct_decl_id(&self) -> StructDeclId {
        match self {
            CValue::String(_, id, _) => *id,
            CValue::Number(_, id, _) => *id,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            CValue::String(_, _, span) | CValue::Number(_, _, span) => *span,
        }
    }

    pub fn r#type(&self) -> TypeDesc {
        match self {
            CValue::String(_, struct_decl_id, _) => TypeDesc::Struct(*struct_decl_id),
            CValue::Number(_, struct_decl_id, _) => TypeDesc::Struct(*struct_decl_id),
        }
    }
}

impl TypeComparison<CValue> for CValue {
    fn has_same_return_type(&self, rhs: &CValue) -> bool {
        self.struct_decl_id() == rhs.struct_decl_id()
    }
}
