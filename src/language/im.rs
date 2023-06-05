use crate::impl_intern_key;
use crate::language::analysis::interner::{Interner, IntoInternId};
use crate::language::analysis::namespace::{EnumNameholder, Nameholder};
use crate::language::ast::{SpannedBool, UseRestriction};
use crate::language::{Ident, ImmutableString, Span, Spanned};
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Im(pub Vec<Root>);

impl FromIterator<Root> for Im {
    fn from_iter<T: IntoIterator<Item = Root>>(iter: T) -> Self {
        let mut vec = Vec::new();

        for x in iter {
            vec.push(x);
        }

        Im(vec)
    }
}

impl IntoIterator for Im {
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
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub arguments: Vec<DeclaredArgumentId>,
    pub return_type: Type,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionDeclId(salsa::InternId);

impl_intern_key!(FunctionDeclId);

impl IntoInternId for FunctionDecl {
    type Interned = FunctionDeclId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> Self::Interned {
        db.intern_function_decl(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PropertyDecl {
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub desc: Spanned<UseRestriction>,
    pub r#type: Type,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct PropertyDeclId(salsa::InternId);

impl_intern_key!(PropertyDeclId);

impl IntoInternId for PropertyDecl {
    type Interned = PropertyDeclId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> Self::Interned {
        db.intern_property_decl(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    pub decl: StructDeclarationId,
    pub def: StructDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDeclaration {
    pub is_open: SpannedBool,
    pub is_workshop: SpannedBool,
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDefinition {
    pub functions: Vec<FunctionDeclId>,
    pub properties: Vec<PropertyDeclId>,
}

impl IntoInternId for StructDeclaration {
    type Interned = StructDeclarationId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> StructDeclarationId {
        db.intern_struct_decl(self)
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructDeclarationId(salsa::InternId);

impl_intern_key!(StructDeclarationId);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumDeclaration {
    pub name: Ident,
    pub is_workshop: SpannedBool,
}

impl IntoInternId for EnumDeclaration {
    type Interned = EnumDeclarationId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> EnumDeclarationId {
        db.intern_enum_decl(self)
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumDeclarationId(salsa::InternId);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumConstantId(salsa::InternId);

impl IntoInternId for EnumConstant {
    type Interned = EnumConstantId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> EnumConstantId {
        db.intern_enum_constant(self)
    }
}
impl_intern_key!(EnumDeclarationId);

impl_intern_key!(EnumConstantId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDefinition {
    // TODO Maybe use a hashset or hashmap to show that names are unique
    pub constants: Vec<EnumConstantId>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Enum(EnumDeclarationId),
    Struct(StructDeclarationId),
    Event(EventDeclarationId),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Predicate {
    pub return_value: AValue,
}

impl From<Type> for Nameholder {
    fn from(value: Type) -> Self {
        match value {
            Type::Enum(r#enum) => Nameholder::Enum(EnumNameholder::ByEnum(r#enum)),
            Type::Struct(r#struct) => Nameholder::Struct(r#struct),
            Type::Event(event) => Nameholder::Event(event),
            Type::Unit => Nameholder::Empty,
        }
    }
}

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
    pub types: Vec<CalledType>,
    pub span: Span,
}

impl CalledTypes {
    // TODO This should not be O(n)
    // Either make it a map or sort the vec before
    pub fn contains_type(&self, r#type: Type) -> bool {
        self.types.iter().any(|it| it.r#type == r#type)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CalledArgument {
    pub declared: DeclaredArgumentId,
    pub value: AValue,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub types: CalledTypes,
    pub default_value: Option<AValue>,
}

impl IntoInternId for DeclaredArgument {
    type Interned = DeclaredArgumentId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> DeclaredArgumentId {
        db.intern_decl_arg(self)
    }
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

impl IntoInternId for EventDeclaration {
    type Interned = EventDeclarationId;

    fn intern<T: Interner + ?Sized>(self, db: &T) -> EventDeclarationId {
        db.intern_event_decl(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Event {
    pub declaration: EventDeclarationId,
    pub definition: EventDefinition,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EventDefinition {
    pub arguments: Vec<DeclaredArgumentId>,
    pub properties: Vec<PropertyDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rule {
    pub title: ImmutableString,
    pub event: EventDeclarationId,
    pub arguments: Vec<CalledArgument>,
    pub conditions: Vec<Predicate>,
    pub actions: Vec<AValue>,
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

impl From<RValue> for Nameholder {
    fn from(value: RValue) -> Self {
        match value {
            RValue::Type(r#type) => r#type.into(),
            RValue::EnumConstant(enum_constant_id) => {
                EnumNameholder::ByConstant(enum_constant_id).into()
            }
            RValue::Property(property_decl) => property_decl.r#type.into(),
            RValue::Function(function_decl) => function_decl.return_type.into(),
        }
    }
}

impl RValue {
    pub fn r#type<I: Interner + ?Sized>(&self, db: &I) -> Type {
        match self {
            // What is the type of type?
            RValue::Type(r#type) => r#type.clone(),
            RValue::EnumConstant(enum_constant_id) => {
                let enum_constant: EnumConstant = db.lookup_intern_enum_constant(*enum_constant_id);
                Type::Enum(enum_constant.r#enum)
            }
            RValue::Property(property_decl) => property_decl.r#type.clone(),
            RValue::Function(function_decl) => function_decl.return_type.clone(),
        }
    }

    pub fn name<I: Interner + ?Sized>(&self, db: &I) -> Ident {
        // TODO Maybe not clone here?
        match self.clone() {
            RValue::Type(r#type) => match r#type {
                Type::Enum(r#enum) => db.lookup_intern_enum_decl(r#enum).name,
                Type::Struct(r#struct) => db.lookup_intern_struct_decl(r#struct).name,
                Type::Event(event) => db.lookup_intern_event_decl(event).name,
                Type::Unit => panic!("Unit type has no name"),
            },
            RValue::EnumConstant(enum_constant) => {
                db.lookup_intern_enum_constant(enum_constant).name
            }
            RValue::Property(property_decl) => property_decl.name,
            RValue::Function(function_decl) => function_decl.name,
        }
    }
}

/// Represents a value which is known at runtime time or compile time
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AValue {
    // TODO Should this be FunctionDeclId or FunctionDecl?
    FunctionCall(FunctionDeclId, Vec<AValue>),
    RValue(RValue, Span),
    CValue(CValue),
}

impl AValue {
    // TODO Rename this to return_type
    pub fn r#type<I: Interner + ?Sized>(&self, db: &I) -> Type {
        match self {
            AValue::RValue(rvalue, _) => rvalue.r#type(db),
            AValue::CValue(cvalue) => cvalue.r#type(),
            AValue::FunctionCall(function_decl_id, _) => {
                let function_decl: FunctionDecl = db.lookup_intern_function_decl(*function_decl_id);
                function_decl.return_type
            }
        }
    }
}

/// Represent a value which is known at compile time
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CValue {
    String(ImmutableString, StructDeclarationId, Span),
    Number(ImmutableString, StructDeclarationId, Span),
}

impl CValue {
    pub fn r#type(&self) -> Type {
        match self {
            CValue::String(_, struct_decl_id, _) => Type::Struct(*struct_decl_id),
            CValue::Number(_, struct_decl_id, _) => Type::Struct(*struct_decl_id),
        }
    }
}
