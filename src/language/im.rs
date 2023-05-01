use std::fmt::{Debug, Display, Formatter};
use std::rc::{Rc, Weak};
use derivative::Derivative;
use crate::language::ast::{SpannedBool, UseRestriction};
use crate::language::{Ident, ImmutableString, Span};

pub type RuleRef = Rc<RuleDeclaration>;
pub type EnumRef = Rc<EnumDeclaration>;
pub type EventRef = Rc<EventDeclaration>;
pub type DeclaredArgumentRef = Rc<DeclaredArgument>;
pub type FunctionRef = Rc<Function>;
pub type PropertyRef = Rc<Property>;
pub type StructRef = Rc<Struct>;
pub type EnumConstantRef = Rc<EnumConstant>;

#[derive(Debug, Clone)]
pub struct Im(pub Vec<Root>);

impl IntoIterator for Im {
    type Item = Root;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, Clone)]
pub enum Root {
    Rule(RuleRef),
    Enum(EnumRef),
    Event(EventRef),
    Struct(StructRef)
}

impl Root {
    pub fn name(&self) -> &'static str {
        match self {
            Root::Event(_) => "Event",
            Root::Enum(_) => "Enum",
            Root::Rule(_) => "Rule",
            Root::Struct(_) => "Struct"
        }
    }
}

#[derive(Derivative, Debug, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Function {
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub arguments: Vec<DeclaredArgumentRef>,
    pub return_type: Type
}

#[derive(Derivative, Debug, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Property {
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub desc: UseRestriction,
    pub r#type: Type
}

#[derive(Derivative, Debug, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Struct {
    pub is_open: SpannedBool,
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub functions: Vec<FunctionRef>,
    pub properties: Vec<PropertyRef>,
    pub span: Span
}

#[derive(Derivative, Debug, Clone)]
#[derivative(PartialEq, Eq)]
pub struct EnumDeclaration {
    pub name: Ident,
    pub is_workshop: SpannedBool,
}

#[derive(Derivative, Debug, Clone)]
#[derivative(PartialEq, Eq)]
pub struct EnumDefinition {
    pub constants: Vec<Rc<EnumConstant>>,
}

#[derive(Derivative, Debug, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Enum {
    pub declaration: Rc<EnumDeclaration>,
    pub definition: Rc<EnumDefinition>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

#[derive(Derivative, Debug)]
#[derivative(PartialEq, Eq)]
pub struct EnumConstant {
    pub name: Ident,

    #[derivative(PartialEq = "ignore")]
    pub r#enum: Rc<EnumDeclaration>
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Enum(EnumRef),
    Struct(StructRef),
    Event(EventRef)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CalledType {
    r#type: Type,
    span: Span
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
            Type::Event(_) => write!(f, "Event")
        }
    }
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub types: CalledTypes,
    pub default_value: Option<AValue>,
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct CalledTypes {
    pub types: Vec<CalledType>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

impl CalledTypes {

    pub fn contains_type(&self, r#type: Type) -> bool {
        self.types.iter().any(|it| it.r#type == r#type)
    }
}

impl Display for CalledTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let output = self.types.iter()
            .map(|it| it.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}", output)
    }
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct CalledArgument {
    pub declared: DeclaredArgumentRef,
    pub value: AValue,
}


#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct EventDeclaration {
    pub name: Ident,
    pub arguments: Vec<DeclaredArgumentRef>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

#[derive(Derivative, Debug, Eq)]
#[derivative(PartialEq)]
pub struct Rule {
    pub declaration: RuleDeclaration,
    pub definition: RuleDefinition
}

#[derive(Derivative, Debug, Eq)]
#[derivative(PartialEq)]
pub struct RuleDefinition {

}

#[derive(Derivative, Debug, Eq)]
#[derivative(PartialEq)]
pub struct RuleDeclaration {
    pub title: String,
    pub event: EventRef,
    pub arguments: Vec<CalledArgument>,
}

/// Represents a value which is only known during runtime
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RValue {
    Enum(EnumRef),
    Event(EventRef),
    Struct(StructRef),
    EnumConstant(Rc<EnumConstant>),
    Function(FunctionRef),
    Property(PropertyRef),
}

impl RValue {

    pub fn name(&self) -> Ident {
        match self {
            RValue::Struct(r#struct) => r#struct.name.clone(),
            RValue::Enum(r#enum) => r#enum.name.clone(),
            RValue::Event(event) => event.name.clone(),
            RValue::EnumConstant(enum_constant) => enum_constant.name.clone(),
            RValue::Function(function) => function.name.clone(),
            RValue::Property(property) => property.name.clone()
        }
    }

    pub fn r#type(&self) -> Type {
        match self {
            RValue::Enum(r#enum) => Type::Enum(Rc::clone(r#enum)),
            RValue::Event(event) => Type::Event(Rc::clone(event)),
            RValue::Struct(r#struct) => Type::Struct(Rc::clone(r#struct)),
            RValue::EnumConstant(enum_constant) => Type::Enum(Rc::clone(&enum_constant.r#enum)),
            RValue::Function(function) => function.return_type.clone(),
            RValue::Property(property) => property.r#type.clone()
        }
    }
}

impl Into<StructRef> for RValue {
    fn into(self) -> StructRef {
        match self {
            RValue::Struct(r#struct) => r#struct,
            _ => panic!("Referable is not a struct, but a {:?}", self)
        }
    }
}

impl Display for RValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RValue::EnumConstant(enum_constant) => write!(f, "{}", enum_constant.name.value),
            _ => panic!()
        }
    }
}

/// Represents a value which is known at runtime time or compile time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AValue {
    RValue(RValue, Span),
    CValue(CValue)
}

impl AValue {

    pub fn name(&self) -> Ident {
        match self {
            AValue::RValue(rvalue, _) => rvalue.name(),
            AValue::CValue(cvalue) => cvalue.name(),
        }
    }

    pub fn r#type(&self) -> Type {
        match self {
            AValue::RValue(rvalue, _) => rvalue.r#type(),
            AValue::CValue(cvalue) => cvalue.r#type()
        }
    }
}

/// Represent a value which is known at compile time
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CValue {
    String(ImmutableString, StructRef, Span),
    Number(ImmutableString, StructRef, Span),
}

impl CValue {

    pub fn name(&self) -> Ident {
        match self {
            CValue::String(_, r#struct, _) => r#struct.name.clone(),
            CValue::Number(_, r#struct, _) => r#struct.name.clone()
        }
    }

    pub fn r#type(&self) -> Type {
        match self {
            CValue::String(_, r#struct, _) => Type::Struct(Rc::clone(r#struct)),
            CValue::Number(_, r#struct, _) => Type::Struct(Rc::clone(&r#struct))
        }
    }
}