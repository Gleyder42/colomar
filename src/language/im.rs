use std::fmt::{Debug, Display, Formatter};
use std::rc::{Rc, Weak};
use derivative::Derivative;
use crate::language::ast::{SpannedBool, UseRestriction};
use crate::language::{Ident, Span};

pub type RuleRef = Rc<RuleDeclaration>;
pub type EnumRef = Rc<Enum>;
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

    pub fn span(&self) -> Span {
        match self {
            Root::Rule(rule) => rule.span.clone(),
            Root::Enum(r#enum) => r#enum.span.clone(),
            Root::Event(event) => event.span.clone(),
            Root::Struct(r#struct) => r#struct.span.clone(),
        }
    }
}

#[derive(Derivative, Debug)]
#[derivative(PartialEq, Eq)]
pub struct EnumConstant {
    pub name: Ident,

    #[derivative(PartialEq = "ignore")]
    pub r#enum: Weak<Enum>
}

#[derive(Derivative, Debug, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Function {
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub arguments: Vec<DeclaredArgumentRef>
}

#[derive(Derivative, Debug, Clone)]
#[derivative(PartialEq, Eq)]
pub struct Property {
    pub is_workshop: SpannedBool,
    pub name: Ident,
    pub desc: UseRestriction,
    pub r#type: Types
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
pub struct Enum {
    pub name: Ident,
    pub is_workshop: SpannedBool,
    pub constants: Vec<Rc<EnumConstant>>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Enum(EnumRef),
    Struct(StructRef)
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Enum(_) => write!(f, "Enum"),
            Type::Struct(_) => write!(f, "Struct")
        }
    }
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub types: Types,
    pub default_value: Option<AValue>,
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Types {
    pub types: Vec<Type>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
}

impl Types {

    pub fn contains_type(&self, r#type: Type) -> bool {
        self.types.iter().any(|it| it == &r#type)
    }
}

impl Display for Types {
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
pub struct RuleDeclaration {
    pub title: String,
    pub event: EventRef,
    pub arguments: Vec<CalledArgument>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RValue {
    Enum(EnumRef),
    Event(EventRef),
    Struct(StructRef),
    EnumConstant(Rc<EnumConstant>),
    Function(FunctionRef),
    Property(PropertyRef),
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


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AValue {
    RValue(RValue, Span),
    CValue(CValue)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CValue {
    String(String, StructRef, Span),
    Number(String, StructRef, Span),
}

