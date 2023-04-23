use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::rc::{Rc, Weak};
use derivative::Derivative;
use crate::language::ast::{CallChain, UseRestriction, Spanned};
use crate::language::{ast, Ident, Span};

pub type RuleRef = Rc<RefCell<RuleDeclaration>>;
pub type EnumRef = Rc<RefCell<Enum>>;
pub type EventRef = Rc<RefCell<EventDeclaration>>;
pub type DeclaredArgumentRef = Rc<RefCell<DeclaredArgument>>;
pub type FunctionRef = Rc<RefCell<Function>>;
pub type PropertyRef = Rc<RefCell<Property>>;
pub type StructRef = Rc<RefCell<Struct>>;

pub fn make_ref<T>(value: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(value))
}

// Intermediate
pub type Im = Vec<Root>;

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct IdentChain(pub Vec<Ident>);

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
            Root::Rule(rule) => rule.borrow().span.clone(),
            Root::Enum(r#enum) => r#enum.borrow().span.clone(),
            Root::Event(event) => event.borrow().span.clone(),
            Root::Struct(r#struct) => r#struct.borrow().span.clone(),
        }
    }
}

#[derive(Derivative, Debug)]
#[derivative(PartialEq, Eq)]
pub struct EnumConstant {
    pub name: Ident,

    #[derivative(PartialEq = "ignore")]
    pub r#enum: Weak<RefCell<Enum>>
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Function {
    pub is_workshop: Spanned<bool>,
    pub name: Ident,
    pub arguments: Vec<DeclaredArgumentRef>
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Property {
    pub is_workshop: Spanned<bool>,
    pub name: Ident,
    pub desc: UseRestriction,
    pub r#type: Link<Ident, Type>
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Struct {
    pub is_open: Spanned<bool>,
    pub is_workshop: Spanned<bool>,
    pub name: Ident,
    pub functions: Vec<FunctionRef>,
    pub properties: Vec<PropertyRef>,
    pub span: Span
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Enum {
    pub name: Ident,
    pub is_workshop: Spanned<bool>,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Link<T, V>
    where T: Debug + Clone + Eq,
          V: Debug + Clone + Eq {
    Unbound(T),
    Bound(V),
}

impl<T, V> Link<T, V>
    where T: Debug + Clone + Eq,
          V: Debug + Clone + Eq {
    /// Returns the unbound value, panics if the link is bound
    pub fn unbound(&self) -> &T {
        match self {
            Link::Unbound(value) => value,
            Link::Bound(_) => panic!("Link {self:?} was expected to be unbound, but was bound")
        }
    }

    pub fn take_unbound(self) -> T {
        match self {
            Link::Unbound(value) => value,
            Link::Bound(_) => panic!("Link {self:?} was expected to be unbound, but was bound")
        }
    }

    /// Returns the bound value, panics if the link is unbound
    pub fn bound(&self) -> &V {
        match self {
            Link::Unbound(_) => panic!("Link {self:?} was expected to be bound, but was unbound"),
            Link::Bound(value) => value,
        }
    }
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct DeclaredArgument {
    pub name: Ident,
    pub types: Link<ast::Types, Types>,
    pub default_value: Option<Link<CallChain, ActualValue>>,
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Types {
    pub types: Vec<Type>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span
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

impl Types {

    pub fn contains_type(&self, r#type: Type) -> bool {
        self.types.iter().any(|it| it == &r#type)
    }
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct CalledArgument {
    pub declared: DeclaredArgumentRef,
    pub value: ActualValue,
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
pub enum Referable {
    Enum(EnumRef),
    Event(EventRef),
    Struct(StructRef),
    EnumConstant(Rc<EnumConstant>),
    Function(FunctionRef),
    Property(PropertyRef),
}

impl Referable {

    pub fn r#type(&self) -> Type {
        match self {
            Referable::Enum(r#enum) => Type::Enum(r#enum.clone()),
            Referable::Struct(r#struct) => Type::Struct(r#struct.clone()),
            Referable::EnumConstant(enum_constant) => Type::Enum(enum_constant.r#enum.upgrade().unwrap().clone()),
            Referable::Property(property) => property.borrow().r#type.bound().clone(),
            Referable::Function(_) => todo!("Function types are not implemented yet"),
            Referable::Event(_) => todo!("Event types are not implemented yet"),
        }
    }
}

impl Into<StructRef> for Referable {
    fn into(self) -> StructRef {
        match self {
            Referable::Struct(r#struct) => r#struct,
            _ => panic!("Referable is not a struct, but a {:?}", self)
        }
    }
}

impl Display for Referable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Referable::EnumConstant(enum_constant) => write!(f, "{}", enum_constant.name.value),
            _ => panic!()
        }
    }
}

impl Referable {
    pub fn name_span(&self) -> Span {
        match self {
            Referable::Enum(r#enum) => r#enum.borrow().name.span.clone(),
            Referable::Struct(r#struct) => r#struct.borrow().name.span.clone(),
            Referable::Event(event) => event.borrow().name.span.clone(),
            Referable::EnumConstant(enum_constant) => enum_constant.name.span.clone(),
            Referable::Function(function) => function.borrow().name.span.clone(),
            Referable::Property(property) => property.borrow().name.span.clone(),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Referable::Event(_) => "Event",
            Referable::Enum(_) => "Enum",
            Referable::EnumConstant(_) => "Enum Constant",
            Referable::Struct(_) => "Struct",
            Referable::Function(_) => "Function",
            Referable::Property(_) => "Property"
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActualValue {
    Referable(Referable, Span),
    String(String, StructRef, Span),
    Number(String, StructRef, Span),
}

impl Display for ActualValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ActualValue::String(string, ..) => write!(f, "{string}"),
            ActualValue::Number(number, ..) => write!(f, "{number}"),
            ActualValue::Referable(referable, ..) => write!(f, "{referable}")
        }
    }
}

impl ActualValue {

    pub fn r#type(&self) -> Type {
        match self {
            ActualValue::String(_, r#struct, _) => Type::Struct(Rc::clone(&r#struct)),
            ActualValue::Number(_, r#struct, _) => Type::Struct(Rc::clone(&r#struct)),
            ActualValue::Referable(referable, _) => referable.r#type()
        }
    }

    /// The span of the actual value
    pub fn span(&self) -> Span {
        match self {
            ActualValue::String(_, _, span) => span.clone(),
            ActualValue::Number(_, _, span) => span.clone(),
            ActualValue::Referable(_, span) => span.clone()
        }
    }
}
