use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use derivative::Derivative;
use crate::language::ast::Spanned;
use crate::language::{Ident, Span};

pub type RuleRef = Rc<RefCell<Rule>>;
pub type EnumRef = Rc<RefCell<Enum>>;
pub type EventRef = Rc<RefCell<Event>>;
pub type DeclaredArgumentRef = Rc<RefCell<DeclaredArgument>>;

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
}

impl Root {
    pub fn name(&self) -> &'static str {
        match self {
            Root::Event(_) => "Event",
            Root::Enum(_) => "Enum",
            Root::Rule(_) => "Rule"
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Root::Rule(rule) => rule.borrow().span.clone(),
            Root::Enum(r#enum) => r#enum.borrow().span.clone(),
            Root::Event(event) => event.borrow().span.clone()
        }
    }
}

#[derive(Derivative, Debug, Eq)]
#[derivative(PartialEq)]
pub struct EnumConstant {
    pub name: Ident,
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

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub enum ConstValue {
    EnumConstant(Rc<EnumConstant>)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Enum(EnumRef)
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
    pub types: Vec<Link<Ident, Type>>,
    pub default_value: Option<Link<IdentChain, ConstValue>>,
}

impl DeclaredArgument {
    pub fn contains_type(&self, r#enum: &EnumRef) -> bool {
        for link in &self.types {
            match link.bound() {
                Type::Enum(bound_enum) => {
                    if Rc::ptr_eq(bound_enum, r#enum) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct CalledArgument {
    pub declared: DeclaredArgumentRef,
    pub value: ConstValue,
}

#[derive(Derivative, Debug, Clone, Eq)]
#[derivative(PartialEq)]
pub struct Event {
    pub name: Ident,
    pub arguments: Vec<DeclaredArgumentRef>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

#[derive(Derivative, Debug, Eq)]
#[derivative(PartialEq)]
pub struct Rule {
    pub title: String,
    pub event: Link<Ident, EventRef>,
    pub arguments: Link<Vec<IdentChain>, Vec<CalledArgument>>,

    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;
    use crate::language::ast::{Spanned};
    use crate::language::Ident;
    use crate::language::im::{CalledArgument, ConstValue, DeclaredArgument, Enum, EnumConstant, Event, IdentChain, Link, Rule};

    #[test]
    fn test_ident_eq() {
        let a = Ident::new("Hello", 0..10);
        let b = Ident::new("Hello", 10..20);

        assert_eq!(a, b);
    }

    #[test]
    fn test_spanned_eq() {
        let a = Spanned("Hello", 0..1);
        let b = Spanned("Hello", 10..20);

        assert_eq!(a, b);
    }

    #[test]
    fn test_ident_chain_eq() {
        let a = IdentChain(vec![
            Ident::new("Hello", 0..5),
            Ident::new("World", 5..10)
        ]);
        let b = IdentChain(vec![
            Ident::new("Hello", 10..15),
            Ident::new("World", 15..20)
        ]);

        assert_eq!(a, b);
    }

    #[test]
    fn test_enum_constant_eq() {
        let a = EnumConstant { name: Ident::new("Ana", 0..1) };
        let b = EnumConstant { name: Ident::new("Ana", 0..10) };

        assert_eq!(a, b);
    }

    #[test]
    fn test_enum_eq() {
        let a = Enum {
            name: Ident::new("Hero", 0..10),
            span: 0..20,
            is_workshop: Spanned(true, 0..2),
            constants: vec![
                Rc::new(EnumConstant { name: Ident::new("Ana", 5..6) }),
                Rc::new(EnumConstant { name: Ident::new("Zarya", 6..7) })
            ]
        };

        let b = Enum {
            name: Ident::new("Hero", 100..120),
            span: 0..20,
            is_workshop: Spanned(true, 15..16),
            constants: vec![
                Rc::new(EnumConstant { name: Ident::new("Ana", 50..60) }),
                Rc::new(EnumConstant { name: Ident::new("Zarya", 60..70) })
            ]
        };

        assert_eq!(a, b);
    }

    #[test]
    fn test_rule_eq() {
        let declared_argument = DeclaredArgument {
            name: Ident::new("team", 1..2),
            types: vec![Link::Unbound(Ident::new("Team", 12..13))],
            default_value: None
        };

        let enum_constant = EnumConstant { name: Ident::new("Hello World", 10..15)};
        let argument = CalledArgument {
            value: ConstValue::EnumConstant(Rc::new(enum_constant)),
            declared: Rc::new(RefCell::new(declared_argument))
        };

        let a = Rule {
            span: 0..100,
            title: "Hello World".to_string(),
            arguments: Link::Bound(vec![argument]),
            event: Link::Unbound(Ident::new("Ongoing", 10..12))
        };


        let declared_argument = DeclaredArgument {
            name: Ident::new("team", 10..20),
            types: vec![Link::Unbound(Ident::new("Team", 120..130))],
            default_value: None
        };

        let enum_constant = EnumConstant { name: Ident::new("Hello World", 100..150)};
        let argument = CalledArgument {
            value: ConstValue::EnumConstant(Rc::new(enum_constant)),
            declared: Rc::new(RefCell::new(declared_argument))
        };

        let b = Rule {
            span: 0..1000,
            title: "Hello World".to_string(),
            arguments: Link::Bound(vec![argument]),
            event: Link::Unbound(Ident::new("Ongoing", 100..120))
        };

        assert_eq!(a, b);
    }

    #[test]
    fn test_event_eq() {
        let declared_argument = DeclaredArgument {
            name: Ident::new("team", 10..20),
            types: vec![Link::Unbound(Ident::new("Team", 120..130))],
            default_value: None
        };

        let a = Event {
            span: 0..15,
            name: Ident::new("Hello", 2..10),
            arguments: vec![Rc::new(RefCell::new(declared_argument))]
        };

        let declared_argument = DeclaredArgument {
            name: Ident::new("team", 77..77),
            types: vec![Link::Unbound(Ident::new("Team", 10..30))],
            default_value: None
        };

        let b = Event {
            span: 0..15,
            name: Ident::new("Hello", 5..10),
            arguments: vec![Rc::new(RefCell::new(declared_argument))]
        };

        assert_eq!(a, b);
    }
}