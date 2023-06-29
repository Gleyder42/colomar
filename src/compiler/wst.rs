use std::collections::HashMap;
use std::fmt::Formatter;
use std::marker::PhantomData;
use serde::de::{Error, Visitor};
use serde::{Deserialize, Deserializer};
use crate::compiler::{HashableMap, Op, Text};

pub mod partial {
    use std::collections::HashMap;
    use crate::compiler::{Op, Text, wst};

    #[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
    pub struct Placeholder(pub Text);

    impl From<Text> for Placeholder {
        fn from(value: Text) -> Self {
            Placeholder(value)
        }
    }

    #[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
    pub enum Call {
        Condition(Condition),
        String(String),
        Number(String),
        Ident(wst::Ident),
        Function(Function),
        Placeholder(Placeholder),
    }

    impl Call {

        pub fn complete(self) -> Result<wst::Call, String> {
            self.try_into_call(|placeholder| Err(format!("Assumed Call but was PartialCall with placeholder {:?}", placeholder)))
        }

        pub fn saturate(self, map: &mut HashMap<Placeholder, super::Call>) -> Result<super::Call, String> {
            self.try_into_call(|placeholder| map
                .get(&placeholder)
                .ok_or(format!("Cannot find placeholder {:?}", placeholder))
                .cloned()
            )
        }

        pub(super) fn try_into_call(self, error_func: impl Fn(Placeholder)->Result<super::Call, String>) -> Result<wst::Call, String> {
            match self {
                Call::Condition(condition) => Ok(wst::Call::Condition(condition.try_into()?)),
                Call::String(string) => Ok(wst::Call::String(Text::from(string))),
                Call::Number(number) => Ok(wst::Call::String(Text::from(number))),
                Call::Ident(ident) => Ok(wst::Call::Ident(ident)),
                Call::Function(function) => Ok(wst::Call::Function(function.try_into()?)),
                Call::Placeholder(placeholder) => error_func(placeholder)
            }
        }
    }

    #[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
    pub struct Function {
        pub name: wst::Ident,
        pub args: Vec<Call>,
    }

    #[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
    pub struct Condition {
        pub left: Function,
        pub op: Op,
        pub right: Function,
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Ident(pub Text);

impl From<Text> for Ident {
    fn from(value: Text) -> Self {
        Ident(value)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Event {
    pub name: Ident,
    pub team: Ident,
    pub hero_slot: Ident,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Call {
    Condition(Condition),
    String(Text),
    Number(Text),
    Ident(Ident),
    Function(Function),
}

impl TryFrom<partial::Call> for Call {
    type Error = String;

    fn try_from(value: partial::Call) -> Result<Self, Self::Error> {
        value.try_into_call(|placeholder| Err(format!("Cannot convert placeholder {:?}", placeholder)))
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Function {
    pub name: Ident,
    pub args: Vec<Call>,
}

impl TryFrom<partial::Function> for Function {
    type Error = String;

    fn try_from(value: partial::Function) -> Result<Self, Self::Error> {
        let args: Vec<_> = value.args.into_iter()
            .flat_map(|partial_call| Call::try_from(partial_call))
            .collect();
        Ok(Function { name: value.name, args })
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Condition {
    left: Function,
    op: Op,
    right: Function,
}

impl TryFrom<partial::Condition> for Condition {
    type Error = String;

    fn try_from(value: partial::Condition) -> Result<Self, Self::Error> {
        Ok(Condition {
            left: value.left.try_into()?,
            op: value.op,
            right: value.right.try_into()?,
        })
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Rule {
    event: Event,
    conditions: Vec<Condition>,
    actions: Vec<Function>,
}