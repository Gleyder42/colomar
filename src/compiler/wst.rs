use std::collections::HashMap;
use std::fmt::Formatter;
use std::marker::PhantomData;
use serde::de::{Error, Visitor};
use serde::{Deserialize, Deserializer};
use crate::compiler::{HashableMap, Op, Text};

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Ident(pub Text);

impl From<Text> for Ident {
    fn from(value: Text) -> Self {
        Ident(value)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Placeholder(pub Text);

impl From<Text> for Placeholder {
    fn from(value: Text) -> Self {
        Placeholder(value)
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

impl TryFrom<PartialCall> for Call {
    type Error = String;

    fn try_from(value: PartialCall) -> Result<Self, Self::Error> {
        value.try_into_call(|placeholder| Err(format!("Cannot convert placeholder {:?}", placeholder)))
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum PartialCall {
    Condition(PartialCondition),
    String(String),
    Number(String),
    Ident(Ident),
    Function(PartialFunction),
    Placeholder(Placeholder),
}

impl PartialCall {

    pub fn complete(self) -> Result<Call, String> {
        self.try_into_call(|placeholder| Err(format!("Assumed Call but was PartialCall with placeholder {:?}", placeholder)))
    }

    pub fn saturate(self, map: &mut HashMap<Placeholder, Call>) -> Result<Call, String> {
        self.try_into_call(|placeholder| map
            .get(&placeholder)
            .ok_or(format!("Cannot find placeholder {:?}", placeholder))
            .cloned()
        )
    }

    fn try_into_call(self, error_func: impl Fn(Placeholder)->Result<Call, String>) -> Result<Call, String> {
        match self {
            PartialCall::Condition(condition) => Ok(Call::Condition(condition.try_into()?)),
            PartialCall::String(string) => Ok(Call::String(Text::from(string))),
            PartialCall::Number(number) => Ok(Call::String(Text::from(number))),
            PartialCall::Ident(ident) => Ok(Call::Ident(ident)),
            PartialCall::Function(function) => Ok(Call::Function(function.try_into()?)),
            PartialCall::Placeholder(placeholder) => error_func(placeholder)
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Function {
    pub name: Ident,
    pub args: Vec<Call>,
}

impl TryFrom<PartialFunction> for Function {
    type Error = String;

    fn try_from(value: PartialFunction) -> Result<Self, Self::Error> {
        let args: Vec<_> = value.args.into_iter()
            .flat_map(|partial_call| Call::try_from(partial_call))
            .collect();
        Ok(Function { name: value.name, args })
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct PartialFunction {
    pub name: Ident,
    pub args: Vec<PartialCall>,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Condition {
    left: Function,
    op: Op,
    right: Function,
}

impl TryFrom<PartialCondition> for Condition {
    type Error = String;

    fn try_from(value: PartialCondition) -> Result<Self, Self::Error> {
        Ok(Condition {
            left: value.left.try_into()?,
            op: value.op,
            right: value.right.try_into()?,
        })
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct PartialCondition {
    left: PartialFunction,
    op: Op,
    right: PartialFunction,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Rule {
    event: Event,
    conditions: Vec<Condition>,
    actions: Vec<Function>,
}