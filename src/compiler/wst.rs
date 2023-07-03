use crate::compiler;
use crate::compiler::wst::partial::Placeholder;
use crate::compiler::{wst, Op, Text};

pub mod partial {
    use crate::compiler::{wst, Op, Text};
    use std::collections::HashMap;

    #[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
    pub struct Placeholder(pub Text);

    impl<T: Into<Text>> From<T> for Placeholder {
        fn from(value: T) -> Self {
            Placeholder(value.into())
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
            self.try_into_call(|placeholder| {
                Err(format!(
                    "Assumed Call but was PartialCall with placeholder {:?}",
                    placeholder
                ))
            })
        }

        pub fn saturate(
            self,
            map: &mut HashMap<Placeholder, wst::Call>,
        ) -> Result<wst::Call, String> {
            self.try_into_call(|placeholder| {
                map.get(&placeholder)
                    .ok_or(format!("Cannot find placeholder {:?}", placeholder))
                    .cloned()
            })
        }

        pub(super) fn try_into_call(
            self,
            // TODO Add opaque type
            error_func: impl Fn(Placeholder) -> Result<wst::Call, String> + Clone,
        ) -> Result<wst::Call, String> {
            match self {
                Call::Condition(condition) => Ok(wst::Call::Condition(
                    wst::Condition::try_from_with(condition, error_func)?,
                )),
                Call::String(string) => Ok(wst::Call::String(Text::from(string))),
                Call::Number(number) => Ok(wst::Call::String(Text::from(number))),
                Call::Ident(ident) => Ok(wst::Call::Ident(ident)),
                Call::Function(function) => Ok(wst::Call::Function(wst::Function::try_from_with(
                    function, error_func,
                )?)),
                Call::Placeholder(placeholder) => error_func(placeholder),
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

impl From<compiler::Ident> for Ident {
    fn from(value: compiler::Ident) -> Self {
        Ident(value.value)
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Ident(Text::new(value))
    }
}

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

impl From<Condition> for Call {
    fn from(value: Condition) -> Self {
        Call::Condition(value)
    }
}

impl From<Ident> for Call {
    fn from(value: Ident) -> Self {
        Call::Ident(value)
    }
}

impl From<Function> for Call {
    fn from(value: Function) -> Self {
        Call::Function(value)
    }
}

impl Call {
    fn try_from_with(
        value: partial::Call,
        error_func: impl Fn(Placeholder) -> Result<Call, String> + Clone,
    ) -> Result<Self, String> {
        value.try_into_call(error_func)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Function {
    pub name: Ident,
    pub args: Vec<Call>,
}

impl Function {
    fn try_from_with(
        value: partial::Function,
        error_func: impl Fn(Placeholder) -> Result<Call, String> + Clone,
    ) -> Result<Self, String> {
        let args: Vec<_> = value
            .args
            .into_iter()
            .flat_map(|partial_call| Call::try_from_with(partial_call, error_func.clone()))
            .collect();
        Ok(Function {
            name: value.name,
            args,
        })
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Condition {
    left: Function,
    op: Op,
    right: Function,
}

impl Condition {
    fn try_from_with(
        value: partial::Condition,
        error_func: impl Fn(Placeholder) -> Result<Call, String> + Clone,
    ) -> Result<Self, String> {
        Ok(Condition {
            left: Function::try_from_with(value.left, error_func.clone())?,
            op: value.op,
            right: Function::try_from_with(value.right, error_func)?,
        })
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Rule {
    pub title: Text,
    pub event: Event,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Function>,
}
