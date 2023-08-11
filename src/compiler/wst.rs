use crate::compiler;
use crate::compiler::wst::partial::Placeholder;
use crate::compiler::{Op, Text};
use std::fmt::{Display, Formatter};

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
        pub left: Box<Call>,
        pub op: Op,
        pub right: Box<Call>,
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Ident(pub Text);

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: AsRef<str>> From<T> for Ident {
    fn from(value: T) -> Self {
        Ident(Text::new(value))
    }
}

impl From<compiler::Ident> for Ident {
    fn from(value: compiler::Ident) -> Self {
        Ident(value.value)
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

impl Display for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Call::Condition(it) => it.to_string(),
            Call::String(it) => format!("\"{}\"", it.to_string()),
            Call::Number(it) => it.to_string(),
            Call::Ident(it) => it.to_string(),
            Call::Function(it) => it.to_string(),
        };
        write!(f, "{}", string)
    }
}

impl Call {
    pub fn unwrap_function(self) -> Function {
        if let Call::Function(function) = self {
            function
        } else {
            panic!("Cannot unwrap {:?} as function", self)
        }
    }
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

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|it| it.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "{name}({args})", name = self.name)
    }
}

impl Function {
    fn try_from_with(
        value: partial::Function,
        error_func: impl Fn(Placeholder) -> Result<Call, String> + Clone,
    ) -> Result<Self, String> {
        let args: Result<Vec<Call>, _> = dbg!(value.args)
            .into_iter()
            .map(|partial_call| Call::try_from_with(partial_call, error_func.clone()))
            .collect();
        Ok(Function {
            name: dbg!(value.name),
            args: args?,
        })
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Condition {
    pub left: Box<Call>,
    pub op: Op,
    pub right: Box<Call>,
}

impl Display for Condition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{left} {op} {right}",
            left = self.left,
            op = self.op,
            right = self.right
        )
    }
}

impl Condition {
    fn try_from_with(
        value: partial::Condition,
        error_func: impl Fn(Placeholder) -> Result<Call, String> + Clone,
    ) -> Result<Self, String> {
        Ok(Condition {
            left: Box::new(Call::try_from_with(*value.left, error_func.clone())?),
            op: value.op,
            right: Box::new(Call::try_from_with(*value.right, error_func)?),
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
