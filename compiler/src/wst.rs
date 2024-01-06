use super::{Op, Text, TextId};
use crate::analysis::interner::Interner;
use smol_str::SmolStr;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;

pub type Span = Range<usize>;

pub mod partial {
    use super::super::{wst, Op};
    use smol_str::SmolStr;
    use std::collections::HashMap;

    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub struct Placeholder(pub SmolStr);

    impl<T: Into<SmolStr>> From<T> for Placeholder {
        fn from(value: T) -> Self {
            Placeholder(value.into())
        }
    }

    pub trait Replacer: Fn(Placeholder) -> Result<wst::Call, SaturateError> + Clone {}
    impl<T: Fn(Placeholder) -> Result<wst::Call, SaturateError> + Clone> Replacer for T {}

    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub struct SaturateError(Placeholder, SaturateErrorReason);

    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub enum SaturateErrorReason {
        WasPartial,
        CannotFindReplace,
    }

    /// A partial call is almost identical to a regular [wst::Call].
    /// The difference is that a partial call can contain placeholder.
    /// A placeholder is not valid workshop code, and must be 'saturated' with values into a [wst::Call].
    /// Use [Call::saturate] or [Call::saturate_with] for that.
    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub enum Call {
        Condition(Condition),
        String(String),
        Number(String),
        Ident(wst::Ident),
        Function(Function),
        Placeholder(Placeholder),
    }

    impl Call {
        /// Converts a partial call into a [wst::Call] without replacing placeholder.
        ///
        /// If you have a replace map, use [Call::saturate] or [Call::saturate_with] instead.
        pub fn complete(self) -> Result<wst::Call, SaturateError> {
            self.saturate_with(|placeholder| {
                let error = SaturateError(placeholder, SaturateErrorReason::WasPartial);
                Err(error)
            })
        }

        pub fn saturate(
            self,
            replace_call_map: &HashMap<Placeholder, wst::Call>,
        ) -> Result<wst::Call, SaturateError> {
            self.saturate_with(|placeholder| {
                replace_call_map
                    .get(&placeholder)
                    .ok_or(SaturateError(placeholder, SaturateErrorReason::WasPartial))
                    .cloned()
            })
        }

        pub(super) fn saturate_with(
            self,
            replacer: impl Replacer,
        ) -> Result<wst::Call, SaturateError> {
            match self {
                Call::Condition(condition) => {
                    condition.saturate_with(replacer).map(wst::Call::Condition)
                }
                Call::String(string) => Ok(wst::Call::String(string)),
                Call::Number(number) => Ok(wst::Call::String(number)),
                Call::Ident(ident) => Ok(wst::Call::Ident(ident)),
                Call::Function(function) => {
                    function.saturate_with(replacer).map(wst::Call::Function)
                }
                Call::Placeholder(placeholder) => replacer(placeholder),
            }
        }
    }

    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub struct Function {
        pub name: wst::Ident,
        pub args: Vec<Call>,
    }

    impl Function {
        fn saturate_with(self, replacer: impl Replacer) -> Result<wst::Function, SaturateError> {
            let args: Result<Vec<wst::Call>, _> = self
                .args
                .into_iter()
                .map(|call| Call::saturate_with(call, replacer.clone()))
                .collect();
            let function = wst::Function {
                name: self.name,
                args: args?,
            };
            Ok(function)
        }
    }

    #[derive(Clone, Debug, Hash, Eq, PartialEq)]
    pub struct Condition {
        pub left: Box<Call>,
        pub op: Op,
        pub right: Box<Call>,
    }

    impl Condition {
        fn saturate_with(self, replacer: impl Replacer) -> Result<wst::Condition, SaturateError> {
            let left = Call::saturate_with(*self.left, replacer.clone())?;
            let right = Call::saturate_with(*self.right, replacer)?;
            let condition = wst::Condition {
                left: Box::new(left),
                op: self.op,
                right: Box::new(right),
            };
            Ok(condition)
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Ident(pub SmolStr);

impl Ident {
    pub fn from_ident(ident: super::Ident, interner: &dyn Interner) -> Self {
        Self(interner.lookup_intern_string(ident.value).into())
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: AsRef<str>> From<T> for Ident {
    fn from(value: T) -> Self {
        Ident(SmolStr::new(value))
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Event {
    pub name: Ident,
    pub team: Option<Ident>,
    pub hero_slot: Option<Ident>,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Variable {
    pub index: u8,
    pub name: Ident,
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.index, self.name)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Call {
    Condition(Condition),
    String(Text),
    Number(Text),
    Ident(Ident),
    Boolean(bool),
    Property(Ident, Ident),
    Function(Function),
    Vararg(Vec<Call>),
}

impl Call {
    pub fn unwrap_function(self) -> Function {
        if let Call::Function(function) = self {
            function
        } else {
            panic!("Cannot unwrap {:?} as function", self)
        }
    }

    pub fn is_condition(&self) -> bool {
        match self {
            Call::Condition(_) => true,
            _ => false,
        }
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Call::Condition(it) => it.to_string(),
            Call::String(it) => format!("\"{}\"", it.to_string()),
            Call::Number(it) => it.to_string(),
            Call::Ident(it) => it.to_string(),
            Call::Vararg(calls) => {
                let string: Vec<_> = calls.iter().map(|it| it.to_string()).collect();
                string.join(", ")
            }
            Call::Boolean(it) => match *it {
                true => "True".to_owned(),
                false => "False".to_owned(),
            },
            Call::Function(it) => it.to_string(),
            Call::Property(caller, name) => format!("{caller}.{name}"),
        };
        write!(f, "{}", string)
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

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
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

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Condition {
    pub left: Box<Call>,
    pub op: Op,
    pub right: Box<Call>,
}

impl Display for Condition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.left.is_condition() {
            write!(f, "({})", self.left)?;
        } else {
            write!(f, "{}", self.left)?;
        }

        write!(f, " {} ", self.op)?;

        if self.right.is_condition() {
            write!(f, "({})", self.right)?;
        } else {
            write!(f, "{}", self.right)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Rule {
    pub title: TextId,
    pub event: Event,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Function>,
}
