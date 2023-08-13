use crate::compiler::trisult::Trisult;
use error::CompilerError;
use smol_str::SmolStr;
use span::Span;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

pub mod analysis;
pub mod cir;
// pub mod codegen;
pub mod codegen;
pub mod cst;
pub mod database;
pub mod error;
pub mod language;
pub mod loader;
pub mod printer;
pub mod span;
pub mod trisult;
pub mod wir;
pub mod workshop;
pub mod wst;

pub type Text = SmolStr;
// TODO Use hashlink instead
pub type HashableMap<K, V> = BTreeMap<K, V>;
pub type QueryTrisult<T> = Trisult<T, CompilerError>;

pub const CONDITIONS_LEN: usize = 6;
pub const ACTIONS_LEN: usize = 8;
pub const DECLARED_ARGUMENTS_LEN: usize = 4;
// TODO Give this a more generic name
pub const PROPERTY_DECLS_LEN: usize = 4;
pub const FUNCTIONS_DECLS_LEN: usize = 6;
pub const ENUM_CONSTANTS_LEN: usize = 8;

pub const CALLED_ARGUMENTS_LEN: usize = DECLARED_ARGUMENTS_LEN;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    // TODO Rename this field to text
    pub value: Text,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum UseRestriction {
    /// Can not be assigned directly, but can be accessed.
    GetVar,
    /// Can be assigned, but not be accessed
    SetVar,
    /// Can only be assigned once and be accessed
    Val,
    /// Can be assigned and accessed
    Var,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Op {
    Equals,
    NotEquals,
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Equals => write!(f, "=="),
            Op::NotEquals => write!(f, "!="),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignMod {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for AssignMod {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            AssignMod::Add => "Add",
            AssignMod::Sub => "Subtract",
            AssignMod::Mul => "Multiply",
            AssignMod::Div => "Divide",
        };
        write!(f, "{name}")
    }
}
