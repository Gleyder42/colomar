use error::CompilerError;
use hashlink::LinkedHashMap;
use smallvec::SmallVec;
use span::Span;
use span::StringId;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use trisult::Trisult;

pub mod analysis;
pub mod cir;
// pub mod codegen;
pub mod codegen;
pub mod cst;
pub mod database;
pub mod error;
pub mod error_reporter;
pub mod language;
pub mod loader;
pub mod printer;
pub mod source_cache;
pub mod span;
pub mod trisult;
pub mod wir;
pub mod workshop;
pub mod wst;

pub type FullText = String;
pub type TextId = StringId;

pub type HashableMap<K, V> = LinkedHashMap<K, V>;
pub type QueryTrisult<T> = Trisult<T, CompilerError>;

pub const CONDITIONS_LEN: usize = 6;
pub const ACTIONS_LEN: usize = 8;
pub const DECL_ARGS_LEN: usize = 4;
// TODO Give this a more generic name
pub const PROPERTY_DECLS_LEN: usize = 4;
pub const FUNCTIONS_DECLS_LEN: usize = 6;
pub const ENUM_CONSTANTS_LEN: usize = 8;
pub const CALLED_ARGS_LEN: usize = DECL_ARGS_LEN;
pub type StructId = TextId;
pub type SVMultiMap<K, V, const N: usize> = HashMap<K, SmallVec<[V; N]>>;

pub struct SVMultiMapWrapper<K, V, const N: usize>(SVMultiMap<K, V, N>);

impl<K, V, const N: usize> FromIterator<(K, V)> for SVMultiMapWrapper<K, V, N>
where
    K: Eq + Hash,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut map = SVMultiMap::new();

        for (key, value) in iter {
            map.entry(key)
                .or_insert(SmallVec::<[V; N]>::new())
                .push(value);
        }

        SVMultiMapWrapper(map)
    }
}

impl<K, V, const N: usize> FromIterator<(K, SmallVec<[V; N]>)> for SVMultiMapWrapper<K, V, N>
where
    K: Eq + Hash,
{
    fn from_iter<T: IntoIterator<Item = (K, SmallVec<[V; N]>)>>(iter: T) -> Self {
        let mut map = SVMultiMap::new();

        for (key, mut value) in iter {
            map.entry(key)
                .or_insert(SmallVec::<[V; N]>::new())
                .append(&mut value)
        }

        SVMultiMapWrapper(map)
    }
}

impl<K, V, const N: usize> Into<SVMultiMap<K, V, N>> for SVMultiMapWrapper<K, V, N> {
    fn into(self) -> SVMultiMap<K, V, N> {
        self.0
    }
}

pub fn flatten<LR, L, R, const LN: usize, const RN: usize>(
    iter: impl IntoIterator<Item = LR>,
    splitter: impl Fn(LR) -> (SmallVec<[L; LN]>, SmallVec<[R; RN]>),
) -> (SmallVec<[L; LN]>, SmallVec<[R; RN]>) {
    let mut left_vec: SmallVec<[L; LN]> = SmallVec::new();
    let mut right_vec: SmallVec<[R; RN]> = SmallVec::new();

    for lr in iter {
        let (mut left, mut right) = splitter(lr);
        left_vec.append(&mut left);
        right_vec.append(&mut right);
    }

    (left_vec, right_vec)
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    pub value: TextId,
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

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Op {
    And,
    Equals,
    NotEquals,
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Equals => write!(f, "=="),
            Op::NotEquals => write!(f, "!="),
            Op::And => write!(f, "&&"),
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

fn compiler_todo<T>(string: impl Into<Cow<'static, str>>, span: Span) -> QueryTrisult<T> {
    QueryTrisult::Err(vec![CompilerError::NotImplemented(string.into(), span)])
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Expr<T> {
    Chain(T),
    Neg(Box<Expr<T>>),
    And(Box<Expr<T>>, Box<Expr<T>>),
    Or(Box<Expr<T>>, Box<Expr<T>>),
}

impl<T> Expr<T> {
    fn lhs(&self) -> Option<&Expr<T>> {
        match self {
            Expr::Chain(_) => None,
            Expr::Neg(_) => None,
            Expr::And(lhs, _) | Expr::Or(lhs, _) => Some(lhs.as_ref()),
        }
    }

    fn rhs(&self) -> Option<&Expr<T>> {
        match self {
            Expr::Chain(_) => None,
            Expr::Neg(_) => None,
            Expr::And(_, rhs) | Expr::Or(_, rhs) => Some(rhs.as_ref()),
        }
    }
}
