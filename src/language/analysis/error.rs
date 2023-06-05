use std::fmt::Debug;
use crate::language::{ast, Ident};
use crate::language::analysis::interner::{Interner, IntoInternId};
use crate::language::analysis::namespace::Namespace;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Trisult<T, E> {
    Ok(T),
    Par(T, Vec<E>),
    Err(Vec<E>),
}

impl<Id, T: IntoInternId<Interned=Id>, I: IntoIterator<Item=T>, E> Trisult<I, E> {
    pub fn intern_inner<Db: Interner + ?Sized>(self, db: &Db) -> Trisult<Vec<Id>, E> {
        self.map_inner(|t| t.intern(db))
    }
}

impl<E> Trisult<Namespace, E> {

    pub fn fold_with<T, I, F>(self, with: Trisult<I, E>, func: F) -> Trisult<Namespace, E>
        where F: Fn(Namespace, T) -> Trisult<Namespace, E>,
              I: IntoIterator<Item=T>
    {
        self.flat_map(|acc| with.fold(acc, func))
    }
}

impl<T, I: IntoIterator<Item=T>, E> Trisult<I, E> {
    pub fn fold<A, F>(self, initial: A, func: F) -> Trisult<A, E>
        where F: Fn(A, T) -> Trisult<A, E>
    {
        self.fold_flat_map(initial, |it| it, func)
    }

    pub fn fold_flat_map<U, A, F, M>(self, initial: A, map_func: M, func: F) -> Trisult<U, E>
        where F: Fn(A, T) -> Trisult<A, E>,
              M: FnOnce(A) -> U,
    {
        self.flat_map(|iter| {
            let mut errors = Vec::new();
            let mut current = initial;

            for item in iter.into_iter() {
                let result = func(current, item);
                match result {
                    Trisult::Ok(value) => current = value,
                    Trisult::Par(value, mut result_errors) => {
                        current = value;
                        errors.append(&mut result_errors);
                    }
                    Trisult::Err(mut result_errors) => {
                        errors.append(&mut result_errors);
                        return Trisult::Err(errors);
                    }
                }
            }

            Trisult::Ok(map_func(current))
        })
    }

    pub fn map_inner<F: Fn(T) -> U, U>(self, func: F) -> Trisult<Vec<U>, E> {
        self.map(|iter| iter.into_iter().map(|it| func(it)).collect::<Vec<U>>())
    }
}

impl<T, E> Trisult<T, E> {
    pub fn from_option(option: Option<T>, error: E) -> Trisult<T, E> {
        match option {
            Some(value) => Trisult::Ok(value),
            None => Trisult::Err(vec![error])
        }
    }

    pub fn to_option(self) -> (Option<T>, Vec<E>) {
        match self {
            Trisult::Ok(value) => (Some(value), Vec::new()),
            Trisult::Par(value, errors) => (Some(value), errors),
            Trisult::Err(errors) => (None, errors)
        }
    }
}

impl<Id, T: IntoInternId<Interned=Id>, E> Trisult<T, E> {
    pub fn intern<Db: Interner + ?Sized>(self, db: &Db) -> Trisult<Id, E> {
        self.map(|it| it.intern(db))
    }
}

impl<E> From<ast::CallChain> for Trisult<ast::CallChain, E> {
    fn from(value: ast::CallChain) -> Self {
        Trisult::Ok(value)
    }
}

impl<T, E> From<Result<Trisult<T, E>, E>> for Trisult<T, E> {
    fn from(value: Result<Trisult<T, E>, E>) -> Self {
        match value {
            Ok(query_result) => query_result,
            Err(error) => Trisult::Err(vec![error])
        }
    }
}

impl<T, E> From<Result<T, E>> for Trisult<T, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(value) => Trisult::Ok(value),
            Err(error) => Trisult::Err(vec![error])
        }
    }
}

impl<T, E> FromIterator<Result<T, E>> for Trisult<Vec<T>, E> {
    fn from_iter<I: IntoIterator<Item=Result<T, E>>>(iter: I) -> Self {
        let mut results = Vec::new();
        let mut errors = Vec::new();

        for result in iter {
            match result {
                Ok(value) => results.push(value),
                Err(error) => errors.push(error)
            }
        }

        from_results(results, errors)
    }
}


impl<T, E> FromIterator<Trisult<T, E>> for Trisult<Vec<T>, E> {
    fn from_iter<I: IntoIterator<Item=Trisult<T, E>>>(iter: I) -> Self {
        let mut results = Vec::new();
        let mut errors = Vec::new();
        for result in iter {
            match result {
                Trisult::Ok(value) => results.push(value),
                Trisult::Par(value, mut result_errors) => {
                    results.push(value);
                    errors.append(&mut result_errors);
                }
                Trisult::Err(mut result_errors) => errors.append(&mut result_errors)
            }
        }

        from_results(results, errors)
    }
}

/// Creates a [Trisult] from a result and error [Vec].
/// The result is
/// * [Trisult::Ok], if no errors are found
/// * [Trisult::Par], if errors are found.
fn from_results<T, E>(results: Vec<T>, errors: Vec<E>) -> Trisult<Vec<T>, E> {
    match errors.is_empty() {
        true => Trisult::Ok(results),
        false => Trisult::Par(results, errors)
    }
}

impl<T, E> From<(Vec<T>, Vec<E>)> for Trisult<Vec<T>, E> {
    fn from(value: (Vec<T>, Vec<E>)) -> Self {
        from_results(value.0, value.1)
    }
}

impl<E> Trisult<(), E> {
    pub fn empty() -> Trisult<(), E> {
        Trisult::Ok(())
    }
}

#[macro_export]
macro_rules! query_error {
    ($($x:expr),+ $(,)?) => {
        Trisult::Err(vec![$($x),+])
    };
}

impl<T: Clone, E> Trisult<T, E> {
    pub fn map_and_require<O, F>(self, func: F) -> Trisult<(T, O), E>
        where F: FnOnce(T) -> Trisult<O, E>
    {
        self.flat_map(|t| func(t.clone()).map(|o| (t, o)))
    }
}

impl<T, E> Trisult<T, E> {
    pub fn maybe_add_error(self, option: Option<E>) -> Trisult<T, E> {
        match option {
            Some(error) => self.add_error(error),
            None => self
        }
    }

    pub fn add_error(self, error: E) -> Trisult<T, E> {
        match self {
            Trisult::Ok(value) => Trisult::Par(value, vec![error]),
            Trisult::Par(value, mut errors) => {
                errors.push(error);
                Trisult::Par(value, errors)
            }
            Trisult::Err(mut errors) => {
                errors.push(error);
                Trisult::Err(errors)
            }
        }
    }

    /// Combines this result's value [T] with another result's value [O].
    ///
    /// # Notes
    /// * If this result is [Trisult::Err] the combined result will also be [Trisult::Err]
    /// * If the other result is [Trisult::Err] the combined result will be [Trisult::Par]
    /// and use the default value of [O]
    pub fn and_or_default<O: Default>(self, other: Trisult<O, E>) -> Trisult<(T, O), E> {
        self.and(
            |value, errors| Trisult::Par((value, O::default()), errors),
            other,
        )
    }

    /// Combines this result's value [T] with another result's value [O].
    ///
    /// # Notes
    /// * If this result or the other result is [Trisult::Err] the combined result will also be [Trisult::Err]
    pub fn and_require<O>(self, other: Trisult<O, E>) -> Trisult<(T, O), E> {
        self.and(|_value, errors| Trisult::Err(errors), other)
    }

    /// Combines this result's value [T] with another result's value [O].
    ///
    /// # Notes
    /// * If this result is [Trisult::Err] the combined result will also be [Trisult::Err]
    /// * If the other result is [Trisult::Err] the recovery function defines the combined result
    fn and<O, F>(self, recovery: F, other: Trisult<O, E>) -> Trisult<(T, O), E>
        where F: FnOnce(T, Vec<E>) -> Trisult<(T, O), E>,
    {
        use Trisult::*;

        match (self, other) {
            (Ok(value), Ok(other_value)) => Ok((value, other_value)),
            (Ok(value), Par(other_value, other_errors)) => Par((value, other_value), other_errors),
            (Ok(value), Err(errors)) => recovery(value, errors),
            (Par(value, errors), Ok(other_value)) => Par((value, other_value), errors),
            (Par(value, mut errors), Par(other_value, mut other_errors)) => {
                errors.append(&mut other_errors);
                Par((value, other_value), errors)
            }
            (Par(value, mut errors), Err(mut other_errors)) => {
                errors.append(&mut other_errors);
                recovery(value, errors)
            }
            (Err(errors), Ok(_other_value)) => Err(errors),
            (Err(mut errors), Par(_other_value, mut other_errors)) => {
                errors.append(&mut other_errors);
                Err(errors)
            }
            (Err(mut errors), Err(mut other_errors)) => {
                errors.append(&mut other_errors);
                Err(errors)
            }
        }
    }

    pub fn and_maybe<O>(
        self,
        option: Option<Trisult<O, E>>,
    ) -> Trisult<(T, Option<O>), E> {
        match option {
            Some(result) => {
                self.and(
                    |value, errors| Trisult::Par((value, None), errors),
                    result.map(|it| Some(it)),
                )
            }
            None => self.map(|it| (it, None))
        }
    }

    pub fn map_with_result_option<O, U, F>(
        self,
        option: Option<Result<O, E>>,
        func: F,
    ) -> Trisult<U, E>
        where F: FnOnce(T, Option<O>) -> U
    {
        let (value, error) = match option {
            None => (None, None),
            Some(result) => {
                match result {
                    Ok(result_value) => (Some(result_value), None),
                    Err(error) => (None, Some(error))
                }
            }
        };

        self
            .maybe_add_error(error)
            .map(|it| func(it, value))
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, func: F) -> Trisult<U, E> {
        match self {
            Trisult::Ok(value) => Trisult::Ok(func(value)),
            Trisult::Par(value, errors) => Trisult::Par(func(value), errors),
            Trisult::Err(errors) => Trisult::Err(errors)
        }
    }

    pub fn flat_map<U, F: FnOnce(T) -> Trisult<U, E>>(self, func: F) -> Trisult<U, E> {
        match self {
            Trisult::Ok(value) => func(value),
            Trisult::Par(value, mut errors) => {
                let result = func(value);
                match result {
                    Trisult::Ok(value) => Trisult::Par(value, errors),
                    Trisult::Par(value, mut mapped_errors) => {
                        errors.append(&mut mapped_errors);
                        Trisult::Par(value, errors)
                    }
                    Trisult::Err(mut mapped_errors) => {
                        errors.append(&mut mapped_errors);
                        Trisult::Err(errors)
                    }
                }
            }
            Trisult::Err(errors) => Trisult::Err(errors)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnalysisError {
    DuplicateIdent {
        first: Ident,
        second: Ident,
    },
    CannotFindDefinition(salsa::InternId),
    CannotFindIdent(Ident),
    WrongType,
    NotABool,
    CannotFindPrimitive,
}

impl<T> Into<Result<T, AnalysisError>> for AnalysisError {
    fn into(self) -> Result<T, AnalysisError> {
        Err(self)
    }
}

impl<T> Into<Trisult<T, AnalysisError>> for AnalysisError {
    fn into(self) -> Trisult<T, AnalysisError> {
        query_error!(self)
    }
}

pub struct Sbe<V, E>(pub Vec<V>, pub Vec<E>);

impl<T, E> FromIterator<Result<T, E>> for Sbe<T, E> {
    fn from_iter<I: IntoIterator<Item=Result<T, E>>>(iter: I) -> Self {
        let mut results = Vec::new();
        let mut errors = Vec::new();
        for result in iter {
            match result {
                Ok(value) => results.push(value),
                Err(error) => errors.push(error)
            }
        }
        Sbe(results, errors)
    }
}

impl<T, E> FromIterator<Result<T, Vec<E>>> for Sbe<T, E> {
    fn from_iter<I: IntoIterator<Item=Result<T, Vec<E>>>>(iter: I) -> Self {
        let mut results = Vec::new();
        let mut errors = Vec::new();
        for result in iter {
            match result {
                Ok(value) => results.push(value),
                Err(mut error) => errors.append(&mut error)
            }
        }
        Sbe(results, errors)
    }
}