use std::collections::HashSet;
use std::fmt::Debug;
use crate::language::{ast, Ident};
use crate::language::analysis::interner::{Interner, IntoInternId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum QueryResult<T, E> {
    Ok(T),
    Par(T, Vec<E>),
    Err(Vec<E>),
}

impl<Id, T: IntoInternId<Interned=Id>, I: IntoIterator<Item=T>, E> QueryResult<I, E> {
    pub fn intern_inner<Db: Interner + ?Sized>(self, db: &Db) -> QueryResult<Vec<Id>, E> {
        self.map_inner(|t| t.intern(db))
    }
}


impl<T, I: IntoIterator<Item=T>, E> QueryResult<I, E> {

    pub fn fold<A, F>(self, initial: A, func: F) -> QueryResult<A, E>
        where F: Fn(A, T) -> QueryResult<A, E>
    {
        self.fold_flat_map(initial, |it| it, func)
    }

    pub fn fold_flat_map<U, A, F, M>(self, initial: A, map_func: M, func: F) -> QueryResult<U, E>
        where F: Fn(A, T) -> QueryResult<A, E>,
              M: FnOnce(A) -> U,
    {
        self.flat_map(|iter| {
            let mut errors = Vec::new();
            let mut current = initial;

            for item in iter.into_iter() {
                let result = func(current, item);
                match result {
                    QueryResult::Ok(value) => current = value,
                    QueryResult::Par(value, mut result_errors) => {
                        current = value;
                        errors.append(&mut result_errors);
                    }
                    QueryResult::Err(mut result_errors) => {
                        errors.append(&mut result_errors);
                        return QueryResult::Err(errors);
                    }
                }
            }

            QueryResult::Ok(map_func(current))
        })
    }

    pub fn map_inner<F: Fn(T) -> U, U>(self, func: F) -> QueryResult<Vec<U>, E> {
        self.map(|iter| iter.into_iter().map(|it| func(it)).collect::<Vec<U>>())
    }
}

impl<T, E> QueryResult<T, E> {

    pub fn from_option(option: Option<T>, error: E) -> QueryResult<T, E> {
        match option {
            Some(value) => QueryResult::Ok(value),
            None => QueryResult::Err(vec![error])
        }
    }

    pub fn to_option(self) -> (Option<T>, Vec<E>) {
        match self {
            QueryResult::Ok(value) => (Some(value), Vec::new()),
            QueryResult::Par(value, errors) => (Some(value), errors),
            QueryResult::Err(errors) => (None, errors)
        }
    }
}

impl<Id, T: IntoInternId<Interned=Id>, E> QueryResult<T, E> {
    pub fn intern<Db: Interner + ?Sized>(self, db: &Db) -> QueryResult<Id, E> {
        self.map(|it| it.intern(db))
    }
}

impl<E> From<ast::CallChain> for QueryResult<ast::CallChain, E> {
    fn from(value: ast::CallChain) -> Self {
        QueryResult::Ok(value)
    }
}

impl<T, E> From<Result<QueryResult<T, E>, E>> for QueryResult<T, E> {
    fn from(value: Result<QueryResult<T, E>, E>) -> Self {
        match value {
            Ok(query_result) => query_result,
            Err(error) => QueryResult::Err(vec![error])
        }
    }
}

impl<T, E> From<Result<T, E>> for QueryResult<T, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(value) => QueryResult::Ok(value),
            Err(error) => QueryResult::Err(vec![error])
        }
    }
}

impl<T, E> FromIterator<Result<T, E>> for QueryResult<Vec<T>, E> {
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


impl<T, E> FromIterator<QueryResult<T, E>> for QueryResult<Vec<T>, E> {
    fn from_iter<I: IntoIterator<Item=QueryResult<T, E>>>(iter: I) -> Self {
        let mut results = Vec::new();
        let mut errors = Vec::new();
        for result in iter {
            match result {
                QueryResult::Ok(value) => results.push(value),
                QueryResult::Par(value, mut result_errors) => {
                    results.push(value);
                    errors.append(&mut result_errors);
                }
                QueryResult::Err(mut result_errors) => errors.append(&mut result_errors)
            }
        }

        from_results(results, errors)
    }
}

/// Creates a [QueryResult] from a result and error [Vec].
/// The result is
/// * [QueryResult::Ok], if no errors are found
/// * [QueryResult::Par], if errors are found.
fn from_results<T, E>(results: Vec<T>, errors: Vec<E>) -> QueryResult<Vec<T>, E> {
    match errors.is_empty() {
        true => QueryResult::Ok(results),
        false => QueryResult::Par(results, errors)
    }
}

impl<T, E> From<(Vec<T>, Vec<E>)> for QueryResult<Vec<T>, E> {
    fn from(value: (Vec<T>, Vec<E>)) -> Self {
        from_results(value.0, value.1)
    }
}

impl<E> QueryResult<(), E> {
    pub fn empty() -> QueryResult<(), E> {
        QueryResult::Ok(())
    }
}

#[macro_export]
macro_rules! query_error {
    ($($x:expr),+ $(,)?) => {
        QueryResult::Err(vec![$($x),+])
    };
}

impl<T: Clone, E> QueryResult<T, E> {

    pub fn map_and_require<O, F>(self, func: F) -> QueryResult<(T, O), E>
        where F: FnOnce(T) -> QueryResult<O, E>
    {
        self.flat_map(|t| func(t.clone()).map(|o| (t, o)))
    }
}

impl<T, E> QueryResult<T, E> {
    pub fn maybe_add_error(self, option: Option<E>) -> QueryResult<T, E> {
        match option {
            Some(error) => self.add_error(error),
            None => self
        }
    }

    pub fn add_error(self, error: E) -> QueryResult<T, E> {
        match self {
            QueryResult::Ok(value) => QueryResult::Par(value, vec![error]),
            QueryResult::Par(value, mut errors) => {
                errors.push(error);
                QueryResult::Par(value, errors)
            }
            QueryResult::Err(mut errors) => {
                errors.push(error);
                QueryResult::Err(errors)
            }
        }
    }

    /// Combines this result's value [T] with another result's value [O].
    ///
    /// # Notes
    /// * If this result is [QueryResult::Err] the combined result will also be [QueryResult::Err]
    /// * If the other result is [QueryResult::Err] the combined result will be [QueryResult::Par]
    /// and use the default value of [O]
    pub fn and_or_default<O: Default>(self, other: QueryResult<O, E>) -> QueryResult<(T, O), E> {
        self.and(
            |value, errors| QueryResult::Par((value, O::default()), errors),
            other
        )
    }

    /// Combines this result's value [T] with another result's value [O].
    ///
    /// # Notes
    /// * If this result or the other result is [QueryResult::Err] the combined result will also be [QueryResult::Err]
    pub fn and_require<O>(self, other: QueryResult<O, E>) -> QueryResult<(T, O), E> {
        self.and(|_value, errors| QueryResult::Err(errors), other)
    }

    /// Combines this result's value [T] with another result's value [O].
    ///
    /// # Notes
    /// * If this result is [QueryResult::Err] the combined result will also be [QueryResult::Err]
    /// * If the other result is [QueryResult::Err] the recovery function defines the combined result
    fn and<O, F>(self, recovery: F, other: QueryResult<O, E>) -> QueryResult<(T, O), E>
        where F: FnOnce(T, Vec<E>) -> QueryResult<(T, O), E>,
    {
        use QueryResult::*;

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
        option: Option<QueryResult<O, E>>,
    ) -> QueryResult<(T, Option<O>), E> {
        match option {
            Some(result) => {
                self.and(
                    |value, errors| QueryResult::Par((value, None), errors),
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
    ) -> QueryResult<U, E>
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

    pub fn map<U, F: FnOnce(T) -> U>(self, func: F) -> QueryResult<U, E> {
        match self {
            QueryResult::Ok(value) => QueryResult::Ok(func(value)),
            QueryResult::Par(value, errors) => QueryResult::Par(func(value), errors),
            QueryResult::Err(errors) => QueryResult::Err(errors)
        }
    }

    pub fn flat_map<U, F: FnOnce(T) -> QueryResult<U, E>>(self, func: F) -> QueryResult<U, E> {
        match self {
            QueryResult::Ok(value) => func(value),
            QueryResult::Par(value, mut errors) => {
                let result = func(value);
                match result {
                    QueryResult::Ok(value) => QueryResult::Par(value, errors),
                    QueryResult::Par(value, mut mapped_errors) => {
                        errors.append(&mut mapped_errors);
                        QueryResult::Par(value, errors)
                    }
                    QueryResult::Err(mut mapped_errors) => {
                        errors.append(&mut mapped_errors);
                        QueryResult::Err(errors)
                    }
                }
            }
            QueryResult::Err(errors) => QueryResult::Err(errors)
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
}

impl<T> Into<Result<T, AnalysisError>> for AnalysisError {
    fn into(self) -> Result<T, AnalysisError> {
        Err(self)
    }
}

impl<T> Into<QueryResult<T, AnalysisError>> for AnalysisError {
    fn into(self) -> QueryResult<T, AnalysisError> {
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