use std::fmt::Debug;
use crate::language::{Ident};
use crate::language::analysis::interner::{Interner, IntoInternId};

enum Meter {
    Empty,
    Filled
}

impl Meter {

    fn measure<T, E>(results: &Vec<T>, errors: &Vec<E>) -> (Meter, Meter) {
        (
            if results.is_empty() { Meter::Empty } else { Meter::Filled },
            if errors.is_empty() { Meter::Empty } else { Meter::Filled }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum QueryResult<T, E> {
    Ok(T),
    Par(T, Vec<E>),
    Err(Vec<E>)
}

impl<T, E> QueryResult<T, E> {

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

        QueryResult::from_results(results, errors)
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
                },
                QueryResult::Err(mut result_errors) => errors.append(&mut result_errors)
            }
        }

        QueryResult::from_results(results, errors)
    }
}

impl<T, E> QueryResult<Vec<T>, E> {

    fn from_results(results: Vec<T>, errors: Vec<E>) -> QueryResult<Vec<T>, E> {
        use Meter::*;
        match Meter::measure(&results, &errors) {
            (Empty, Empty) => QueryResult::Ok(Vec::<T>::new()),
            (Filled, Empty) => QueryResult::Ok(results),
            (Empty, Filled) => QueryResult::Err(errors),
            (Filled, Filled) => QueryResult::Par(results, errors)
        }
    }
}

impl<T, E> QueryResult<T, E> {

    pub fn map<U, F: FnOnce(T)->U>(self, func: F) -> QueryResult<U, E> {
        match self {
            QueryResult::Ok(value) => QueryResult::Ok(func(value)),
            QueryResult::Par(value, errors) => QueryResult::Par(func(value), errors),
            QueryResult::Err(errors) => QueryResult::Err(errors)
        }
    }

    pub fn then<U, F: FnOnce(T)->QueryResult<U,E>>(self, func: F) -> QueryResult<U, E> {
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
        second: Ident
    },
    CannotFindIdent(Ident),
    WrongType
}

impl<T> Into<Result<T, AnalysisError>> for AnalysisError {
    fn into(self) -> Result<T, AnalysisError> {
        Err(self)
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