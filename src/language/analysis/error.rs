use std::collections::HashSet;
use std::fmt::Debug;
use std::hint;
use crate::language::{Ident};
use crate::language::analysis::interner::{Interner, IntoInternId};

enum Meter {
    Empty,
    Filled,
}

enum Measurable<T> {
    Vec(Vec<T>),
    Set(HashSet<T>),
}

impl<T> Measurable<T> {
    fn is_empty(&self) -> bool {
        match self {
            Measurable::Vec(vec) => vec.is_empty(),
            Measurable::Set(set) => set.is_empty()
        }
    }

    unsafe fn unwrap_vec_unchecked(self) -> Vec<T> {
        match self {
            Measurable::Vec(vec) => vec,
            Measurable::Set(_) => unsafe { hint::unreachable_unchecked() }
        }
    }

    unsafe fn unwrap_set_unchecked(self) -> HashSet<T> {
        match self {
            Measurable::Vec(_) => unsafe { hint::unreachable_unchecked() },
            Measurable::Set(set) => set
        }
    }

}

impl Meter {
    fn measure<T, E>(results: &Measurable<T>, errors: &Vec<E>) -> (Meter, Meter) {
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
    Err(Vec<E>),
}

impl<T, I: IntoIterator<Item=T>, E> QueryResult<I, E> {

    pub fn map_inner<F: Fn(T)->U, U>(self, func: F) -> QueryResult<Vec<U>, E> {
        self.map(|iter| iter.into_iter().map(|it| func(it)).collect::<Vec<U>>())
    }
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

        QueryResult::from_results_vec(results, errors)
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

        QueryResult::from_results_vec(results, errors)
    }
}

fn from_results<T, E, F, U>(
    measurable: Measurable<T>,
    errors: Vec<E>,
    func: F,
) -> QueryResult<U, E>
    where F: FnOnce(Measurable<T>) -> U
{
    use Meter::*;
    match Meter::measure(&measurable, &errors) {
        (Empty, Empty) => QueryResult::Ok(func(measurable)),
        (Filled, Empty) => QueryResult::Ok(func(measurable)),
        (Empty, Filled) => QueryResult::Err(errors),
        (Filled, Filled) => QueryResult::Par(func(measurable), errors)
    }
}

impl<T, E> QueryResult<Vec<T>, E> {

    fn from_results_vec(results: Vec<T>, errors: Vec<E>) -> QueryResult<Vec<T>, E> {
        // We can unwrap unchecked here, because we know Measurable is a vec
        let func = |it: Measurable<T>| unsafe { it.unwrap_vec_unchecked() };
        from_results(Measurable::Vec(results), errors, func)
    }
}

impl<T, E> From<(Vec<T>, Vec<E>)> for QueryResult<Vec<T>, E> {
    fn from(value: (Vec<T>, Vec<E>)) -> Self {
        QueryResult::from_results_vec(value.0, value.1)
    }
}

impl<T, E> QueryResult<HashSet<T>, E> {

    fn from_results_set(results: HashSet<T>, errors: Vec<E>) -> QueryResult<HashSet<T>, E> {
        // We can unwrap unchecked here, because we know Measurable is a set
        let func = |it: Measurable<T>| unsafe { it.unwrap_set_unchecked() };
        from_results(Measurable::Set(results), errors, func)
    }
}

impl<T, E> QueryResult<T, E> {
    pub fn map<U, F: FnOnce(T) -> U>(self, func: F) -> QueryResult<U, E> {
        match self {
            QueryResult::Ok(value) => QueryResult::Ok(func(value)),
            QueryResult::Par(value, errors) => QueryResult::Par(func(value), errors),
            QueryResult::Err(errors) => QueryResult::Err(errors)
        }
    }

    pub fn then<U, F: FnOnce(T) -> QueryResult<U, E>>(self, func: F) -> QueryResult<U, E> {
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
    CannotFindIdent(Ident),
    WrongType,
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