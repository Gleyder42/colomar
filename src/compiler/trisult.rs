use smallvec::{Array, SmallVec};

use crate::compiler::span::{Span, Spanned};
use std::fmt::Debug;

/// Trisult is similar to [Result] but has one more in-between state.
/// These states are
/// - [Trisult::Ok], like [Result::Ok]
/// - [Trisult::Par], like [Result::Ok] and [Result::Err] combined. It has a value and errors
/// - [Trisult::Err], like [Result::Err] but errors are always stored in a vec
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Trisult<T, E> {
    /// Contains the success value
    Ok(T),
    /// Contains the partial success value and the error values
    Par(T, Vec<E>),
    /// Contains the error values
    Err(Vec<E>),
}

impl<T: Debug, E: Debug> Trisult<T, E> {
    pub fn debug_print(self) -> Trisult<T, E> {
        println!("Current Trisult: {:#?}", self);
        self
    }

    pub fn debug_print_labeled(self, label: impl Debug) -> Trisult<T, E> {
        println!("{:?}: Current Trisult: {:#?}", label, self);
        self
    }
}

impl<T, E: Debug> Trisult<T, E> {
    pub fn unwrap_ok(self) -> T {
        match self {
            Trisult::Ok(ok) => ok,
            Trisult::Par(_, _) => panic!("Trying to unwrap Ok, but was Par"),
            Trisult::Err(err) => panic!("Trying to unwrap Ok, but was Err {:?}", err),
        }
    }
}

impl<T, E> Trisult<T, E> {
    pub fn inner_into_some(self) -> Trisult<Option<T>, E> {
        self.map(|value| Some(value))
    }

    pub fn spanned(self, span: Span) -> Trisult<Spanned<T>, E> {
        self.map(|value| Spanned::new(value, span))
    }

    pub fn new(value: T, errors: Vec<E>) -> Trisult<T, E> {
        if errors.is_empty() {
            Trisult::Ok(value)
        } else {
            Trisult::Par(value, errors)
        }
    }

    pub fn assume_ok(self) -> Trisult<T, E> {
        match self {
            ok @ Trisult::Ok(_) => ok,
            Trisult::Par(_, errors) => Trisult::Err(errors),
            err @ Trisult::Err(_) => err,
        }
    }

    pub fn map_errors<L>(self, func: impl Fn(E) -> L) -> Trisult<T, L> {
        match self {
            Trisult::Ok(value) => Trisult::Ok(value),
            Trisult::Par(value, errors) => {
                Trisult::Par(value, errors.into_iter().map(func).collect())
            }
            Trisult::Err(errors) => Trisult::Err(errors.into_iter().map(func).collect()),
        }
    }

    pub fn drop_errors(self, fallback: impl Fn() -> T) -> Trisult<T, E> {
        match self {
            Trisult::Ok(_) => self,
            Trisult::Par(value, _) => Trisult::Ok(value),
            Trisult::Err(_) => Trisult::Ok(fallback()),
        }
    }

    pub fn and_only_errors<U>(self, other: Trisult<U, E>) -> Trisult<T, E> {
        let other_errors = match other {
            Trisult::Ok(_) => return self,
            Trisult::Par(_, errors) | Trisult::Err(errors) => errors,
        };

        self.flat_map(|value| Trisult::Par(value, other_errors))
    }

    /// Converts the [Trisult] to an option and error vec.
    /// The option is none if [Trisult::Err] otherwise none.
    /// The error vec contains errors, if any.
    pub fn to_option(self) -> (Option<T>, Vec<E>) {
        match self {
            Trisult::Ok(value) => (Some(value), Vec::new()),
            Trisult::Par(value, errors) => (Some(value), errors),
            Trisult::Err(errors) => (None, errors),
        }
    }

    /// Combines this result's value [T] with another result's value [O].
    ///
    /// # Notes
    /// * If this result is [Trisult::Err] the combined result will also be [Trisult::Err]
    /// * If the other result is [Trisult::Err] the combined result will be [Trisult::Par]
    /// and use the default value of [O]
    pub fn and_or_default<O: Default>(self, other: Trisult<O, E>) -> Trisult<(T, O), E> {
        self.and_or(other, O::default())
    }

    pub fn and_or<O>(self, other: Trisult<O, E>, default: O) -> Trisult<(T, O), E> {
        self.and(
            |value, errors| Trisult::Par((value, default), errors),
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

    pub fn map_and_require<O, F>(self, func: F) -> Trisult<(T, O), E>
    where
        F: FnOnce(&T) -> Trisult<O, E>,
    {
        self.flat_map(|t| func(&t).map(|o| (t, o)))
    }

    /// Combines this result's value [T] with another result's value [O].
    ///
    /// # Notes
    /// * If this result is [Trisult::Err] the combined result will also be [Trisult::Err]
    /// * If the other result is [Trisult::Err] the recovery function defines the combined result
    fn and<O, F>(self, recovery: F, other: Trisult<O, E>) -> Trisult<(T, O), E>
    where
        F: FnOnce(T, Vec<E>) -> Trisult<(T, O), E>,
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

    /// Combines this result's value [T] with another result's value [O].
    ///
    /// - If the supplied option is none, the other option of type [O] will also be none.
    /// - If the supplied option is some and the result is [Trisult::Ok] or [Trisult::Par], the
    /// other option will also be some.
    /// -  If the supplied option is some and the result is [Trisult::Err], the other option will be
    /// none
    pub fn and_maybe<O>(self, option: Option<Trisult<O, E>>) -> Trisult<(T, Option<O>), E> {
        match option {
            Some(result) => self.and(
                |value, errors| Trisult::Par((value, None), errors),
                result.map(|it| Some(it)),
            ),
            None => self.map(|it| (it, None)),
        }
    }

    /// Maps this result's value [T] to another value [O].
    pub fn map<U, F: FnOnce(T) -> U>(self, func: F) -> Trisult<U, E> {
        match self {
            Trisult::Ok(value) => Trisult::Ok(func(value)),
            Trisult::Par(value, errors) => Trisult::Par(func(value), errors),
            Trisult::Err(errors) => Trisult::Err(errors),
        }
    }

    pub fn if_ok(self, func: impl FnOnce(T)) {
        if let Trisult::Ok(value) = self {
            func(value)
        }
    }

    pub fn validate(self, validator: impl FnOnce(&T) -> Vec<E>) -> Trisult<T, E> {
        self.flat_map(|t| {
            let errors = validator(&t);
            Trisult::new(t, errors)
        })
    }

    /// Maps this result's value [T] to another value [O], but flattens the inner [Trisult].
    /// The mapping function also returns a [Trisult].
    /// Errors from this trisult and the mapped trisult are combined.
    /// If the mapped trisult is [Trisult::Err] the current result will also be [Trisult::Err],
    /// dropping the current value.
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
            Trisult::Err(errors) => Trisult::Err(errors),
        }
    }
}

impl<T, I: IntoIterator<Item = T>, E> Trisult<I, E> {
    /// Folds all elements of the current [Trisult] while having a context.
    /// The context is not part of the accumulator.
    ///
    /// # Example
    /// The function adds all elements together and multiplies each step by the context.
    /// - (0 + 1) * 1 = 1
    /// - (1 + 2) * 2 = 6
    /// - (6 + 3) * 3 = 27
    ///
    /// ```
    /// let trisult = QueryTrisult::Ok(vec![1, 2, 3])
    ///         .fold_with(1, 0, |ctx, acc, current| {
    ///             QueryTrisult::Ok((ctx + 1, (acc + current) * ctx))
    ///         });
    ///
    /// assert_eq!(trisult.to_option().0.unwrap(), 27);
    /// ```
    pub fn fold_with<C, A, F>(self, initial_ctx: C, initial: A, fold_fn: F) -> Trisult<A, E>
    where
        F: Fn(C, A, T) -> Trisult<(C, A), E>,
    {
        self.fold_flat_map(
            (initial_ctx, initial),
            |(_, acc)| acc,
            |(ctx, acc), t| fold_fn(ctx, acc, t),
        )
    }
    pub fn fold<A, F>(self, initial: A, func: F) -> Trisult<A, E>
    where
        F: Fn(A, T) -> Trisult<A, E>,
    {
        self.fold_flat_map(initial, |it| it, func)
    }

    pub fn fold_flat_map<U, A, F, M>(self, initial: A, map_func: M, mut func: F) -> Trisult<U, E>
    where
        F: FnMut(A, T) -> Trisult<A, E>,
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

            if errors.is_empty() {
                Trisult::Ok(map_func(current))
            } else {
                Trisult::Par(map_func(current), errors)
            }
        })
    }

    pub fn map_inner<F, U, Iu>(self, func: F) -> Trisult<Iu, E>
    where
        F: Fn(T) -> U,
        Iu: IntoIterator<Item = U> + FromIterator<U>,
    {
        self.map(|iter| iter.into_iter().map(func).collect::<Iu>())
    }
}

impl<E> Trisult<(), E> {
    /// Creates a [Trisult::Ok] with an empty type
    pub fn empty() -> Trisult<(), E> {
        Trisult::Ok(())
    }

    pub fn start<U>(self, func: impl FnOnce() -> U) -> Trisult<U, E> {
        self.map(|_| func())
    }

    pub fn flat_start<U>(self, func: impl FnOnce() -> Trisult<U, E>) -> Trisult<U, E> {
        self.flat_map(|_| func())
    }
}

pub trait IntoTrisult<T, E> {
    fn trisult_ok_or(self, error: E) -> Trisult<T, E>;
}

impl<T, E> IntoTrisult<T, E> for Option<T> {
    fn trisult_ok_or(self, error: E) -> Trisult<T, E> {
        match self {
            Some(value) => Trisult::Ok(value),
            None => Trisult::Err(vec![error]),
        }
    }
}

impl<T, E> From<Result<Trisult<T, E>, E>> for Trisult<T, E> {
    fn from(value: Result<Trisult<T, E>, E>) -> Self {
        match value {
            Ok(query_result) => query_result,
            Err(error) => Trisult::Err(vec![error]),
        }
    }
}

impl<T, E> TryFrom<(Option<T>, Vec<E>)> for Trisult<T, E> {
    type Error = &'static str;

    fn try_from(value: (Option<T>, Vec<E>)) -> Result<Self, Self::Error> {
        let trisult = match value {
            (Some(value), errors) if errors.is_empty() => Trisult::Ok(value),
            (Some(value), errors) => Trisult::Par(value, errors),
            (None, errors) if errors.is_empty() => {
                return Err("If option is none, errors must have at least one value");
            }
            (None, errors) => Trisult::Err(errors),
        };
        Ok(trisult)
    }
}

impl<T, E> From<Result<T, E>> for Trisult<T, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(value) => Trisult::Ok(value),
            Err(error) => Trisult::Err(vec![error]),
        }
    }
}

impl<T, E> FromIterator<Result<T, E>> for Trisult<Vec<T>, E> {
    fn from_iter<I: IntoIterator<Item = Result<T, E>>>(iter: I) -> Self {
        let mut results = Vec::new();
        let mut errors = Vec::new();

        for result in iter {
            match result {
                Ok(value) => results.push(value),
                Err(error) => errors.push(error),
            }
        }

        from_vec_results(results, errors)
    }
}

impl<T, A: Array<Item = T>, E> FromIterator<Trisult<T, E>> for Trisult<SmallVec<A>, E> {
    fn from_iter<I: IntoIterator<Item = Trisult<T, E>>>(iter: I) -> Self {
        let (results, errors) = from_iter_trisults(iter);

        from_small_vec_results(results, errors)
    }
}

impl<T, E> FromIterator<Trisult<T, E>> for Trisult<Vec<T>, E> {
    fn from_iter<I: IntoIterator<Item = Trisult<T, E>>>(iter: I) -> Self {
        let (results, errors) = from_iter_trisults(iter);

        from_vec_results(results, errors)
    }
}

impl<T, E> FromIterator<T> for Trisult<Vec<T>, E> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let results: Vec<T> = iter.into_iter().collect();
        from_vec_results(results, Vec::new())
    }
}

fn from_iter_trisults<T, E, I: IntoIterator<Item = Trisult<T, E>>>(iter: I) -> (Vec<T>, Vec<E>) {
    let mut results = Vec::new();
    let mut errors = Vec::new();
    for result in iter {
        match result {
            Trisult::Ok(value) => results.push(value),
            Trisult::Par(value, mut result_errors) => {
                results.push(value);
                errors.append(&mut result_errors);
            }
            Trisult::Err(mut result_errors) => errors.append(&mut result_errors),
        }
    }
    (results, errors)
}

impl<T, E> From<(Vec<T>, Vec<E>)> for Trisult<Vec<T>, E> {
    fn from(value: (Vec<T>, Vec<E>)) -> Self {
        from_vec_results(value.0, value.1)
    }
}

impl<T: Array, E> From<(SmallVec<T>, Vec<E>)> for Trisult<SmallVec<T>, E> {
    fn from(value: (SmallVec<T>, Vec<E>)) -> Self {
        from_small_vec_results(value.0, value.1)
    }
}

/// Creates a [Trisult] from a result and error [Vec].
/// The result is
/// * [Trisult::Ok], if no errors are found
/// * [Trisult::Par], if errors are found.
fn from_vec_results<T, E>(results: Vec<T>, errors: Vec<E>) -> Trisult<Vec<T>, E> {
    match errors.is_empty() {
        true => Trisult::Ok(results),
        false => Trisult::Par(results, errors),
    }
}

fn from_small_vec_results<T, A, E>(
    results: impl Into<SmallVec<A>>,
    errors: Vec<E>,
) -> Trisult<SmallVec<A>, E>
where
    A: Array<Item = T>,
{
    match errors.is_empty() {
        true => Trisult::Ok(results.into()),
        false => Trisult::Par(results.into(), errors),
    }
}

#[macro_export]
macro_rules! query_error {
    ($($x:expr),+ $(,)?) => {
        $crate::compiler::trisult::Trisult::Err(vec![$($x),+])
    };
}
