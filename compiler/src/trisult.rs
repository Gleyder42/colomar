use super::span::{Span, Spanned};
use std::fmt::Debug;
use std::hash::Hash;

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
    Par(T, NonEmptyVec<E>),
    /// Contains the error values
    Err(NonEmptyVec<E>),
}

pub fn err<T, E>(error: E) -> Trisult<T, E> {
    Trisult::Err(NonEmptyVec::new(error))
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

    pub fn expect_ok(self, message: &'static str) -> T {
        match self {
            Trisult::Ok(value) => value,
            Trisult::Par(_, errors) => panic!("Expected Ok, but was Par {:?}: {}", errors, message),
            Trisult::Err(errors) => panic!("Expected Ok, but was Err {:?}: {}", errors, message),
        }
    }
}

impl<T, E> Trisult<Trisult<T, E>, E> {
    pub fn flatten(self) -> Trisult<T, E> {
        match self {
            Trisult::Ok(value) => value,
            Trisult::Par(value, errors) => value.merge_errors(Errors::from(errors)),
            Trisult::Err(errors) => Trisult::Err(errors),
        }
    }
}

impl<T, E> Trisult<T, E> {
    pub fn fail_when_par(self) -> Trisult<T, E> {
        match self {
            Trisult::Ok(value) => Trisult::Ok(value),
            Trisult::Par(_, errors) => Trisult::Err(errors),
            err @ Trisult::Err(_) => err,
        }
    }

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
            Trisult::Par(
                value,
                NonEmptyVec::try_from(errors).expect("Errors are already checked to be not empty"),
            )
        }
    }

    pub fn assume_ok(self) -> Trisult<T, E> {
        match self {
            ok @ Trisult::Ok(_) => ok,
            Trisult::Par(_, errors) => Trisult::Err(errors),
            err @ Trisult::Err(_) => err,
        }
    }

    pub fn merge_errors(self, errors: Errors<E>) -> Trisult<T, E> {
        match self {
            ok @ Trisult::Ok(_) => ok,
            Trisult::Par(value, other_errors) => {
                Trisult::Par(value, other_errors.append_vec(errors.vec))
            }
            Trisult::Err(other_errors) => Trisult::Err(other_errors.append_vec(errors.vec)),
        }
    }

    pub fn map_each_error<L>(self, func: impl Fn(E) -> L) -> Trisult<T, L> {
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
            Trisult::Par(value, errors) => (Some(value), errors.into()),
            Trisult::Err(errors) => (None, errors.into()),
        }
    }

    pub fn soft_filter_with<W, F>(self, with: Trisult<W, E>, filter: F) -> Trisult<T, E>
    where
        F: FnOnce(&T, W) -> Result<(), E>,
    {
        self.and(with)
            .flat_map(|(value, with)| match filter(&value, with) {
                Ok(_) => Trisult::Ok(value),
                Err(error) => Trisult::Par(value, NonEmptyVec::new(error)),
            })
    }

    pub fn soft_filter<F: FnOnce(&T) -> Result<(), E>>(self, filter: F) -> Trisult<T, E> {
        self.flat_map(|value| match filter(&value) {
            Ok(_) => Trisult::Ok(value),
            Err(error) => Trisult::Par(value, NonEmptyVec::new(error)),
        })
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

    /// Combines this result's value [T] with another result's value [O].
    /// If other result is [Trisult::Err] use the default value, supplied to the function
    pub fn and_or<O>(self, other: Trisult<O, E>, default: O) -> Trisult<(T, O), E> {
        self.and_or_recover(
            |value, errors| Trisult::Par((value, default), errors),
            other,
        )
    }

    /// Combines this result's value [T] with another result's value [O].
    ///
    /// # Notes
    /// * If this result or the other result is [Trisult::Err] the combined result will also be [Trisult::Err]
    pub fn and<O>(self, other: Trisult<O, E>) -> Trisult<(T, O), E> {
        self.and_or_recover(|_value, errors| Trisult::Err(errors), other)
    }

    pub fn and_with<O, F>(self, func: F) -> Trisult<(T, O), E>
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
    fn and_or_recover<O, F>(self, recovery: F, other: Trisult<O, E>) -> Trisult<(T, O), E>
    where
        F: FnOnce(T, NonEmptyVec<E>) -> Trisult<(T, O), E>,
    {
        use Trisult::*;

        match (self, other) {
            (Ok(value), Ok(other_value)) => Ok((value, other_value)),
            (Ok(value), Par(other_value, other_errors)) => Par((value, other_value), other_errors),
            (Ok(value), Err(errors)) => recovery(value, errors),
            (Par(value, errors), Ok(other_value)) => Par((value, other_value), errors),
            (Par(value, errors), Par(other_value, other_errors)) => {
                Par((value, other_value), errors.append(other_errors))
            }
            (Par(value, errors), Err(other_errors)) => recovery(value, errors.append(other_errors)),
            (Err(errors), Ok(_other_value)) => Err(errors),
            (Err(errors), Par(_other_value, other_errors)) => Err(errors.append(other_errors)),
            (Err(errors), Err(other_errors)) => Err(errors.append(other_errors)),
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
            Some(result) => self.and_or_recover(
                |value, errors| Trisult::Par((value, None), errors),
                result.map(|it| Some(it)),
            ),
            None => self.map(|it| (it, None)),
        }
    }

    pub fn to<U, F: FnOnce() -> U>(self, func: F) -> Trisult<U, E> {
        self.map(|_| func())
    }

    pub fn helper<H, F>(self, helper: Trisult<H, E>, func: F) -> Trisult<T, E>
    where
        F: FnOnce(Trisult<(T, H), E>) -> Trisult<T, E>,
    {
        let trisult = self.and(helper);
        func(trisult)
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

    /// Maps this result's value [T] to another value [O].
    pub fn map<U, F: FnOnce(T) -> U>(self, func: F) -> Trisult<U, E> {
        match self {
            Trisult::Ok(value) => Trisult::Ok(func(value)),
            Trisult::Par(value, errors) => Trisult::Par(func(value), errors),
            Trisult::Err(errors) => Trisult::Err(errors),
        }
    }

    pub fn map_error<U>(
        self,
        func: impl FnOnce(NonEmptyVec<E>) -> NonEmptyVec<U>,
    ) -> Trisult<T, U> {
        match self {
            Trisult::Ok(value) => Trisult::Ok(value),
            Trisult::Par(value, errors) => Trisult::Par(value, func(errors)),
            Trisult::Err(errors) => Trisult::Err(func(errors)),
        }
    }

    /// Maps this result's value [T] to another value [O], but flattens the inner [Trisult].
    /// The mapping function also returns a [Trisult].
    /// Errors from this trisult and the mapped trisult are combined.
    /// If the mapped trisult is [Trisult::Err] the current result will also be [Trisult::Err],
    /// dropping the current value.
    pub fn flat_map<U, F: FnOnce(T) -> Trisult<U, E>>(self, func: F) -> Trisult<U, E> {
        match self {
            Trisult::Ok(value) => func(value),
            Trisult::Par(value, errors) => {
                let result = func(value);
                match result {
                    Trisult::Ok(value) => Trisult::Par(value, errors),
                    Trisult::Par(value, mapped_errors) => {
                        Trisult::Par(value, errors.append(mapped_errors))
                    }
                    Trisult::Err(mapped_errors) => Trisult::Err(errors.append(mapped_errors)),
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
    /// use compiler::trisult::Trisult;
    ///
    /// let trisult: Trisult<_, &str> = Trisult::Ok(vec![1, 2, 3])
    ///         .fold_with(1, 0, |ctx, acc, current| {
    ///             Trisult::Ok((ctx + 1, (acc + current) * ctx))
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

    pub fn reduce<C, F, A>(self, initial: A, mut func: F) -> Trisult<Vec<C>, E>
    where
        F: FnMut(A, T) -> Trisult<(A, Option<C>), E>,
    {
        self.flat_map(|iter| {
            let mut errors = Vec::new();
            let mut values = Vec::new();
            let mut current = initial;

            for item in iter.into_iter() {
                let result = func(current, item);
                match result {
                    Trisult::Ok((acc, new_value)) => {
                        current = acc;
                        new_value.map(|it| values.push(it));
                    }
                    Trisult::Par((acc, new_value), result_errors) => {
                        current = acc;
                        new_value.map(|it| values.push(it));
                        result_errors.add_to_vec(&mut errors);
                    }
                    Trisult::Err(result_errors) => {
                        return Trisult::Err(result_errors.with(errors));
                    }
                }
            }

            Trisult::new(values, errors)
        })
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
                    Trisult::Par(value, result_errors) => {
                        current = value;
                        result_errors.add_to_vec(&mut errors);
                    }
                    Trisult::Err(result_errors) => {
                        return Trisult::Err(result_errors.with(errors));
                    }
                }
            }

            Trisult::new(map_func(current), errors)
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

    pub fn start<U>(func: impl FnOnce() -> U) -> Trisult<U, E> {
        Trisult::empty().map(|_| func())
    }

    pub fn flat_start<U>(func: impl FnOnce() -> Trisult<U, E>) -> Trisult<U, E> {
        Trisult::empty().flat_map(|_| func())
    }
}

pub trait IntoTrisult<T, E> {
    fn trisult_ok_or(self, error: E) -> Trisult<T, E>;

    fn trisult_ok_or_else(self, error: impl Fn() -> E) -> Trisult<T, E>;
}

impl<T, E> IntoTrisult<T, E> for Option<T> {
    fn trisult_ok_or(self, error: E) -> Trisult<T, E> {
        match self {
            Some(value) => Trisult::Ok(value),
            None => err(error),
        }
    }

    fn trisult_ok_or_else(self, error: impl Fn() -> E) -> Trisult<T, E> {
        match self {
            Some(value) => Trisult::Ok(value),
            None => err(error()),
        }
    }
}

impl<T, E> From<Result<Trisult<T, E>, E>> for Trisult<T, E> {
    fn from(value: Result<Trisult<T, E>, E>) -> Self {
        match value {
            Ok(query_result) => query_result,
            Err(error) => err(error),
        }
    }
}

impl<T, E> TryFrom<(Option<T>, Vec<E>)> for Trisult<T, E> {
    type Error = &'static str;

    fn try_from(value: (Option<T>, Vec<E>)) -> Result<Self, Self::Error> {
        const ERROR_MESSAGE: &str = "Errors are already checked to be not empty";

        let is_empty = value.1.is_empty();

        let trisult = match (value.0, value.1, is_empty) {
            (Some(value), _, true) => Trisult::Ok(value),
            (Some(value), errors, false) => {
                Trisult::Par(value, NonEmptyVec::try_from(errors).expect(ERROR_MESSAGE))
            }
            (None, errors, false) => {
                Trisult::Err(NonEmptyVec::try_from(errors).expect(ERROR_MESSAGE))
            }
            (None, _, true) => {
                return Err("If option is none, errors must have at least one value")
            }
        };

        Ok(trisult)
    }
}

impl<T, E> From<Result<T, E>> for Trisult<T, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(value) => Trisult::Ok(value),
            Err(error) => err(error),
        }
    }
}

impl<R, T, E, C> FromIterator<R> for Trisult<C, E>
where
    R: Into<Trisult<T, E>>,
    C: IntoIterator<Item = T> + FromIterator<T> + Extend<T> + Default,
{
    fn from_iter<I: IntoIterator<Item = R>>(iter: I) -> Self {
        let mut values = C::default();
        let mut errors: Vec<E> = Vec::new();

        for r in iter {
            let result = r.into();
            match result {
                Trisult::Ok(value) => values.extend(Some(value)),
                Trisult::Par(value, other_errors) => {
                    values.extend(Some(value));
                    errors.extend(other_errors);
                }
                Trisult::Err(other_errors) => errors.extend(other_errors),
            };
        }

        Trisult::from((values, errors))
    }
}

impl<T, E> From<(T, Vec<E>)> for Trisult<T, E> {
    fn from(value: (T, Vec<E>)) -> Self {
        if value.1.is_empty() {
            Trisult::Ok(value.0)
        } else {
            Trisult::Par(
                value.0,
                value
                    .1
                    .try_into()
                    .expect("Vec is already checked to be not empty"),
            )
        }
    }
}

pub trait ErrorHolder<E> {
    fn consume(self) -> Vec<E>;
}

#[derive(Debug)]
pub struct Errors<E> {
    vec: Vec<E>,
}

impl<E> From<NonEmptyVec<E>> for Errors<E> {
    fn from(value: NonEmptyVec<E>) -> Self {
        match value.0 {
            NonEmptyVecRepr::Vec(vec) => Errors { vec },
            NonEmptyVecRepr::Inline(inline) => Errors { vec: vec![inline] },
        }
    }
}

impl<E> From<Vec<E>> for Errors<E> {
    fn from(value: Vec<E>) -> Self {
        Errors { vec: value }
    }
}

impl<E> Errors<E> {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    /// Returns [Trisult::Ok] if no errors were collected and returns [Trisult::Par]
    /// if there were any errors.
    pub fn value<T>(self, value: T) -> Trisult<T, E> {
        if self.vec.is_empty() {
            Trisult::Ok(value)
        } else {
            Trisult::Par(
                value,
                self.vec
                    .try_into()
                    .expect("Vec is already checked to be not empty"),
            )
        }
    }

    /// Returns [Trisult::Err] with all previously collected errors.
    ///
    /// If you have one more error to add, use [Errors::fail] instead.
    pub fn fail_directly<T>(self) -> Trisult<T, E> {
        Trisult::Err(
            self.vec
                .try_into()
                .expect("Errors should at lease have one value"),
        )
    }

    pub fn par<T>(mut self, value: T, error: E) -> Trisult<T, E> {
        self.vec.push(error);
        Trisult::Par(
            value,
            self.vec
                .try_into()
                .expect("Vec is guaranteed to not be empty"),
        )
    }

    /// Returns [Trisult::Err] with all previously collected errors, but also
    /// adds one more error.
    pub fn fail<T>(mut self, error: E) -> Trisult<T, E> {
        self.vec.push(error);
        Trisult::Err(
            self.vec
                .try_into()
                .expect("Vec is guaranteed to not be empty"),
        )
    }
}
impl<E> Default for Errors<E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<E> Into<Vec<E>> for Errors<E> {
    fn into(self) -> Vec<E> {
        self.vec
    }
}

impl<E> ErrorHolder<E> for Errors<E> {
    fn consume(self) -> Vec<E> {
        self.vec
    }
}

impl<E> ErrorHolder<E> for NonEmptyVec<E> {
    fn consume(self) -> Vec<E> {
        self.into()
    }
}

impl<E> Errors<E> {
    pub fn append(&mut self, context: impl ErrorHolder<E>) {
        if self.vec.is_empty() {
            self.vec = context.consume();
        } else {
            self.vec.append(&mut context.consume())
        }
    }

    pub fn push(&mut self, error: E) {
        self.vec.push(error);
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum NonEmptyVecRepr<T> {
    Vec(Vec<T>),
    Inline(T),
}

/// An array which always has at least one element.
/// A singular element does may not allocate.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NonEmptyVec<T>(NonEmptyVecRepr<T>);

impl<T> NonEmptyVec<T> {
    /// Combines this vec with another.
    /// Order is not preserved.
    /// If your other vec is not owned, use [NonEmptyVec::add_to_vec]
    pub fn with(self, mut other: Vec<T>) -> NonEmptyVec<T> {
        let repr = match self.0 {
            NonEmptyVecRepr::Vec(mut vec) => {
                other.append(&mut vec);
                std::mem::swap(&mut vec, &mut other);
                NonEmptyVecRepr::Vec(vec)
            }
            NonEmptyVecRepr::Inline(inline) => {
                other.push(inline);
                NonEmptyVecRepr::Vec(other)
            }
        };
        NonEmptyVec(repr)
    }

    /// Combines this vec with another.
    /// Order is not preserved.
    pub fn add_to_vec(self, other: &mut Vec<T>) {
        match self.0 {
            NonEmptyVecRepr::Vec(mut vec) => other.append(&mut vec),
            NonEmptyVecRepr::Inline(inline) => other.push(inline),
        }
    }

    /// Combines this vec with another.
    /// Order is not preserved.
    pub fn append_vec(self, mut other: Vec<T>) -> Self {
        let repr = match self.0 {
            NonEmptyVecRepr::Vec(mut vec) => {
                Self::merge_vec_into(&mut vec, other);
                NonEmptyVecRepr::Vec(vec)
            }
            NonEmptyVecRepr::Inline(inline) => {
                other.push(inline);
                NonEmptyVecRepr::Vec(other)
            }
        };
        NonEmptyVec(repr)
    }

    /// Combines this vec with another.
    /// Order is not preserved.
    pub fn append(self, other: Self) -> Self {
        let repr = match (self.0, other.0) {
            (NonEmptyVecRepr::Vec(mut vec), NonEmptyVecRepr::Vec(other_vec)) => {
                Self::merge_vec_into(&mut vec, other_vec);
                NonEmptyVecRepr::Vec(vec)
            }
            (NonEmptyVecRepr::Vec(mut vec), NonEmptyVecRepr::Inline(inline)) => {
                vec.push(inline);
                NonEmptyVecRepr::Vec(vec)
            }
            (NonEmptyVecRepr::Inline(inline), NonEmptyVecRepr::Vec(mut other_vec)) => {
                other_vec.push(inline);
                NonEmptyVecRepr::Vec(other_vec)
            }
            (NonEmptyVecRepr::Inline(inline), NonEmptyVecRepr::Inline(other_inline)) => {
                NonEmptyVecRepr::Vec(vec![inline, other_inline])
            }
        };
        NonEmptyVec(repr)
    }

    /// Merges two vecs into one.
    /// Appends the smaller vec to the larger one.
    fn merge_vec_into(out_vec: &mut Vec<T>, mut other_vec: Vec<T>) {
        if out_vec.len() > other_vec.len() {
            out_vec.append(&mut other_vec);
        } else {
            other_vec.append(out_vec);
            std::mem::swap(out_vec, &mut other_vec);
        }
    }
}

impl<T> FromIterator<T> for NonEmptyVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let vec: Vec<_> = iter.into_iter().collect(); // TODO can we avoid allocation here?
        if vec.is_empty() {
            panic!("Cannot collect an empty iterator into NonEmptyVec");
        }

        NonEmptyVec(NonEmptyVecRepr::Vec(vec))
    }
}

impl<T> IntoIterator for NonEmptyVec<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        match self.0 {
            NonEmptyVecRepr::Vec(iter) => iter.into_iter(),
            NonEmptyVecRepr::Inline(element) => vec![element].into_iter(), // TODO can we avoid an allocation here?
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct VecIsEmptyError;

impl<T> TryFrom<Errors<T>> for NonEmptyVec<T> {
    type Error = VecIsEmptyError;

    fn try_from(value: Errors<T>) -> Result<Self, Self::Error> {
        TryFrom::<Vec<_>>::try_from(value.vec)
    }
}

impl<T> TryFrom<Vec<T>> for NonEmptyVec<T> {
    type Error = VecIsEmptyError;

    fn try_from(value: Vec<T>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(VecIsEmptyError)
        } else {
            Ok(NonEmptyVec(NonEmptyVecRepr::Vec(value)))
        }
    }
}

impl<T> NonEmptyVec<T> {
    pub fn new(element: T) -> NonEmptyVec<T> {
        NonEmptyVec(NonEmptyVecRepr::Inline(element))
    }
}

impl<T> Into<Vec<T>> for NonEmptyVec<T> {
    fn into(self) -> Vec<T> {
        match self.0 {
            NonEmptyVecRepr::Vec(vec) => vec,
            NonEmptyVecRepr::Inline(inline) => vec![inline],
        }
    }
}

impl<T: Default> Default for NonEmptyVec<T> {
    fn default() -> Self {
        NonEmptyVec::new(T::default())
    }
}

pub const MESSAGE: &str =
    "Errors cannot be empty because errors are combined with a Trisult errors \
which can never be empty";

#[macro_export]
macro_rules! tri {
    ($trisult:expr, $context:expr) => {
        match $trisult {
            $crate::trisult::Trisult::Ok(value) => value,
            $crate::trisult::Trisult::Par(value, errors) => {
                $context.append(errors);
                value
            }
            $crate::trisult::Trisult::Err(errors) => {
                $context.append(errors);
                return $crate::trisult::Trisult::Err(
                    $context.try_into().expect($crate::trisult::MESSAGE),
                );
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_non_empty_vec_with() {
        let vec = NonEmptyVec::try_from(vec![1, 2, 3]).unwrap();
        let other = vec![4, 5, 6];

        let vec: Vec<_> = vec.with(other).into();

        assert_eq!(vec, vec![4, 5, 6, 1, 2, 3]);
    }

    #[test]
    fn test_inline_non_empty_vec_with() {
        let vec = NonEmptyVec::new(10);
        let other = vec![1, 2, 3];

        let vec: Vec<_> = vec.with(other).into();

        assert_eq!(vec, vec![1, 2, 3, 10]);
    }

    #[test]
    fn test_inline_add_to_vec() {
        let vec = NonEmptyVec::new(10);
        let mut other = vec![1, 2, 3];

        vec.add_to_vec(&mut other);

        assert_eq!(other, vec![1, 2, 3, 10]);
    }

    #[test]
    fn test_add_to_vec() {
        let vec = NonEmptyVec::try_from(vec![1, 2, 3]).unwrap();
        let mut other = vec![4, 5];

        vec.add_to_vec(&mut other);

        assert_eq!(other, vec![4, 5, 1, 2, 3]);
    }

    #[test]
    fn test_add_to_vec_other_order() {
        let vec = NonEmptyVec::try_from(vec![1, 2, 3]).unwrap();
        let mut other = vec![4, 5, 6, 7];

        vec.add_to_vec(&mut other);

        assert_eq!(other, vec![4, 5, 6, 7, 1, 2, 3]);
    }
}
