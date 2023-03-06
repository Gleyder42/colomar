use std::collections::HashSet;
use std::hash::Hash;
use crate::language::im::Named;

struct Namespace(HashSet<String>);

struct Validator<T>(T);

impl<T> Validator<T> {
    fn new(t: T) -> Self {
        Validator(t)
    }
}

impl<'a, T, I> Validator<I>
    where T: 'a + Eq + PartialEq + Hash + Named,
          I: Iterator<Item=&'a T> {

    fn unique_names(self, namespace: &mut Namespace) {
        let mut is_conflicting = false;
        for x in self.0 {
            if !namespace.0.insert(x.name()) {
                is_conflicting = true;
            }
        }
    }
}