use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;
use std::marker::PhantomData;
use std::process::id;
use std::rc::Rc;
use crate::language::ast;
use crate::language::im::Named;

struct Namespace<'a> {
    idents: HashSet<&'a str>,
    parent: Option<Rc<Namespace<'a>>>,
    label: &'a str
}

impl<'a> Namespace<'a>  {

    fn new(parent: Option<Rc<Namespace<'a>>>, label: &'a str) -> Namespace<'a> {
        Namespace { idents: HashSet::new(), parent, label }
    }

    fn add(&mut self, ident: &'a str) -> bool {
        if Namespace::exists(&self, ident) {
            return false
        } else {
            self.idents.insert(ident)
        }
    }
}


impl<'a> Namespace<'a> {

    fn exists(namespace: &Namespace<'a>, ident: &'a str) -> bool {
        if namespace.idents.contains(ident) {
            true
        } else {
            if let Some(parent) = &namespace.parent {
                Namespace::exists(parent, ident)
            } else {
                false
            }
        }
    }
}

struct Error {
    message: String
}

struct Validator<T> {
    value: T,
    errors: Vec<Error>,
}

impl<T> Validator<T> {

    fn new(value: T) -> Self {
        Validator { value, errors: Vec::new() }
    }
}

impl<'a, I, T> Validator<I>
    where
        T: 'a + Eq + Hash + Named<'a>,
        I: Iterator<Item=&'a T> {

    fn unique_names(mut self, namespace: &'a mut Namespace<'a>) {
        let duplicates = self.value.into_iter()
            .filter_map(|it| if namespace.add(it.name()) {
                Some(it.name())
            } else {
                None
            })
            .collect::<Vec<_>>()
            .join(", ");

        self.errors.push(Error { message: format!("{duplicates} already exits") })
    }
}
