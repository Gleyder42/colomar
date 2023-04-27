use std::collections::HashMap;
use std::rc::Rc;
use crate::language::converter::error::ConverterError;
use crate::language::{Ident, ImmutableString};
use crate::language::im::RValue;

pub struct Namespace {
    parents: Vec<Rc<Namespace>>,
    map: HashMap<ImmutableString, RValue>,
}

impl Namespace {

    fn new_root() -> Namespace {
        Namespace { map: HashMap::new(), parents: Vec::new() }
    }

    fn add(&mut self, ident: Ident, rvalue: RValue) -> Result<(), ConverterError> {
        match self.get(&ident) {
            Some(rvalue) => {
                ConverterError::DuplicateIdent {
                    first: rvalue.name(),
                    second: ident
                }.into()
            }
            None => {
                self.map.insert(ident.value.clone(), rvalue);
                Ok(())
            }
        }
    }

    fn get(&self, ident: &Ident) -> Option<RValue> {
        let option = self.map.get(&ident.value).map(|it| it.clone());
        if option.is_some() {
            option
        } else {
            self.parents.iter()
                .map(|it| it.get(&ident))
                .filter(|it| it.is_some())
                .flatten()
                .collect::<Vec<_>>()
                .pop()
        }
    }
}