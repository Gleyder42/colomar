use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use crate::intermediate;
use crate::language::parser;

type Namespace = HashMap<String, String>;

pub fn validate(ast: parser::Ast) {
    let mut errors: Vec<String> = Vec::new();

    for root in ast.0 {
        match root {
            parser::Root::Enum(enumm) => {
                validate_enum(enumm);
            }
            _ => unimplemented!()
        }
    }
}

pub fn validate_enum(enumm: parser::Enum) -> Rc<intermediate::Enum> {
    let intermediate = intermediate::Enum {
        constants: enumm.constants
            .into_iter()
            .map(|item| Rc::new(intermediate::EnumConstant { name: item }))
            .collect(),
        is_workshop: enumm.is_native,
    };
    Rc::new(intermediate)
}

struct Validator<T>(T);

impl<T> Validator<T> {

    fn unique_names(self) -> Self {
        self
    }

    fn map(self) -> Self {
        self
    }
}