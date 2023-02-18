use std::collections::{HashMap, HashSet};
use crate::language::parser::{Enum, Event, Rule, TopLevelDecl};

type Namespace = HashMap<String, String>;

pub fn validate(ast: &mut Vec<TopLevelDecl>) {

}

fn validate_enum(global_namespace: Namespace, enumm: &mut Enum) {

}

fn has_duplicates(idents: &Vec<String>) -> bool {
    let set: HashSet<_> = idents.iter().collect();
    idents.len() > set.len()
}
