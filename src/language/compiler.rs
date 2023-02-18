use crate::language::parser;
use crate::workshop;
use crate::workshop::{Event, Team};

pub fn compile(ast: Vec<parser::TopLevelDecl>) -> Vec<workshop::Rule> {
    ast.into_iter()
        .filter_map(|o| match o {
            parser::TopLevelDecl::Rule(rule) => Some(rule),
            _ => None
        })
        .map(|rule| {
            workshop::Rule {
               name: rule.name,
                event: Event(rule.event),
                team: Team(rule.),
                player: (),
            }
        })
    todo!()
}