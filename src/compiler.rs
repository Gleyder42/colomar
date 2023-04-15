use std::fmt::{Display, Formatter};
use crate::language::im;
use crate::language::im::RuleRef;
use crate::workshop as ws;
use crate::workshop::{Event, HeroSlot, Team};

impl Display for im::EnumConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.value)
    }
}

fn get_argument_value(rule: RuleRef, index: usize) -> im::ActualValue {
    let binding = rule.borrow();
    let called_argument = binding.arguments.bound().get(index);
    let binding = binding.event.bound()
        .borrow().arguments.get(index).unwrap()
        .borrow().default_value.clone().unwrap();
    let declared_argument = binding.bound();

    called_argument.map(|it| it.value.clone()).unwrap_or(declared_argument.clone())
}

pub fn compile(im: im::Im) -> ws::WorkshopTree {
    let mut rules = Vec::new();

    for root in im {
        match root {
            im::Root::Rule(rule) => {
                let binding = rule.borrow();

                let rule = ws::Rule {
                    name: binding.title.clone(),
                    event: Event(binding.event.bound().borrow().name.value.clone()) ,
                    team: Team(get_argument_value(rule.clone(), 0).to_string()),
                    player: HeroSlot(get_argument_value(rule.clone(), 1).to_string())
                };
                rules.push(rule);
            }
            _ => {}
        }
    }

    ws::WorkshopTree(rules)
}
