use std::fmt::{Display, Formatter};
use crate::language::im;
use crate::workshop as ws;
use crate::workshop::{Event, HeroSlot, Team};

impl Display for im::EnumConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.value)
    }
}

pub fn compile(im: im::Im) -> ws::WorkshopTree {
    let mut rules = Vec::new();

    for root in im {
        match root {
            im::Root::Rule(rule) => {
                let rule = rule.borrow();

                let rule = ws::Rule {
                    name: rule.title.clone(),
                    event: Event(rule.event.bound().borrow().name.value.clone()) ,
                    team: Team(rule.arguments.bound()
                        .get(0)
                        .map(|it| it.value.to_string())
                        .unwrap_or("All".to_string())),
                    player: HeroSlot(rule.arguments.bound()
                        .get(1)
                        .map(|it| it.value.to_string())
                        .unwrap_or("All".to_string()))
                };
                rules.push(rule);
            }
            _ => {}
        }
    }

    ws::WorkshopTree(rules)
}
