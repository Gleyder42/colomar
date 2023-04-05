use std::fmt::{Display, Formatter};
use crate::language::imt;
use crate::workshop as ws;
use crate::workshop::{Event, HeroSlot, Team};

impl Display for imt::EnumConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.value)
    }
}

impl Display for imt::ConstValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            imt::ConstValue::EnumConstant(enum_constant) => write!(f, "{}", enum_constant.name.value)
        }
    }
}

pub fn compile(imt: imt::Imt) -> ws::WorkshopTree {
    let mut rules = Vec::new();

    for root in imt {
        match root {
            imt::Root::Rule(rule) => {
                let rule = rule.borrow();

                let rule = ws::Rule {
                    name: rule.title.clone(),
                    event: Event(rule.event.bound_or_panic().borrow().name.value.clone()) ,
                    team: Team(rule.arguments.bound_or_panic()
                        .get(0)
                        .map(|it| it.value.to_string())
                        .unwrap_or("All".to_string())),
                    player: HeroSlot(rule.arguments.bound_or_panic()
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
