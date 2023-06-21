use crate::language::ImmutableString;
use std::fmt;
use std::fmt::{Display, Formatter};

pub struct WorkshopTree(pub Vec<Rule>);

pub struct HeroSlot(pub ImmutableString);

pub struct Team(pub ImmutableString);

pub struct Event(pub ImmutableString);

pub struct Condition(pub ImmutableString);

pub struct Action(pub ImmutableString);

pub struct CustomString(pub ImmutableString);

pub struct Rule {
    pub name: CustomString,
    pub event: Event,
    pub team: Team,
    pub hero_slot: HeroSlot,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>,
}

macro_rules! derive_display {
    ($ty:ty) => {
        impl core::fmt::Display for $ty {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

derive_display!(Action);
derive_display!(Condition);
derive_display!(Event);
derive_display!(HeroSlot);
derive_display!(Team);

impl Display for CustomString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

impl Display for WorkshopTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let string = self
            .0
            .iter()
            .map(|it| it.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        write!(f, "{string}")
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            include_str!("rule.ows"),
            rule_name = self.name,
            event_type = self.event,
            arg0 = self.team,
            arg1 = self.hero_slot
        )
    }
}
