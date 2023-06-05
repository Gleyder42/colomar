use std::fmt::{Display, Formatter};

pub struct WorkshopTree(pub Vec<Rule>);

pub struct HeroSlot(pub String);

pub struct Team(pub String);

pub struct Event(pub String);

pub struct Rule {
    pub name: String,
    pub event: Event,
    pub team: Team,
    pub player: HeroSlot,
}

impl Display for Event {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for HeroSlot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Team {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for WorkshopTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = self
            .0
            .iter()
            .map(|it| it.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        write!(f, "{string}")
    }
}

const RULE_TEMPLATE: &'static str = include_str!("rule.ows");

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let rule = RULE_TEMPLATE
            .to_string()
            .replace("%rule_name%", &self.name)
            .replace("%event_type%", &self.event.0)
            .replace("%arg0%", &self.team.0)
            .replace("%arg1%", &self.player.0);

        write!(f, "{}", rule)
    }
}
