use std::fmt::{Display, Formatter, write};

pub struct HeroSlot(String);

pub struct Team(String);

pub struct Event(String);

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

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,
               "rule({rule_name})
                {{
                    event
                    {{
                        {event_type}
                        {team}
                        {player}
                    }}
                }}",
               rule_name = self.name,
               event_type = self.event,
               team = self.team,
               player = self.player
        )
    }
}