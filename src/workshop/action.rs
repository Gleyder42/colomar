use super::common::*;

pub enum Action {
    SetAbility1(EventPlayer, bool)
}

impl Action {
    pub fn workshop_name(&self) -> String {
        match self {
            Action::SetAbility1(player, bool) => {
                format!("Set Ability 1 Enabled({}, {})", player, workshop_name(bool))
            }
        }
    }
}