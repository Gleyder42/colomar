use std::fmt::format;
use crate::common::workshop_name;

pub enum Condition {
    IsGameInProgress(bool)
}

impl Condition {

    pub fn workshop_name(&self) -> String {
        match self {
            Condition::IsGameInProgress(bool) => {
                format!("Is Game In Progress == {}", workshop_name(bool))
            }
        }
    }
}