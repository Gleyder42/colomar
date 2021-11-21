use std::fmt;
use std::fmt::{Formatter, write};

pub struct EventPlayer;

impl fmt::Display for EventPlayer{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "EventPlayer")
    }
}

pub fn workshop_name(bool: &bool) -> &'static str {
    match bool {
        true => "True",
        false => "False"
    }
}
