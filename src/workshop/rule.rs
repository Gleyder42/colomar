use crate::workshop::common::EventPlayer;
use crate::workshop::condition::*;
use crate::workshop::action::*;

use std::convert::AsRef;
use strum_macros::AsRefStr;

pub struct WorkshopTree {
    pub rules: Vec<WorkshopRule>
}

impl WorkshopTree {
    pub fn new() -> WorkshopTree {
        WorkshopTree { rules: Vec::new() }
    }
}

pub struct WorkshopRule {
    pub name: String,
    pub event: WorkshopRuleEvent,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>
}

impl WorkshopRule {
    pub fn new(rule: String, event_type: WorkshopEventType) -> WorkshopRule {
        WorkshopRule {
            name: rule,
            event: WorkshopRuleEvent::new(event_type),
            conditions: Vec::new(),
            actions: Vec::new()
        }
    }
}

pub struct WorkshopRuleEvent {
    pub event_type: WorkshopEventType,
    pub team: WorkshopRuleTeam,
    pub slot: WorkshopRuleSlot
}

impl WorkshopRuleEvent {
    pub fn new(event_type: WorkshopEventType) -> WorkshopRuleEvent {
        WorkshopRuleEvent {
            event_type,
            slot: WorkshopRuleSlot::All,
            team: WorkshopRuleTeam::All
        }
    }
}

pub enum WorkshopEventType {
    OngoingGlobal,
    OngoingEachPlayer,
}

impl WorkshopEventType {
    pub fn workshop_name(&self) -> &'static str {
       match self {
           WorkshopEventType::OngoingGlobal => "Ongoing - Global",
           WorkshopEventType::OngoingEachPlayer => "Ongoing - Each Player"
       }
    }
}

pub enum WorkshopRuleTeam {
    All, Team1, Team2
}

impl WorkshopRuleTeam {
    pub fn workshop_name(&self) -> &'static str {
        match self {
            WorkshopRuleTeam::All => "All",
            WorkshopRuleTeam::Team1 => "Team 1",
            WorkshopRuleTeam::Team2 => "Team 2"
        }
    }
}

pub enum WorkshopRuleSlot {
    All, Slot(u8)
}

impl WorkshopRuleSlot {
    pub fn workshop_name(&self) -> String {
        match self {
            WorkshopRuleSlot::All => "All".to_string(),
            WorkshopRuleSlot::Slot(slot) => format!("Slot {}", slot),
        }
    }
}
