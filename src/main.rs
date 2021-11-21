mod workshop;

use crate::action::Action;
use crate::common::EventPlayer;
use crate::condition::Condition;
use crate::rule::{WorkshopEventType, WorkshopRule, WorkshopTree};
use crate::workshop::*;
use crate::writer::write_to_string;

fn main() {
    let mut tree = WorkshopTree::new();
    let mut rule = WorkshopRule::new(
        "Example Rule".to_string(), WorkshopEventType::OngoingEachPlayer
    );
    rule.conditions.push(Condition::IsGameInProgress(true));
    rule.actions.push(Action::SetAbility1(EventPlayer, true));
    tree.rules.push(rule);

    let output = write_to_string(&tree);
    println!("{}", output);
}

