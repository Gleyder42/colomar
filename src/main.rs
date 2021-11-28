mod workshop;
mod parser;

use std::fs::File;
use std::io::BufReader;
use crate::action::Action;
use crate::common::EventPlayer;
use crate::condition::Condition;
use crate::lexer::Lexer;
use crate::rule::{WorkshopEventType, WorkshopRule, WorkshopTree};
use crate::workshop::*;
use crate::writer::write_to_string;
use crate::parser::lexer;
use crate::parser::reader::Reader;

fn main() {
    let mut lexer = Lexer::new();
    let file = File::open("dsl/test.colo").expect("cannot open file");
    let mut reader = BufReader::new(file);
    let mut tokens = lexer.lex(&mut reader);

    println!("{:#?}", tokens);
    let inner = &tokens;
    let reader2 = &Reader::new(inner);
    let result = parser::parser::parse(reader2);
    let node = result.unwrap();
    println!("{:#?}", node)
}

fn test() {
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
