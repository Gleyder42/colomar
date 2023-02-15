use std::fs;

use crate::language::parser::{Call, CallName, Rule};

pub struct WorkshopPrinter {
    rule_template: String,
    output: String,
    all: Box<Call>
}

impl WorkshopPrinter {
    pub fn new() -> WorkshopPrinter {
        let rule_template = fs::read_to_string("workshop/rule.ows").unwrap();
        WorkshopPrinter { output: String::new(), rule_template, all: Call::new_var("All") }
    }

    pub fn print_rule(&mut self, rules: Vec<Rule>) {
        for rule in rules {
            let mut workshop_rule = self.rule_template
                .replace("%rule_name%", &rule.name)
                .replace("%event_type%", &rule.event)
                .replace("%arg0%", rule.args.get(0).unwrap_or(&self.all).name())
                .replace("%arg1%", rule.args.get(1).unwrap_or(&self.all).name());

            workshop_rule.push('\n');
            workshop_rule.push('\n');
            self.output.push_str(&workshop_rule);
        }
    }

    pub fn output(&self) -> &String {
        &self.output
    }
}