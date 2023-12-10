use crate::compiler::cir::Root;
use crate::compiler::codegen::Codegen;
use crate::compiler::{wst, QueryTrisult};

#[salsa::query_group(PrinterDatabase)]
pub trait PrinterQuery: Codegen {
    fn query_wst_rule_to_string(&self, rule: wst::Rule) -> String;

    fn query_workshop_output(&self) -> QueryTrisult<String>;
}

const SPACES: &str = "        ";

fn query_workshop_output(db: &dyn PrinterQuery) -> QueryTrisult<String> {
    db.query_im().flat_map(|root| {
        let rules: QueryTrisult<_> = root
            .into_iter()
            .filter_map(|root| match root {
                Root::Rule(rule) => {
                    let trisult = db
                        .query_wst_rule(rule)
                        .map(|rule| db.query_wst_rule_to_string(rule));
                    Some(trisult)
                }
                _ => None,
            })
            .collect::<QueryTrisult<Vec<String>>>()
            .map(|rules| rules.join("\n"));

        let variables = db
            .query_player_variables()
            .map_inner::<_, _, Vec<_>>(|variable| {
                [SPACES.to_string(), variable.to_string()].join(" ")
            })
            .map(|player_variables| {
                let player_variables = player_variables.join("\n");

                format!(
                    include_str!("player_variables.txt"),
                    player_variables = player_variables
                )
            });

        rules
            .and(variables)
            .map(|(rules, variables)| [variables, rules].join("\n"))
    })
}

fn query_wst_rule_to_string(db: &dyn PrinterQuery, rule: wst::Rule) -> String {
    if rule.event.hero_slot.is_none() && rule.event.team.is_none() {
        format!(
            include_str!("workshop_global_rule_template.txt"),
            rule = rule.title.name(db),
            event = rule.event.name,
            conditions = join_to_string(rule.conditions),
            actions = join_to_string(rule.actions)
        )
    } else {
        format!(
            include_str!("workshop_rule_template.txt"),
            rule = rule.title.name(db),
            event = rule.event.name,
            team = rule.event.team.unwrap(),
            hero_slot = rule.event.hero_slot.unwrap(),
            conditions = join_to_string(rule.conditions),
            actions = join_to_string(rule.actions)
        )
    }
}

fn join_to_string<T: ToString>(iter: impl IntoIterator<Item = T>) -> String {
    let mut string = iter
        .into_iter()
        .map(|it| {
            let mut spaces = SPACES.to_string();
            spaces.push_str(&it.to_string());
            spaces
        })
        .collect::<Vec<_>>()
        .join(";\n");
    if string.len() > 0 {
        string.push(';');
    }
    string
}
