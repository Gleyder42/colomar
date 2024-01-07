use super::cir::Root;
use super::codegen::Codegen;
use super::{wst, QueryTrisult};

#[salsa::query_group(PrinterDatabase)]
pub trait PrinterQuery: Codegen {
    fn query_wst_rule_to_string(&self, rule: wst::Rule) -> String;

    fn query_workshop_output(&self) -> QueryTrisult<String>;
}

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

        let variables = db.query_player_variables().map(|player_variables| {
            let player_variables = join_to_string(player_variables, 10);

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
    const SPACES: u8 = 16;

    if rule.event.hero_slot.is_none() && rule.event.team.is_none() {
        format!(
            include_str!("workshop_global_rule_template.txt"),
            rule = rule.title.name(db),
            event = rule.event.name,
            conditions = join_to_string(rule.conditions, SPACES),
            actions = join_to_string(rule.actions, SPACES)
        )
    } else {
        format!(
            include_str!("workshop_rule_template.txt"),
            rule = rule.title.name(db),
            event = rule.event.name,
            team = rule.event.team.unwrap(),
            hero_slot = rule.event.hero_slot.unwrap(),
            conditions = join_to_string(rule.conditions, SPACES),
            actions = join_to_string(rule.actions, SPACES)
        )
    }
}

fn join_to_string<T: ToString>(iter: impl IntoIterator<Item = T>, spaces: u8) -> String {
    let spaces: String = (0..spaces).into_iter().map(|_| ' ').collect();

    iter.into_iter()
        .map(|it| {
            let mut output = spaces.clone();
            output.push_str(&it.to_string());
            output.push(';');
            output
        })
        .collect::<Vec<_>>()
        .join("\n")
}
