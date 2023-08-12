use crate::compiler::codegen::Codegen;
use crate::compiler::wst;

#[salsa::query_group(PrinterDatabase)]
pub trait PrinterQuery: Codegen {
    fn query_wst_rule_to_string(&self, rule: wst::Rule) -> String;
}

fn query_wst_rule_to_string(_db: &dyn PrinterQuery, rule: wst::Rule) -> String {
    format!(
        include_str!("workshop_rule_template.txt"),
        rule = rule.title,
        event = rule.event.name,
        team = rule.event.team,
        hero_slot = rule.event.hero_slot,
        conditions = join_to_string(rule.conditions),
        actions = join_to_string(rule.actions)
    )
}

fn join_to_string<T: ToString>(iter: impl IntoIterator<Item = T>) -> String {
    const SPACES: &str = "        ";

    let mut string = iter
        .into_iter()
        .map(|it| {
            let mut spaces = SPACES.to_string();
            spaces.push_str(&it.to_string());
            spaces
        })
        .collect::<Vec<_>>()
        .join(";\n");
    string.push(';');
    string
}
