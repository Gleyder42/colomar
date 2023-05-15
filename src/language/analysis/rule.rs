use crate::language::{ast, im};
use crate::language::analysis::error::{AnalysisError};
use crate::language::analysis::r#type::TypeQuery;

#[salsa::query_group(RuleDatabase)]
pub trait Rule: TypeQuery {
    fn query_rule_decl(&self, rule: ast::Rule) -> Result<im::Rule, AnalysisError>;
}

fn query_rule_decl(db: &dyn Rule, rule: ast::Rule) -> Result<im::Rule, AnalysisError> {
    db.query_type_event_decl(rule.event)
        .map(|event| im::Rule {
            event,
            title: rule.name.value,
            arguments: Vec::new(), // Add arguments resolver
        })
}