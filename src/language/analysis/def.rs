use crate::language::analysis::decl::DeclQuery;
use crate::language::{ast, im};
use crate::language::analysis::{QueryTrisult};
use crate::language::ast::{Action, Condition};
use crate::language::im::EventDeclarationId;

use super::event;
use super::r#struct;
use super::rule;

#[salsa::query_group(DefDatabase)]
pub trait DefQuery: DeclQuery {

    // Im

    #[salsa::invoke(super::im::query_im)]
    fn query_im(&self) -> QueryTrisult<im::Im>;

    // Event

    #[salsa::invoke(event::query_event_def_by_id)]
    fn query_event_def_by_id(&self, event_decl_id: EventDeclarationId) -> QueryTrisult<im::EventDefinition>;

    #[salsa::invoke(event::query_event_def)]
    fn query_event_def(&self, event_def: ast::EventDefinition) -> QueryTrisult<im::EventDefinition>;

    #[salsa::invoke(event::query_event)]
    fn query_event(&self, event: ast::Event) -> QueryTrisult<im::Event>;

    // Rule

    #[salsa::invoke(rule::query_rule_decl)]
    fn query_rule_decl(&self, rule: ast::Rule) -> QueryTrisult<im::Rule>;

    #[salsa::invoke(rule::query_rule_cond)]
    fn query_rule_cond(&self, event_decl_id: EventDeclarationId, conditions: Vec<Condition>) -> QueryTrisult<Vec<im::AValue>>;

    #[salsa::invoke(rule::query_rule_actions)]
    fn query_rule_actions(
        &self,
        event_decl_id: EventDeclarationId,
        actions: Vec<Action>
    ) -> QueryTrisult<Vec<im::AValue>>;

    // Struct

    #[salsa::invoke(r#struct::query_struct)]
    fn query_struct(&self, r#struct: ast::Struct) -> QueryTrisult<im::Struct>;

    #[salsa::invoke(r#struct::query_struct_def)]
    fn query_struct_def(&self, struct_dec: ast::StructDefinition) -> QueryTrisult<im::StructDefinition>;
}