use crate::language::analysis::decl::DeclQuery;
use crate::language::{ast, im};
use crate::language::analysis::AnalysisError;
use crate::language::error::Trisult;
use crate::language::ast::{Action, Condition};
use crate::language::im::EventDeclarationId;

use super::event;
use super::r#struct;
use super::rule;

#[salsa::query_group(DefDatabase)]
pub trait DefQuery: DeclQuery {

    // Im

    #[salsa::invoke(super::im::query_im)]
    fn query_im(&self) -> Trisult<im::Im, AnalysisError>;

    // Event

    #[salsa::invoke(event::query_event_def_by_id)]
    fn query_event_def_by_id(&self, event_decl_id: EventDeclarationId) -> Trisult<im::EventDefinition, AnalysisError>;

    #[salsa::invoke(event::query_event_def)]
    fn query_event_def(&self, event_def: ast::EventDefinition) -> Trisult<im::EventDefinition, AnalysisError>;

    #[salsa::invoke(event::query_event)]
    fn query_event(&self, event: ast::Event) -> Trisult<im::Event, AnalysisError>;

    // Rule

    #[salsa::invoke(rule::query_rule_decl)]
    fn query_rule_decl(&self, rule: ast::Rule) -> Trisult<im::Rule, AnalysisError>;

    #[salsa::invoke(rule::query_rule_cond)]
    fn query_rule_cond(&self, event_decl_id: EventDeclarationId, conditions: Vec<Condition>) -> Trisult<Vec<im::AValue>, AnalysisError>;

    #[salsa::invoke(rule::query_rule_actions)]
    fn query_rule_actions(
        &self,
        event_decl_id: EventDeclarationId,
        actions: Vec<Action>
    ) -> Trisult<Vec<im::AValue>, AnalysisError>;

    // Struct

    #[salsa::invoke(r#struct::query_struct)]
    fn query_struct(&self, r#struct: ast::Struct) -> Trisult<im::Struct, AnalysisError>;

    #[salsa::invoke(r#struct::query_struct_def)]
    fn query_struct_def(&self, struct_dec: ast::StructDefinition) -> Trisult<im::StructDefinition, AnalysisError>;
}