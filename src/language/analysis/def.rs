use crate::language::analysis::decl::DeclQuery;
use crate::language::{ast, im};
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::ast::{Action, Condition};
use crate::language::im::{EventDeclarationId};

use super::event;
use super::r#struct;
use super::rule;

#[salsa::query_group(DefDatabase)]
pub trait DefQuery: DeclQuery {

    // Im

    #[salsa::invoke(super::im::query_im)]
    fn query_im(&self) -> QueryResult<im::Im, AnalysisError>;

    // Event

    #[salsa::invoke(event::query_event_def_by_id)]
    fn query_event_def_by_id(&self, event_decl_id: EventDeclarationId) -> QueryResult<im::EventDefinition, AnalysisError>;

    #[salsa::invoke(event::query_event_def)]
    fn query_event_def(&self, event_def: ast::EventDefinition) -> QueryResult<im::EventDefinition, AnalysisError>;

    #[salsa::invoke(event::query_event)]
    fn query_event(&self, event: ast::Event) -> QueryResult<im::Event, AnalysisError>;

    // Rule

    #[salsa::invoke(rule::query_rule_decl)]
    fn query_rule_decl(&self, rule: ast::Rule) -> QueryResult<im::Rule, AnalysisError>;

    #[salsa::invoke(rule::query_rule_cond)]
    fn query_rule_cond(&self, event_decl_id: EventDeclarationId, conditions: Vec<Condition>) -> QueryResult<Vec<im::AValue>, AnalysisError>;

    #[salsa::invoke(rule::query_rule_actions)]
    fn query_rule_actions(
        &self,
        event_decl_id: EventDeclarationId,
        actions: Vec<Action>
    ) -> QueryResult<Vec<im::AValue>, AnalysisError>;

    // Struct

    #[salsa::invoke(r#struct::query_struct)]
    fn query_struct(&self, r#struct: ast::Struct) -> QueryResult<im::Struct, AnalysisError>;

    #[salsa::invoke(r#struct::query_struct_def)]
    fn query_struct_def(&self, struct_dec: ast::StructDefinition) -> QueryResult<im::StructDefinition, AnalysisError>;
}