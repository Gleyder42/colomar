use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::analysis::QueryTrisult;
use crate::compiler::cir::{AValueChain, EventDeclarationId, StructDeclarationId};
use crate::compiler::cst::{Actions, Conditions};
use crate::compiler::{cir, cst};

use super::event;
use super::r#struct as sstruct;
use super::rule;

#[salsa::query_group(DefDatabase)]
pub trait DefQuery: DeclQuery {
    // Im

    /// [super::im::query_im]
    #[salsa::invoke(super::im::query_im)]
    fn query_im(&self) -> QueryTrisult<cir::Im>;

    // Event

    /// [event::query_event_def_by_id]
    #[salsa::invoke(event::query_event_def_by_id)]
    fn query_event_def_by_id(
        &self,
        event_decl_id: EventDeclarationId,
    ) -> QueryTrisult<cir::EventDefinition>;

    /// [event::query_event_def]
    #[salsa::invoke(event::query_event_def)]
    fn query_event_def(
        &self,
        event_decl_id: EventDeclarationId,
        event_def: cst::EventDefinition,
    ) -> QueryTrisult<cir::EventDefinition>;

    /// [event::query_event]
    #[salsa::invoke(event::query_event)]
    fn query_event(&self, event: cst::Event) -> QueryTrisult<cir::Event>;

    // Rule

    /// [rule::query_rule_decl]
    #[salsa::invoke(rule::query_rule_decl)]
    fn query_rule_decl(&self, rule: cst::Rule) -> QueryTrisult<cir::Rule>;

    /// [rule::query_rule_cond]
    #[salsa::invoke(rule::query_rule_cond)]
    fn query_rule_cond(
        &self,
        event_decl_id: EventDeclarationId,
        conditions: Conditions,
    ) -> QueryTrisult<Vec<AValueChain>>;

    /// [rule::query_rule_actions]
    #[salsa::invoke(rule::query_rule_actions)]
    fn query_rule_actions(
        &self,
        event_decl_id: EventDeclarationId,
        actions: Actions,
    ) -> QueryTrisult<Vec<AValueChain>>;

    // Struct

    /// [sstruct::query_struct]
    #[salsa::invoke(sstruct::query_struct)]
    fn query_struct(&self, sstruct: cst::Struct) -> QueryTrisult<cir::Struct>;

    /// [sstruct::query_struct_def]
    #[salsa::invoke(sstruct::query_struct_def)]
    fn query_struct_def(
        &self,
        struct_decl_id: StructDeclarationId,
        struct_dec: cst::StructDefinition,
    ) -> QueryTrisult<cir::StructDefinition>;
}
