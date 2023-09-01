use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::cir::{EventDeclarationId, StructDeclarationId};
use crate::compiler::cst::Conditions;
use crate::compiler::QueryTrisult;
use crate::compiler::{cir, cst};
use hashlink::LinkedHashSet;

use super::event;
use super::im;
use super::import;
use super::r#struct as sstruct;
use super::rule;

#[salsa::query_group(DefDatabase)]
pub trait DefQuery: DeclQuery {
    // Im

    /// [im::query_im]
    #[salsa::invoke(im::query_im)]
    fn query_im(&self) -> QueryTrisult<cir::Cir>;

    /// [im::query_player_struct_def]
    #[salsa::invoke(im::query_player_struct_def)]
    fn query_player_struct_def(&self) -> cst::Struct;

    // Import

    /// [import::load_imports]
    #[salsa::invoke(import::load_imports)]
    fn load_imports(&self, imported: LinkedHashSet<cst::Path>) -> QueryTrisult<Vec<cst::Root>>;

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
    ) -> QueryTrisult<Vec<cir::Expr>>;

    /// [rule::query_rule_actions]
    #[salsa::invoke(rule::query_rule_actions)]
    fn query_rule_actions(
        &self,
        event_decl_id: EventDeclarationId,
        actions: cst::Actions,
    ) -> QueryTrisult<cir::Actions>;

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
