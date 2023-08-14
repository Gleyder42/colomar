use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::analysis::def::DefQuery;
use crate::compiler::analysis::interner::IntoInternId;
use crate::compiler::cir::{EventDeclarationId, PropertyDecls};
use crate::compiler::cst::{Action, Actions};
use crate::compiler::QueryTrisult;
use crate::compiler::{cir, cst};

pub(super) fn query_event_def_by_id(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
) -> QueryTrisult<cir::EventDefinition> {
    db.query_ast_event_def(event_decl_id)
        .flat_map(|event_def| db.query_event_def(event_decl_id, event_def))
}

pub(super) fn query_event_context_variables(
    db: &dyn DeclQuery,
    event_decl_id: EventDeclarationId,
) -> QueryTrisult<PropertyDecls> {
    db.query_ast_event_def(event_decl_id)
        .flat_map(|event_def| db.query_event_properties(event_decl_id, event_def.actions))
}

pub(super) fn query_event_properties(
    db: &dyn DeclQuery,
    event_decl_id: EventDeclarationId,
    actions: Actions,
) -> QueryTrisult<PropertyDecls> {
    let event_type = cir::Type::Event(event_decl_id);

    actions
        .into_iter()
        .map(|action| match action {
            Action::Property(property_decl) => db.query_property(Some(event_type), property_decl),
            Action::CallChain(_) | Action::Assignment(..) => {
                todo!("Currently only properties are supported in even definitions")
            }
        })
        .collect::<QueryTrisult<_>>()
}

pub(super) fn query_event_def(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
    event_def: cst::EventDefinition,
) -> QueryTrisult<cir::EventDefinition> {
    event_def
        .arguments
        .into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg))
        .collect::<QueryTrisult<_>>()
        .and_or_default(db.query_event_properties(event_decl_id, event_def.actions))
        .map(|(arguments, properties)| cir::EventDefinition {
            arguments,
            properties,
        })
}

pub(super) fn query_event(db: &dyn DefQuery, event: cst::Event) -> QueryTrisult<cir::Event> {
    let event_declaration_id = db.query_event_decl(event.declaration);

    db.query_event_def(event_declaration_id, event.definition)
        .map(|definition| cir::Event {
            declaration: event_declaration_id,
            definition,
        })
}

pub(super) fn query_event_decl(
    db: &dyn DeclQuery,
    event_decl: cst::EventDeclaration,
) -> EventDeclarationId {
    cir::EventDeclaration {
        name: event_decl.name,
        span: event_decl.span,
    }
    .intern(db)
}
