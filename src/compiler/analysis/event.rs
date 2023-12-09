use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::analysis::def::DefQuery;
use crate::compiler::analysis::interner::IntoInternId;
use crate::compiler::cir::{EventDeclId, PropertyDecls};
use crate::compiler::cst::{Action, Actions};
use crate::compiler::QueryTrisult;
use crate::compiler::{cir, cst};

pub(super) fn query_event_def_by_id(
    db: &dyn DefQuery,
    event_decl_id: EventDeclId,
) -> QueryTrisult<cir::EventDef> {
    db.query_ast_event_def(event_decl_id)
        .flat_map(|event_def| db.query_event_def(event_decl_id, event_def))
}

pub(super) fn query_event_context_variables(
    db: &dyn DeclQuery,
    event_decl_id: EventDeclId,
) -> QueryTrisult<PropertyDecls> {
    db.query_ast_event_def(event_decl_id)
        .flat_map(|event_def| db.query_event_properties(event_decl_id, event_def.actions))
}

pub(super) fn query_event_properties(
    db: &dyn DeclQuery,
    event_decl_id: EventDeclId,
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
    event_decl_id: EventDeclId,
    event_def: cst::EventDef,
) -> QueryTrisult<cir::EventDef> {
    event_def
        .args
        .into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg))
        .collect::<QueryTrisult<_>>()
        .and_or_default(db.query_event_properties(event_decl_id, event_def.actions))
        .map(|(args, properties)| cir::EventDef { args, properties })
}

pub(super) fn query_event(db: &dyn DefQuery, event: cst::Event) -> QueryTrisult<cir::Event> {
    let event_decl_id = db.query_event_decl(event.decl);

    db.query_event_def(event_decl_id, event.def)
        .map(|def| cir::Event {
            decl: event_decl_id,
            def,
        })
}

pub(super) fn query_event_decl(db: &dyn DeclQuery, event_decl: cst::EventDecl) -> EventDeclId {
    cir::EventDecl {
        name: event_decl.name,
        span: event_decl.span,
    }
    .intern(db)
}
