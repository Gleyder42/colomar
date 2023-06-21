use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::def::DefQuery;
use crate::language::analysis::interner::IntoInternId;
use crate::language::analysis::QueryTrisult;
use crate::language::ast::{Action, Actions};
use crate::language::im::{EventDeclarationId, PropertyDecls};
use crate::language::{ast, im};

pub(super) fn query_event_def_by_id(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
) -> QueryTrisult<im::EventDefinition> {
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
    let event_type = im::Type::Event(event_decl_id);

    actions
        .into_iter()
        .map(|action| match action {
            Action::Property(property_decl) => db.query_property(Some(event_type), property_decl),
            Action::CallChain(_) => todo!(),
        })
        .collect::<QueryTrisult<_>>()
}

pub(super) fn query_event_def(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
    event_def: ast::EventDefinition,
) -> QueryTrisult<im::EventDefinition> {
    event_def
        .arguments
        .into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg))
        .collect::<QueryTrisult<_>>()
        .and_or_default(db.query_event_properties(event_decl_id, event_def.actions))
        .map(|(arguments, properties)| im::EventDefinition {
            arguments,
            properties,
        })
}

pub(super) fn query_event(db: &dyn DefQuery, event: ast::Event) -> QueryTrisult<im::Event> {
    let event_declaration_id = db.query_event_decl(event.declaration);

    db.query_event_def(event_declaration_id, event.definition)
        .map(|definition| im::Event {
            declaration: event_declaration_id,
            definition,
        })
}

pub(super) fn query_event_decl(
    db: &dyn DeclQuery,
    event_decl: ast::EventDeclaration,
) -> EventDeclarationId {
    im::EventDeclaration {
        name: event_decl.name,
        span: event_decl.span,
    }
    .intern(db)
}
