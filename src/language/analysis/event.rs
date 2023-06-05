use crate::language::{ast, im};
use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::def::DefQuery;
use crate::language::analysis::error::{AnalysisError, Trisult};
use crate::language::analysis::interner::{IntoInternId};
use crate::language::ast::Action;
use crate::language::im::{EventDeclarationId, PropertyDecl};

pub(in super) fn query_event_def_by_id(db: &dyn DefQuery, event_decl_id: EventDeclarationId) -> Trisult<im::EventDefinition, AnalysisError> {
    db.query_ast_event_def(event_decl_id)
        .flat_map(|event_def| db.query_event_def(event_def))
}

pub(in super) fn query_event_context_variables(
    db: &dyn DeclQuery,
    event_decl: EventDeclarationId
) -> Trisult<Vec<PropertyDecl>, AnalysisError> {
    db.query_ast_event_def(event_decl)
        .flat_map(|event_def| db.query_event_properties(event_def.actions))
}

pub(in super) fn query_event_properties(db: &dyn DeclQuery, actions: Vec<Action>) -> Trisult<Vec<PropertyDecl>, AnalysisError> {
    actions.into_iter()
        .map(|action| match action {
            Action::Property(property_decl) => db.query_property(property_decl),
            Action::CallChain(_) => todo!(),
        })
        .collect::<Trisult<Vec<_>, _>>()
}

pub(in super) fn query_event_def(db: &dyn DefQuery, event_def: ast::EventDefinition) -> Trisult<im::EventDefinition, AnalysisError> {
    event_def.arguments.into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg))
        .collect::<Trisult<Vec<_>, _>>()
        .and_or_default(db.query_event_properties(event_def.actions))
        .map(|(arguments, properties)| im::EventDefinition { arguments, properties })
}

pub(in super) fn query_event(db: &dyn DefQuery, event: ast::Event) -> Trisult<im::Event, AnalysisError> {
    db.query_event_def(event.definition)
        .map(|definition| im::Event {
            declaration: db.query_event_decl(event.declaration),
            definition
        })
}

pub(in super) fn query_event_decl(db: &dyn DeclQuery, event_decl: ast::EventDeclaration) -> EventDeclarationId {
    im::EventDeclaration {
        name: event_decl.name,
        span: event_decl.span,
    }.intern(db)
}
