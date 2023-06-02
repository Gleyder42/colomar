use crate::language::{ast, Ident, im};
use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::def::DefQuery;
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::interner::{Interner, IntoInternId};
use crate::language::ast::Action;
use crate::language::im::EventDeclarationId;

pub(in super) fn query_event_def_by_id(db: &dyn DefQuery, event_decl_id: EventDeclarationId) -> QueryResult<im::EventDefinition, AnalysisError> {
    db.query_ast_event_def(event_decl_id)
        .flat_map(|def| db.query_event_def(def))
}

pub(in super) fn query_event_def(db: &dyn DefQuery, event_def: ast::EventDefinition) -> QueryResult<im::EventDefinition, AnalysisError> {
    let properties = event_def.actions.into_iter()
        .map(|action| match action {
            Action::Property(property_decl) => db.query_property(property_decl),
            Action::CallChain(_) => todo!(),
        })
        .collect::<QueryResult<Vec<_>, _>>();

    event_def.arguments.into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg))
        .collect::<QueryResult<Vec<_>, _>>()
        .and_or_default(properties)
        .map(|(arguments, properties)| im::EventDefinition { arguments, properties })
}

pub(in super) fn query_event(db: &dyn DefQuery, event: ast::Event) -> QueryResult<im::Event, AnalysisError> {
    db.query_event_def(event.definition)
        .map(|definition| im::Event {
            declaration: db.query_event_decl(event.declaration),
            definition
        })
}

pub(in super) fn query_event_decl(db: &dyn DeclQuery, event_decl: ast::EventDeclaration) -> im::EventDeclarationId {
    im::EventDeclaration {
        name: event_decl.name,
        span: event_decl.span,
    }.intern(db)
}
