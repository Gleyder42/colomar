use crate::language::{ast, im};
use crate::language::analysis::arg::ArgQuery;
use crate::language::analysis::error::{AnalysisError, QueryResult, Sbe};
use crate::language::analysis::interner::{Interner, IntoInternId};
use crate::language::im::DeclaredArgument;

#[salsa::query_group(EventDatabase)]
pub trait EventQuery: EventDeclQuery + ArgQuery {

    fn query_event_def(&self, event_def: ast::EventDefinition) -> QueryResult<im::EventDefinition, AnalysisError>;

    fn query_event(&self, event: ast::Event) -> QueryResult<im::Event, AnalysisError>;
}

fn query_event_def(db: &dyn EventQuery, event_def: ast::EventDefinition) -> QueryResult<im::EventDefinition, AnalysisError> {
    event_def.arguments.into_iter()
        .map(|decl_arg| db.query_declared_arg(decl_arg))
        .collect::<QueryResult<Vec<_>, _>>()
        .map(|arguments| im::EventDefinition { arguments })
}

fn query_event(db: &dyn EventQuery, event: ast::Event) -> QueryResult<im::Event, AnalysisError> {
    db.query_event_def(event.definition)
        .map(|definition| im::Event {
            declaration: db.query_event_decl(event.declaration),
            definition
        })
}

#[salsa::query_group(EventDeclarationDatabase)]
pub trait EventDeclQuery: Interner {

    fn query_event_decl(&self, event_decl: ast::EventDeclaration) -> im::EventDeclarationId;
}

fn query_event_decl(db: &dyn EventDeclQuery, event_decl: ast::EventDeclaration) -> im::EventDeclarationId {
    im::EventDeclaration {
        name: event_decl.name,
        span: event_decl.span,
    }.intern(db)
}
