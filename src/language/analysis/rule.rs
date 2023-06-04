use salsa::InternKey;
use crate::language::{ast, im};
use crate::language::analysis::def::DefQuery;
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::interner::IntoInternId;
use crate::language::analysis::namespace::Nameholder;
use crate::language::ast::Condition;
use crate::language::im::{CalledArgument, DeclaredArgument, EventDeclarationId, Predicate, Type};
use crate::query_error;

pub fn query_rule_cond(
    db: &dyn DefQuery,
    event_decl_id: EventDeclarationId,
    conditions: Vec<Condition>,
) -> QueryResult<Vec<im::AValue>, AnalysisError> {
    conditions.into_iter()
        .map(|condition| db.query_call_chain(vec![Nameholder::Root, Nameholder::Event(event_decl_id)], condition))
        .collect::<QueryResult<Vec<_>, _>>()
        .and_require(db.query_bool_type().map(|decl_id| Type::Struct(decl_id)))
        .flat_map(|(avalues, bool_id)| {
            avalues.into_iter()
                .map(|avalue| {
                    if avalue.r#type(db) == bool_id {
                        QueryResult::Ok(avalue)
                    } else {
                        query_error!(AnalysisError::WrongType)
                    }
                }).collect()
        })
}

pub(in super) fn query_rule_decl(db: &dyn DefQuery, rule: ast::Rule) -> QueryResult<im::Rule, AnalysisError> {

    let arguments = |event_decl_id: EventDeclarationId| {
        rule.arguments.into_iter()
            .map(|call_chain| db.query_call_chain(vec![Nameholder::Root], call_chain))
            .collect::<QueryResult<_, _>>()
            .and_require(db.query_event_def_by_id(event_decl_id))
            .flat_map(|(arguments, event_def)| {
                event_def.arguments.into_iter()
                    .map::<DeclaredArgument, _>(|decl_arg_id| db.lookup_intern_decl_arg(decl_arg_id))
                    .zip(arguments.into_iter())
                    .map(|(decl_arg, avalue)| {
                        let valid_type = decl_arg.types.contains_type(avalue.r#type(db));

                        let called_argument = CalledArgument {
                            value: avalue,
                            declared: decl_arg.intern(db),
                        };

                        if valid_type {
                            QueryResult::Ok(called_argument)
                        } else {
                            let error = AnalysisError::WrongType;
                            QueryResult::Par(called_argument, vec![error])
                        }
                    })
                    .collect::<QueryResult<_, _>>()
            })
    };

    db.query_namespaced_event(vec![Nameholder::Root], rule.event)
        .map_and_require(arguments)
        .map_and_require(|(event_decl_id, arguments)| {
            db.query_rule_cond(event_decl_id, rule.conditions)
                .map_inner(|avalue| Predicate { return_value: avalue})
        })
        .map(|((event_decl_id, arguments), conditions)| im::Rule {
            title: rule.name.value,
            event: event_decl_id,
            arguments,
            conditions
        })
}