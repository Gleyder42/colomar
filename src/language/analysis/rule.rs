use salsa::InternKey;
use crate::language::{ast, im};
use crate::language::analysis::arg::ArgQuery;
use crate::language::analysis::call::CallQuery;
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::event::EventQuery;
use crate::language::analysis::file::AstDefQuery;
use crate::language::analysis::interner::IntoInternId;
use crate::language::analysis::namespace::NamespacePlaceholder;
use crate::language::analysis::r#type::TypeQuery;
use crate::language::im::{CalledArgument, DeclaredArgument, EventDeclarationId};

#[salsa::query_group(RuleDeclDatabase)]
pub trait RuleQuery: CallQuery + AstDefQuery + EventQuery {

    fn query_rule_decl(&self, rule: ast::Rule) -> QueryResult<im::Rule, AnalysisError>;
}

fn query_rule_decl(db: &dyn RuleQuery, rule: ast::Rule) -> QueryResult<im::Rule, AnalysisError> {
    let arguments = |event_decl_id: EventDeclarationId| {
        rule.arguments.into_iter()
            .map(|call_chain| db.query_call_chain(NamespacePlaceholder::Root, call_chain))
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
                            declared: decl_arg.intern(db)
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

    db.query_namespaced_event(NamespacePlaceholder::Root, rule.event)
        .map_and_require(arguments)
        .map(|(event, arguments)| im::Rule {
            title: rule.name.value,
            event,
            arguments
        })
}