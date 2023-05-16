use crate::language::{ast, im};
use crate::language::analysis::error::{AnalysisError, QueryResult};
use crate::language::analysis::namespace::{NamespacePlaceholder, NamespaceQuery};

#[salsa::query_group(CallDatabase)]
pub trait CallQuery: NamespaceQuery {
    fn query_call_chain(
        &self,
        namespace: NamespacePlaceholder,
        call_chain: ast::CallChain,
    ) -> QueryResult<im::AValue, AnalysisError>;
}

fn query_call_chain(
    db: &dyn CallQuery,
    namespace: NamespacePlaceholder,
    call_chain: ast::CallChain,
) -> QueryResult<im::AValue, AnalysisError> {
    assert!(!call_chain.value.is_empty(), "A call chain cannot be empty, but was");

    QueryResult::<ast::CallChain, AnalysisError>::from(call_chain)
        .fold_map::<im::AValue, _, _, _>(
            (namespace, None),
            // func refers to the closure processing the call.
            // map_func refers to the closure unwrapping the value .
            //
            // We are allowed to unwrap here, because we know value must be some.
            // The call chain is not null, so we hava at least on call to operate on.
            // fold_map does not guarantee to call func by itself, because in case of Err,
            // func will not be called. However, QueryResult::from(call_chain) returns Ok,
            // so func will definitely be called.
            // If the result returned in func is error, value could be none, but it doesn't matter
            // as map_func in that case is not called
            |(_, value)| value.unwrap(),
            |(namespace, value), call| {
                match *call {
                    ast::Call::Ident(ident) => {
                        db.query_namespaced_rvalue(namespace, ident.clone())
                            .map(|rvalue| (rvalue.clone().into(), im::AValue::RValue(rvalue, ident.span)))
                    }
                    ast::Call::IdentArguments { .. } => { todo!() }
                    ast::Call::String(_, _) => { todo!() }
                    ast::Call::Number(_, _) => { todo!() }
                }.map(|acc| (acc.0, Some(acc.1)))
            })
}