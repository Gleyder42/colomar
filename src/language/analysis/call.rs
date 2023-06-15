use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::interner::IntoInternId;
use crate::language::analysis::namespace::{Nameholder, Nameholders};
use crate::language::analysis::QueryTrisult;
use crate::language::im::{AValueChain, CValue};
use crate::language::{ast, im};
use smallvec::smallvec;

pub(super) fn query_call_chain(
    db: &dyn DeclQuery,
    inital_nameholders: Nameholders,
    call_chain: ast::CallChain,
) -> QueryTrisult<AValueChain> {
    assert!(
        !call_chain.value.is_empty(),
        "A call chain cannot be empty, but was"
    );
    let span = call_chain.span;

    QueryTrisult::from_iter(call_chain.value).fold_flat_map::<AValueChain, _, _, _>(
        (inital_nameholders.clone(), Vec::<im::AValue>::new()),
        // func refers to the closure processing the call.
        // map_func refers to the closure unwrapping the value .
        //
        // We are allowed to unwrap here, because we know value must be some.
        // The call chain is not null, so we hava at least one call to operate on.
        // fold_map does not guarantee to call func by itself, because in case of Err,
        // func will not be called. However, QueryResult::from(ident_chain) returns Ok,
        // so func will definitely be called.
        // If the result returned in func is error, value could be none, but it doesn't matter
        // as map_func in that case is not called
        |(_, avalues)| AValueChain::new(avalues, span),
        |(nameholders, mut avalues), call| {
            match *call {
                ast::Call::Ident(ident) => db
                    .query_namespaced_rvalue(nameholders, ident.clone())
                    .map(|rvalue| {
                        (
                            smallvec![rvalue.clone().into()],
                            im::AValue::RValue(rvalue, ident.span),
                        )
                    }),
                ast::Call::IdentArguments { name, args, span } => db
                    .query_namespaced_function(nameholders, name)
                    .and_or_default(
                        args.into_iter()
                            .map(|call_chain| {
                                db.query_call_chain(inital_nameholders.clone(), call_chain)
                            })
                            .collect::<QueryTrisult<Vec<AValueChain>>>(),
                    )
                    .flat_map(|(function_decl, called_avalue_args)| {
                        db.query_called_args_by_chain(called_avalue_args, function_decl.arguments.clone())
                            .intern_inner(db)
                            .map(|args| (function_decl, args))
                    })
                    .map(|(function_decl, function_args)| {
                        (
                            smallvec![function_decl.return_type.clone().into()],
                            im::AValue::FunctionCall(function_decl.intern(db), function_args, span),
                        )
                    }),
                ast::Call::String(ident, span) => db.query_string_type().map(|string_struct_id| {
                    (
                        smallvec![Nameholder::Empty],
                        im::AValue::CValue(CValue::String(ident, string_struct_id, span)),
                    )
                }),
                ast::Call::Number(ident, span) => db.query_number_type().map(|number_struct_id| {
                    (
                        smallvec![Nameholder::Empty],
                        im::AValue::CValue(CValue::Number(ident, number_struct_id, span)),
                    )
                }),
            }
            .map(|(acc, avalue)| {
                avalues.push(avalue);
                (acc, avalues)
            })
        },
    )
}
