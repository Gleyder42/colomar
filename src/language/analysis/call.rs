use smallvec::smallvec;
use crate::language::analysis::decl::DeclQuery;
use crate::language::analysis::interner::IntoInternId;
use crate::language::analysis::namespace::{Nameholder, Nameholders};
use crate::language::analysis::QueryTrisult;
use crate::language::im::CValue;
use crate::language::{ast, im};

pub(super) fn query_call_chain(
    db: &dyn DeclQuery,
    nameholders: Nameholders,
    call_chain: ast::CallChain,
) -> QueryTrisult<im::AValue> {
    assert!(
        !call_chain.value.is_empty(),
        "A call chain cannot be empty, but was"
    );

    QueryTrisult::<ast::CallChain>::from(call_chain).fold_flat_map::<im::AValue, _, _, _>(
        (nameholders, None),
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
        |(_, value)| value.unwrap(),
        |(nameholders, _value), call| {
            match *call {
                ast::Call::Ident(ident) => db
                    .query_namespaced_rvalue(nameholders, ident.clone())
                    .map(|rvalue| {
                        (
                            smallvec![rvalue.clone().into()],
                            im::AValue::RValue(rvalue, ident.span),
                        )
                    }),
                ast::Call::IdentArguments { name, args, .. } => db
                    .query_namespaced_function(nameholders, name)
                    .and_or_default(
                        args.into_iter()
                            .map(|call_chain| {
                                db.query_call_chain(smallvec![Nameholder::Root], call_chain)
                            })
                            .collect::<QueryTrisult<_>>(),
                    )
                    .map(|(function_decl, function_args)| {
                        (
                            smallvec![function_decl.return_type.clone().into()],
                            im::AValue::FunctionCall(function_decl.intern(db), function_args),
                        )
                    }),
                ast::Call::String(ident, span) => {
                    db.query_string_type().map(|string_struct_decl| {
                        (
                            smallvec![Nameholder::Empty],
                            im::AValue::CValue(CValue::String(ident, string_struct_decl, span)),
                        )
                    })
                }
                ast::Call::Number(_, _) => {
                    todo!()
                }
            }
            .map(|(acc, avalue)| (acc, Some(avalue)))
        },
    )
}
