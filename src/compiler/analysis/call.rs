use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::analysis::interner::IntoInternId;
use crate::compiler::analysis::namespace::{Nameholder, Nameholders};
use crate::compiler::cir::{AValueChain, CValue};
use crate::compiler::cst::CallArgument;
use crate::compiler::span::Spanned;
use crate::compiler::QueryTrisult;
use crate::compiler::{cir, cst};
use smallvec::smallvec;

pub(super) fn query_call_chain(
    db: &dyn DeclQuery,
    inital_nameholders: Nameholders,
    call_chain: cst::CallChain,
) -> QueryTrisult<AValueChain> {
    assert!(
        !call_chain.value.is_empty(),
        "A call chain cannot be empty, but was"
    );
    let span = call_chain.span;

    QueryTrisult::from_iter(call_chain.value).fold_flat_map(
        (inital_nameholders.clone(), Vec::<cir::AValue>::new()),
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
                cst::Call::Ident(ident) => db
                    .query_namespaced_rvalue(nameholders, ident.clone())
                    .map(|rvalue| {
                        (
                            smallvec![Nameholder::from_rvalue(rvalue.clone(), db)],
                            cir::AValue::RValue(rvalue, ident.span),
                        )
                    }),
                cst::Call::IdentArguments { name, args, span } => {
                    let args_span = args.span;

                    db.query_namespaced_function(nameholders, name)
                        .and_or(
                            args.value
                                .into_iter()
                                .map(|call_argument| {
                                    db.query_call_chain(
                                        inital_nameholders.clone(),
                                        call_argument.clone().call_chain(),
                                    )
                                    .map(|it| {
                                        match call_argument {
                                            CallArgument::Named(name, _, _) => (Some(name), it),
                                            CallArgument::Pos(_) => (None, it),
                                        }
                                    })
                                })
                                .collect::<QueryTrisult<Vec<_>>>()
                                .spanned(args_span),
                            Spanned::default_inner(args_span),
                        )
                        .flat_map(|(function_decl, called_avalue_args)| {
                            db.query_called_args(
                                called_avalue_args,
                                function_decl.arguments.clone(),
                            )
                            .intern_inner(db)
                            .map(|args| (function_decl, args))
                        })
                        .map(|(function_decl, function_args)| {
                            (
                                smallvec![function_decl.return_type.into()],
                                cir::AValue::FunctionCall(
                                    function_decl.intern(db),
                                    function_args,
                                    span,
                                ),
                            )
                        })
                }
                cst::Call::String(ident, span) => db.query_string_type().map(|string_struct_id| {
                    (
                        smallvec![Nameholder::Empty],
                        cir::AValue::CValue(CValue::String(ident, string_struct_id, span)),
                    )
                }),
                cst::Call::Number(ident, span) => db.query_number_type().map(|number_struct_id| {
                    (
                        smallvec![Nameholder::Empty],
                        cir::AValue::CValue(CValue::Number(ident, number_struct_id, span)),
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
