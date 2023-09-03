use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::analysis::interner::IntoInternId;
use crate::compiler::analysis::namespace::{Nameholder, Nameholders};
use crate::compiler::cir::{AValueChain, CValue, TypeComparison};
use crate::compiler::cst::CallArgument;
use crate::compiler::error::CompilerError;
use crate::compiler::span::Spanned;
use crate::compiler::QueryTrisult;
use crate::compiler::{cir, cst};
use either::Either;
use smallvec::smallvec;

pub(super) fn query_expr(
    db: &dyn DeclQuery,
    inital_nameholders: Nameholders,
    enforce_bool: bool,
    expr: cst::Expr,
) -> QueryTrisult<cir::Expr> {
    type Lhs = Box<cst::Expr>;
    type Rhs = Lhs;
    let binary_op = |lhs: Lhs, rhs: Rhs, expr: fn(_, _) -> _| {
        let lhs = db.query_expr(inital_nameholders.clone(), enforce_bool, *lhs);
        let rhs = db.query_expr(inital_nameholders.clone(), enforce_bool, *rhs);
        lhs.and(rhs).flat_map(|(lhs, rhs)| {
            let expr: cir::Expr = expr(Box::new(lhs), Box::new(rhs));
            // We can safely unwrap here, because we know the created expression will have lhs and rhs
            let lhs = db.checked_return_avalue(expr.lhs().cloned().unwrap());
            let rhs = db.checked_return_avalue(expr.rhs().cloned().unwrap());

            lhs.and(rhs)
                .flat_map(|(lhs, rhs)| db.check_equal_return_avalue(lhs, rhs))
                .map(|_| expr)
        })
    };

    match expr {
        cst::Expr::Chain(call_chain) => db
            .query_call_chain(inital_nameholders, call_chain)
            .soft_filter_with(db.query_bool_type(), |chain, id| {
                let return_avalue = chain.returning_avalue();
                if !enforce_bool || return_avalue.return_called_type(db) == id {
                    Ok(())
                } else {
                    Err(CompilerError::WrongType {
                        actual: return_avalue.return_called_type(db),
                        expected: Either::Left(id.into()),
                    })
                }
            })
            .map(cir::Expr::Chain),
        cst::Expr::Neg(neg) => db
            .query_expr(inital_nameholders, enforce_bool, *neg)
            .map(|expr| cir::Expr::Neg(Box::new(expr))),
        cst::Expr::And(lhs, rhs) => binary_op(lhs, rhs, cir::Expr::And),
        cst::Expr::Or(lhs, rhs) => binary_op(lhs, rhs, cir::Expr::Or),
    }
}

pub(super) fn checked_return_avalue(
    db: &dyn DeclQuery,
    expr: cir::Expr,
) -> QueryTrisult<cir::AValue> {
    match expr {
        cir::Expr::Chain(chain) => QueryTrisult::Ok(chain.returning_avalue()),
        cir::Expr::Neg(neg) => db.checked_return_avalue(*neg),
        cir::Expr::And(lhs, rhs) | cir::Expr::Or(lhs, rhs) => {
            let lhs = db.checked_return_avalue(*lhs);
            let rhs = db.checked_return_avalue(*rhs);

            lhs.and(rhs)
                .flat_map(|(lhs, rhs)| db.check_equal_return_avalue(lhs, rhs))
        }
    }
}

pub(super) fn check_equal_return_avalue(
    db: &dyn DeclQuery,
    lhs: cir::AValue,
    rhs: cir::AValue,
) -> QueryTrisult<cir::AValue> {
    db.query_bool_type().flat_map(|bool_id| {
        let both_sides_same_type = lhs
            .return_called_type(db)
            .has_same_return_type(&rhs.return_called_type(db));
        let type_is_bool = rhs.return_called_type(db).has_same_return_type(&bool_id);

        if both_sides_same_type && type_is_bool {
            QueryTrisult::Ok(rhs)
        } else {
            // TODO Add info
            QueryTrisult::Par(
                rhs.clone(),
                vec![CompilerError::WrongTypeInBinaryExpression(lhs, rhs)],
            )
        }
    })
}

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

    QueryTrisult::Ok(call_chain.value).fold_flat_map(
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
                            smallvec![Nameholder::from_rvalue(rvalue, db)],
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
                                        // TODO Call argument should also hava an expression
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
