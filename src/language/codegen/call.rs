use crate::language::analysis::QueryTrisult;
use crate::language::codegen::def::LimDefQuery;
use crate::language::im;
use crate::language::im::AValue;
use crate::language::lim::Call;

pub(super) fn query_lim_call(
    db: &dyn LimDefQuery,
    avalue_chain: im::AValueChain,
) -> QueryTrisult<Call> {
    QueryTrisult::Ok(avalue_chain.avalues).fold_with::<Option<AValue>, Option<Call>, _>(
        None,
        None,
        |acc, ctx, avalue| todo!(),
    );

    todo!()
}
