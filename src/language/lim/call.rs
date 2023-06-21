use crate::language::analysis::QueryTrisult;
use crate::language::im;
use crate::language::im::{AValue, CValue, RValue};
use crate::language::lim::def;
use crate::language::lim::def::LimDefQuery;
use crate::language::lim::tree::Call;
use chumsky::primitive::Container;

pub(super) fn query_lim_call(
    db: &dyn LimDefQuery,
    avalue_chain: im::AValueChain,
) -> QueryTrisult<Call> {
    QueryTrisult::Ok(avalue_chain.avalues).fold::<Option<AValue>, _>(None, |acc, current| todo!());

    todo!()
}
