use crate::language::analysis::QueryTrisult;
use crate::language::codegen::def::LimDefQuery;
use crate::language::im::{AValue, CValue, RValue};
use crate::language::lim::Call;
use crate::language::{im, Text};

pub(super) fn query_lim_call(
    db: &dyn LimDefQuery,
    avalue_chain: im::AValueChain,
) -> QueryTrisult<Call> {
    QueryTrisult::Ok(avalue_chain.avalues).fold_with::<Option<Text>, Option<Call>, _>(
        None,
        None,
        |caller, acc, avalue| {
            let call: QueryTrisult<Call> = match avalue {
                AValue::FunctionCall(_, _, _) => {
                    todo!()
                }
                AValue::RValue(RValue::Property(property_decl), _) => {


                    todo!()
                }
                AValue::RValue(RValue::Type(_), _) => {
                    todo!()
                }
                AValue::RValue(RValue::Function(_), _) => {
                    todo!()
                }
                AValue::RValue(RValue::EnumConstant(_), _) => {
                    todo!()
                }
                AValue::CValue(CValue::String(string, _, _)) => {
                    QueryTrisult::Ok(Call::String(string))
                }
                AValue::CValue(CValue::Number(number, _, _)) => {
                    QueryTrisult::Ok(Call::Number(number))
                }
            };

            todo!()
        },
    );

    todo!()
}
