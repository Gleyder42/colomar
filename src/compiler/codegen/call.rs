use crate::compiler::cir::{AValue, CValue, RValue};
use crate::compiler::codegen::def::LimDefQuery;
use crate::compiler::error::CompilerError;
use crate::compiler::wir::{Call, LiteralOwscript};
use crate::compiler::{cir, QueryTrisult};
use crate::query_error;

pub(super) fn query_lim_call(
    db: &dyn LimDefQuery,
    avalue_chain: cir::AValueChain,
) -> QueryTrisult<Call> {
    QueryTrisult::Ok(avalue_chain.avalues)
        .fold_with::<QueryTrisult<LiteralOwscript>, Option<Call>, _>(
            query_error!(CompilerError::NoCaller),
            None,
            |caller, _acc, avalue| {
                let _call: QueryTrisult<Call> = match avalue {
                    AValue::FunctionCall(_, _, _) => {
                        todo!()
                    }
                    AValue::RValue(RValue::Property(property_decl), _) => {
                        db.query_owscript_property_impl(property_decl)
                            .and_require(caller)
                            .map(|(script, caller)| script.saturate(caller));

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
