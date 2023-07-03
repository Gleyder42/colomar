mod call;
mod rule;

use crate::compiler::analysis::def::DefQuery;
use crate::compiler::analysis::interner::Interner as AnalysisInterner;
use crate::compiler::loader::WorkshopScriptLoader;
use crate::compiler::{cir, wst, QueryTrisult};

#[salsa::query_group(CodegenDatabase)]
pub trait Codegen: WorkshopScriptLoader + AnalysisInterner + DefQuery {
    /// Impl: [rule::query_wst_rule]
    #[salsa::invoke(rule::query_wst_rule)]
    fn query_wst_rule(&self, rule: cir::Rule) -> QueryTrisult<wst::Rule>;

    /// Impl: [call::query_wst_call]
    #[salsa::invoke(call::query_wst_call)]
    fn query_wst_call(
        &self,
        caller: Option<Caller>,
        avalue_chain: cir::AValueChain,
    ) -> QueryTrisult<wst::Call>;

    /// Impl: [call::query_const_eval]
    #[salsa::invoke(call::query_const_eval)]
    fn query_const_eval(&self, call: wst::Call) -> QueryTrisult<wst::Ident>;

    /// Impl: [call::query_wst_call_by_avalue]
    #[salsa::invoke(call::query_wst_call_by_avalue)]
    fn query_wst_call_by_avalue(
        &self,
        caller: Option<Caller>,
        avalue: cir::AValue,
    ) -> QueryTrisult<Option<wst::Call>>;
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Caller {
    pub wst: Option<wst::Call>,
    pub cir: cir::AValue,
}
