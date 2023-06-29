mod call;

use crate::compiler::analysis::interner::Interner as AnalysisInterner;
use crate::compiler::loader::WorkshopScriptLoader;
use crate::compiler::{cir, wst, QueryTrisult};

#[salsa::query_group(CodegenDatabase)]
pub trait Codegen: WorkshopScriptLoader + AnalysisInterner {
    /// Impl: [call::query_wst_call]
    #[salsa::invoke(call::query_wst_call)]
    fn query_wst_call(
        &self,
        caller: Option<Caller>,
        avalue_chain: cir::AValueChain,
    ) -> QueryTrisult<wst::Call>;

    /// Impl: [call::query_wst_call_by_avalue]
    #[salsa::invoke(call::query_wst_call_by_avalue)]
    fn query_wst_call_by_avalue(
        &self,
        caller: Option<Caller>,
        avalue: cir::AValue,
    ) -> QueryTrisult<wst::Call>;
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Caller {
    pub wst: Option<wst::Call>,
    pub cir: cir::AValue,
}
