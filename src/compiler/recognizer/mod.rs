mod call;
mod interner;
mod func;

use crate::compiler::{cir, QueryTrisult, wir, wst};
use crate::compiler::analysis::decl::DeclQuery;
use crate::compiler::loader::WorkshopScriptLoader;
use crate::compiler::recognizer::interner::Interner;
use crate::compiler::analysis::interner::Interner as AnalysisInterner;

#[salsa::query_group(RecognizerDatabase)]
pub trait Recognizer: WorkshopScriptLoader + AnalysisInterner {

    /// Impl: [call::query_wir_call]
    #[salsa::invoke(call::query_wir_call)]
    fn query_wir_call(&self, caller: Option<Caller>, avalue_chain: cir::AValueChain) -> QueryTrisult<wst::Call>;

    /// Impl: [func::query_wir_function_id]
    #[salsa::invoke(func::query_wir_function_id)]
    fn query_wir_function_id(&self, function: cir::FunctionDecl) -> QueryTrisult<wst::Function>;

    /// Impl: [call::query_wst_call_by_avalue]
    #[salsa::invoke(call::query_wst_call_by_avalue)]
    fn query_wst_call_by_avalue(&self, caller: Option<Caller>, avalue: cir::AValue) -> QueryTrisult<wst::Call>;
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Caller {
    pub wst: Option<wst::Call>,
    pub cir: cir::AValue
}