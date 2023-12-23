mod call;
mod rule;
mod variables;

use crate::compiler::analysis::def::DefQuery;
use crate::compiler::analysis::interner::Interner as AnalysisInterner;
use crate::compiler::loader::WorkshopScriptLoader;
use crate::compiler::wst::Variable;
use crate::compiler::{cir, wst, AssignMod, Ident, QueryTrisult};

const CALLER_PLACEHOLDER: &str = "$caller$";
const ASSIGMENT_PLACEHOLDER: &str = "$value$";

pub type Assigner = (wst::Call, Option<AssignMod>);

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
        action: cir::Action,
    ) -> QueryTrisult<wst::Call>;

    /// Impl: [call::query_const_eval]
    #[salsa::invoke(call::query_const_eval)]
    fn query_const_eval(&self, call: wst::Call) -> QueryTrisult<wst::Ident>;

    /// Impl: [call::query_wst_call_by_avalue]
    #[salsa::invoke(call::query_wst_call_by_avalue)]
    fn query_wst_call_by_avalue(
        &self,
        caller: Option<Caller>,
        assigner: Option<Assigner>,
        avalue: cir::AValue,
    ) -> QueryTrisult<Option<wst::Call>>;

    /// Impl: [call::query_wst_call_from_args]
    #[salsa::invoke(call::query_wst_call_from_args)]
    fn query_wst_call_from_args(
        &self,
        decl_args: cir::DeclArgIds,
        called_args: cir::CalledArgs,
    ) -> Vec<Arg>;

    /// Impl: [variables::query_player_variables]
    #[salsa::invoke(variables::query_player_variables)]
    fn query_player_variables(&self) -> QueryTrisult<Vec<Variable>>;
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Caller {
    wst: Option<wst::Call>,
    cir: cir::AValue,
}

impl Caller {
    pub fn new(wst: Option<wst::Call>, cir: cir::AValue) -> Caller {
        Caller { wst, cir }
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Arg {
    index: usize,
    name: Ident,
    is_vararg: bool,
    value: cir::AValueChain,
}
