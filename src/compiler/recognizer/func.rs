use crate::compiler::recognizer::Recognizer;
use crate::compiler::{cir, QueryTrisult, wir, wst};

pub(super) fn query_wir_function_id(db: &dyn Recognizer, function: cir::FunctionDecl) -> QueryTrisult<wst::Function> {
    todo!()
}