use std::rc::Rc;
use crate::language::{ast, Ident, im};
use crate::language::analysis::error::AnalysisError;
use crate::language::analysis::namespace::{NamespaceId, NamespacePlaceholder, NamespaceTrait};
use crate::language::im::{AValue, CValue, RValue, Type};

#[salsa::query_group(CallDatabase)]
pub trait Call: NamespaceTrait {

    fn query_ident(&self, ident: Ident) -> im::AValue;

    fn query_call_chain(&self, namespace: NamespacePlaceholder, call_chain: ast::CallChain) -> Result<im::AValue, AnalysisError>;
}

fn query_ident(db: &dyn Call, ident: Ident) -> im::AValue {
    todo!()
}

fn query_call_chain(db: &dyn Call, namespace: NamespacePlaceholder, call_chain: ast::CallChain) -> Result<im::AValue, AnalysisError> {
/*    let mut current_namespace = namespace;
    let mut current_value: Option<AValue>  = None;

    for call in call_chain {
        if let Some(ref value) = current_value {
            current_namespace = match value {
                AValue::RValue(RValue::Type(r#type), _) => r#type.clone().into(),
                AValue::RValue(RValue::EnumConstant(_), _) => NamespacePlaceholder::Empty,
                AValue::CValue(cvalue) => todo!("NamespacePlaceholder::Struct(cvalue.clone().r#struct())"),
            }
        }

        match *call {
            ast::Call::Ident(ident) => {
                let rvalue = db.query_namespaced_rvalue(current_namespace, ident)?;
                current_value = Some(AValue::RValue(rvalue, ident.span.clone()))
            }
            ast::Call::IdentArguments { name, args, span } => {}
            ast::Call::String(string, span) => {

            }
            ast::Call::Number(number, span) => {
                current_value = Some(AValue::CValue(CValue::Number(number, span)))
            }
        }
    }*/
    todo!()
}
