use crate::language::{ast, im};
use crate::language::analysis::r#enum::EnumQuery;
use crate::language::ast::Root;

#[salsa::query_group(FileDatabase)]
pub trait RootFileQuery {

    #[salsa::input]
    fn input_content(&self) -> ast::Ast;
}

