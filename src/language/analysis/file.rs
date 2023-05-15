use crate::language::{ast};

#[salsa::query_group(FileDatabase)]
pub trait RootFileQuery {

    #[salsa::input]
    fn input_content(&self) -> ast::Ast;
}

