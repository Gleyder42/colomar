use crate::language::{analysis, ast, im};
use crate::language::analysis::interner::{Interner, IntoInternId};

#[salsa::query_group(StructDatabase)]
pub trait StructDeclQuery: Interner {

    fn query_struct_decl(&self, r#struct: ast::StructDeclaration) -> im::StructDeclarationId;
}

fn query_struct_decl(db: &dyn StructDeclQuery, r#struct: ast::StructDeclaration) -> im::StructDeclarationId {
    im::StructDeclaration {
        name: r#struct.name,
        is_open: r#struct.is_open,
        is_workshop: r#struct.is_workshop
    }.intern(db)
}