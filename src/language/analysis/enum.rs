use crate::language::{ast, im};
use crate::language::analysis::interner::{Interner, IntoInternId};

#[salsa::query_group(EnumDatabase)]
pub trait EnumQuery: EnumDeclQuery + Interner {

    fn query_enum(&self, r#enum: ast::Enum) -> im::Enum;

    fn find_enum_def(&self, enum_decl: im::EnumDeclarationId) -> im::Enum;
}

fn find_enum_def(db: &dyn EnumQuery, enum_decl: im::EnumDeclarationId) -> im::Enum {
    todo!()
}

fn query_enum(db: &dyn EnumQuery, r#enum: ast::Enum) -> im::Enum {
    let declaration = db.query_enum_decl(r#enum.declaration);

    im::Enum {
        declaration,
        definition: im::EnumDefinition {
            constants: r#enum.definition.constants.into_iter()
                .map(|name| im::EnumConstant {
                    name,
                    r#enum: declaration
                }.intern(db)).collect()
        },
        span: r#enum.span
    }
}

#[salsa::query_group(EnumDeclarationDatabase)]
pub trait EnumDeclQuery: Interner {

    fn query_enum_decl(&self, r#enum: ast::EnumDeclaration) -> im::EnumDeclarationId;
}

fn query_enum_decl(db: &dyn EnumDeclQuery, r#enum: ast::EnumDeclaration) -> im::EnumDeclarationId {
    im::EnumDeclaration {
        name: r#enum.name,
        is_workshop: r#enum.is_workshop,
    }.intern(db)
}
