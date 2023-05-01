use std::cell::RefCell;
use std::rc::Rc;
use petgraph::graph::NodeIndex;
use crate::language::{ast, Ident, im, Span};
use crate::language::ast::Root;
use crate::language::query::{NodeAdder, QueryGraph, ValidityGuarantee};

mod event;
pub mod namespace;
pub mod error;
pub mod call;

pub struct Predefined {
    pub string_primitive: im::StructRef,
    pub num_primitive: im::StructRef,
}

pub fn resolve_ast(ast: ast::Ast) -> im::Im {
    for root in ast.clone().into_iter() {
        match root {
            Root::Event(event) => {}
            Root::Rule(r#rule) => {}
            Root::Enum(r#enum) => {}
            Root::Struct(r#struct) => {}
        }
    }

    todo!()
}

fn resolve_enum_declaration(r#enum: ast::EnumDeclaration) -> Rc<im::EnumDeclaration> {
    im::EnumDeclaration {
        name: r#enum.name,
        is_workshop: r#enum.is_workshop
    }.into()
}

fn resolve_enum(r#enum: ast::Enum, cache: QueryGraph) -> Rc<im::Enum> {
    let declaration = resolve_enum_declaration(r#enum.declration);
    im::Enum {
        declaration,
        definition: im::EnumDefinition {
            constants: r#enum.definition.constants.into_iter()
                .map(|name| Rc::new(im::EnumConstant { name, r#enum: Rc::clone(&declaration) }))
                .collect()
        }.into(),
        span: r#enum.span.clone(),
    }.into()
}