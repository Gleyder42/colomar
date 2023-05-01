use std::collections::HashMap;
use std::rc::Rc;
use crate::language::converter::error::ConverterError;
use crate::language::{Ident, im, ImmutableString};
use crate::language::im::RValue;

pub struct Namespace {
    parents: Vec<Rc<Namespace>>,
    map: HashMap<ImmutableString, RValue>,
}

impl Namespace {

    pub fn new_root() -> Namespace {
        Namespace { map: HashMap::new(), parents: Vec::new() }
    }

    pub fn add(&mut self, ident: Ident, rvalue: RValue) -> Result<(), ConverterError> {
        match self.get(&ident) {
            Some(rvalue) => {
                ConverterError::DuplicateIdent {
                    first: rvalue.name(),
                    second: ident
                }.into()
            }
            None => {
                self.map.insert(ident.value.clone(), rvalue);
                Ok(())
            }
        }
    }

    pub fn get(&self, ident: &Ident) -> Option<RValue> {
        let option = self.map.get(&ident.value).map(|it| it.clone());
        if option.is_some() {
            option
        } else {
            self.parents.iter()
                .map(|it| it.get(&ident))
                .filter(|it| it.is_some())
                .flatten()
                .collect::<Vec<_>>()
                .pop()
        }
    }

    pub fn try_with_struct(r#struct: &im::StructRef) -> Result<Namespace, ConverterError> {
        let mut namespace = Namespace::new_root();
        namespace.add_struct_properties(r#struct)?;
        namespace.add_struct_functions(r#struct)?;
        Ok(namespace)
    }

    pub fn try_with_enum(r#enum: &im::EnumDefinition) -> Result<Namespace, ConverterError> {
        let mut namespace = Namespace::new_root();
        namespace.add_enum_constants(&r#enum)?;
        Ok(namespace)
    }

    pub fn add_struct_properties(&mut self, r#struct: &im::StructRef) -> Result<(), ConverterError> {
        for property in &r#struct.properties {
            self.add(property.name.clone(), RValue::Property(Rc::clone(&property)))?;
        }
        Ok(())
    }

    pub fn add_struct_functions(&mut self, r#struct: &im::StructRef) -> Result<(), ConverterError> {
        for function in &r#struct.functions {
            self.add(function.name.clone(), RValue::Function(Rc::clone(&function)))?;
        }
        Ok(())
    }

    pub fn add_enum_constants(&mut self, r#enum: &im::EnumDefinition) -> Result<(), ConverterError> {
        for enum_constant in &r#enum.constants {
            self.add(enum_constant.name.clone(), RValue::EnumConstant(Rc::clone(enum_constant)))?;
        };
        return Ok(());
    }
}