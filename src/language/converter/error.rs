use crate::language::{ast, Ident};

pub enum ConverterError {
    DuplicateIdent {
        first: Ident,
        second: Ident
    },
    CannotResolveIdent {
        ident: Ident
    },
    MismatchedTypes {
        requested_ident: Ident,
        requested_type: ast::Types,
        resolved: Ident,
        resolved_type: ast::Types
    }
}

impl<T> Into<Result<T, ConverterError>> for ConverterError {
    fn into(self) -> Result<T, ConverterError> {
        return Err(self);
    }
}

trait ErrorHandler {

    fn add_errors(self, errors: &mut Vec<ConverterError>);
}

impl<T> ErrorHandler for Result<T, ConverterError> {

    fn add_errors(self, errors: &mut Vec<ConverterError>) {
        match self {
            Err(error) => errors.push(error),
            Ok(_) => {}
        }
    }
}

impl<T> ErrorHandler for Result<T, Vec<ConverterError>> {

    fn add_errors(self, errors: &mut Vec<ConverterError>) {
        match self {
            Err(mut error) => errors.append(&mut error),
            Ok(_) => {}
        }
    }
}