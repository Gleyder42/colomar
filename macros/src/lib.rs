mod derive_span_link;

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use std::collections::HashMap;

use quote::quote;
use syn::spanned::Spanned;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Error, Field, Type};

type MacroResult<T> = Result<T, MacroError>;

enum ErrorKind {
    WrongType,
    InvalidName,
    InvalidStruct,
}

struct MacroError {
    span: Span,
    kind: ErrorKind,
}

impl MacroError {
    fn new(span: Span, kind: ErrorKind) -> MacroError {
        MacroError { span, kind }
    }
}

impl Into<TokenStream2> for MacroError {
    fn into(self) -> TokenStream2 {
        let message = match self.kind {
            ErrorKind::WrongType => "Wrong Type",
            ErrorKind::InvalidName => "Invalid Name",
            ErrorKind::InvalidStruct => "Invalid Struct",
        };

        Error::new(self.span, message).to_compile_error()
    }
}

struct Initializer {
    is_span: bool,
    is_ident: Option<TokenStream2>,
    param: TokenStream2,
    setup: TokenStream2,
    init: TokenStream2,
}

fn process_struct(r#struct: &DataStruct) -> MacroResult<Vec<Initializer>> {
    r#struct
        .fields
        .iter()
        .filter_map(|field| field.ident.as_ref().map(|ident| (field, ident)))
        .map(|(field, name)| process_field(&field, name))
        .collect()
}

fn process_field(field: &Field, name: &Ident) -> MacroResult<Initializer> {
    match field.ty {
        Type::Path(ref path) => {
            let ty_name_string = path
                .path
                .segments
                .first()
                .map(|segment| segment.ident.to_string())
                .ok_or(MacroError::new(name.span(), ErrorKind::InvalidName))?;

            let is_ident = match ty_name_string.as_str() {
                "Ident" => Some(quote! { #name.name }),
                "Sid" => Some(quote! { #name }),
                _ => None,
            };

            let is_span = match ty_name_string.as_str() {
                "HierSpan" => true,
                _ => false,
            };

            let param = quote! { #name: impl Into<#path> };
            let setup = quote! { let #name = #name.into() };
            let init = quote! { #name };

            Ok(Initializer {
                is_span,
                param,
                setup,
                init,
                is_ident,
            })
        }
        _ => Err(MacroError::new(field.span(), ErrorKind::WrongType)),
    }
}

fn derive_span_link(input: DeriveInput) -> MacroResult<TokenStream2> {
    let name = input.ident;

    let initializers = match input.data {
        Data::Struct(r#struct) => process_struct(&r#struct)?,
        Data::Enum(r#enum) => todo!(),
        Data::Union(_) => return Err(MacroError::new(name.span(), ErrorKind::InvalidStruct)),
    };

    let select = |select: fn(&Initializer) -> &TokenStream2, with_span: bool| {
        initializers
            .iter()
            .filter(|it| with_span || !it.is_span)
            .map(select)
            .collect::<Vec<_>>()
    };

    let params = select(|it| &it.param, false);
    let setups = select(|it| &it.setup, false);
    let inits = select(|it| &it.init, true);

    let params = quote! { #(#params),* };
    let setups = quote! { #(#setups;)* };
    let inits = quote! { #(#inits),* };

    let map = initializers
        .iter()
        .map(|init| (init.is_ident.is_some(), init))
        .collect::<HashMap<_, _>>();
    let span = map.get(&true).unwrap().is_ident.as_ref();
    let span = quote! { let span = SpanKey::new(#span.clone()).into(); };

    let constructor = quote! {
        impl #name {
            fn new(#params) -> Self {
                #setups
                #span

                #name { #inits }
            }
        }
    };
    Ok(constructor)
}

#[proc_macro_derive(NewSpanLink, attributes(no_span))]
pub fn my_macro2(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    // Build the output, possibly using quasi-quotation

    let token_stream = derive_span_link::derive_token_stream(input);

    // Hand the output tokens back to the compiler
    TokenStream::from(token_stream)
}

#[proc_macro_derive(SpanLink)]
pub fn my_macro(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    // Build the output, possibly using quasi-quotation

    let token_stream = derive_span_link(input).unwrap_or_else(|error| error.into());

    // Hand the output tokens back to the compiler
    TokenStream::from(token_stream)
}
