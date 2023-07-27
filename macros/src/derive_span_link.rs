use either::Either;
use hashlink::LinkedHashMap;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{parse_str, Data, DataStruct, DeriveInput, Error, Fields, Type, TypePath};

enum DeriveError {
    InvalidStruct(Ident, &'static str),
    InvalidField(Span, &'static str),
    InvalidFieldType(Ident, Span),
    InvalidTuple(Span),
    Missing(Ident, &'static str),
}

impl Into<TokenStream> for DeriveError {
    fn into(self) -> TokenStream {
        match self {
            DeriveError::InvalidStruct(name, actual) => {
                let message = format!("{} is not a struct but a {}", name.to_string(), actual);
                Error::new(name.span(), message)
            }
            DeriveError::InvalidField(span, actual) => {
                let message = format!("Invalid fields because {}", actual);
                Error::new(span, message)
            }
            DeriveError::InvalidFieldType(name, span) => {
                let message = format!("{} must have a simple type name", name.to_string());
                Error::new(span, message)
            }
            DeriveError::Missing(name, actual) => {
                let message = format!("{} is missing {actual}", name.to_string());
                Error::new(name.span(), message)
            }
            DeriveError::InvalidTuple(span) => {
                let message = format!("The data type must be the first in a tuple");
                Error::new(span, message)
            }
        }
        .to_compile_error()
    }
}

type DeriveResult<T> = Result<T, DeriveError>;
type RootProperty = Property<LinkerRoot>;
type SpanProperty = Property<()>;
type RegularProperty = Property<TypePath>;

#[derive(Clone)]
enum LinkerRoot {
    Sid(TypePath),
    Ident(TypePath),
}

impl Into<TypePath> for LinkerRoot {
    fn into(self) -> TypePath {
        match self {
            LinkerRoot::Sid(ty) | LinkerRoot::Ident(ty) => ty,
        }
    }
}

#[derive(Clone)]
struct Property<T: Clone> {
    name: Ident,
    no_link: bool,
    ty: T,
}

#[derive(Clone)]
enum PropertyKind {
    Root(RootProperty),
    Span(SpanProperty),
    Regular(RegularProperty),
}

impl From<RootProperty> for PropertyKind {
    fn from(value: RootProperty) -> Self {
        PropertyKind::Root(value)
    }
}

impl From<SpanProperty> for PropertyKind {
    fn from(value: SpanProperty) -> Self {
        PropertyKind::Span(value)
    }
}

impl From<RegularProperty> for PropertyKind {
    fn from(value: RegularProperty) -> Self {
        PropertyKind::Regular(value)
    }
}

struct LinkerCandidate {
    name: Ident,
    root: Option<RootProperty>,
    span: SpanProperty,
    properties: Vec<RegularProperty>,
}

fn generate_span_link_impl(candidate: &LinkerCandidate) -> DeriveResult<TokenStream> {
    let name = &candidate.name;

    let root_name = match candidate.root.as_ref() {
        Some(root_property) => {
            let root_name = &root_property.name;
            quote! { Some(self.#root_name.span.clone()) }
        }
        None => {
            let span_name = &candidate.span.name;
            quote! { Some(self.#span_name.clone()) }
        }
    };

    let properties: Vec<_> = candidate
        .properties
        .iter()
        .map(|property| {
            let name = &property.name;

            if property.no_link {
                quote! {}
            } else {
                quote! { self.#name.link_to(span_name.clone()) }
            }
        })
        .collect();

    let trait_impl = quote! {
        impl compiler::span::SpanKeyLinker for #name {

            fn link_self(&self, span_name: HierSpan) {
                #(#properties;)*
            }

            fn span_name(&self) -> Option<compiler::span::HierSpan> {
                #root_name
            }
        }
    };
    Ok(trait_impl)
}

fn generate_constructor(candidate: &LinkerCandidate) -> DeriveResult<TokenStream> {
    let property_params: Vec<TokenStream> = candidate
        .properties
        .iter()
        .map(|property| {
            let name = &property.name;
            let ty = &property.ty;
            quote! { #name: impl Into<#ty> }
        })
        .collect();

    let property_inits: Vec<TokenStream> = candidate
        .properties
        .iter()
        .map(|property| {
            let name = &property.name;
            quote! { #name }
        })
        .collect();

    let property_lets: Vec<TokenStream> = candidate
        .properties
        .iter()
        .map(|property| {
            let name = &property.name;
            quote! { let #name = #name.into() }
        })
        .collect();

    let candidate_name = &candidate.name;

    let span_name = &candidate.span.name;
    let span_key_type_path = "compiler::span::SpanKey";
    let span_ty: Type =
        parse_str(span_key_type_path).expect(&format!("Cannot find {span_key_type_path}"));

    let constructor = if let Some(root_property) = candidate.root.as_ref() {
        let root_name = &root_property.name;
        let root_ty: TypePath = root_property.ty.clone().into();

        let root_init = match root_property.ty {
            LinkerRoot::Sid(ref path) => quote! { #root_name.clone() },
            LinkerRoot::Ident(ref path) => quote! { #root_name.value },
        };

        quote! {
            impl #candidate_name {

                pub fn new(#root_name: impl Into<#root_ty>, #(#property_params),*) -> Self {
                    let #root_name = #root_name.into();
                    #(#property_lets;)*

                    Self {
                        #root_name: #root_name.clone(),
                        #(#property_inits, )*
                        #span_name: #span_ty::new(#root_init).into()
                    }
                }
            }
        }
    } else {
        let string_candidate_name = candidate_name.to_string();

        quote! {
            impl #candidate_name {

                pub fn new(#(#property_params),*) -> Self {
                    #(#property_lets;)*

                    Self {
                        #(#property_inits, )*
                        #span_name: #span_ty::new(compiler::Text::from(#string_candidate_name)).into()
                    }
                }
            }
        }
    };

    Ok(constructor)
}

fn inspect_candidate(r#struct: DataStruct, name: Ident) -> DeriveResult<LinkerCandidate> {
    let fields = match r#struct.fields {
        Fields::Named(named) => named.named,
        Fields::Unnamed(field) => {
            return Err(DeriveError::InvalidField(
                field.span(),
                "has unnamed fields",
            ))
        }
        Fields::Unit => return Err(DeriveError::InvalidField(name.span(), "has no fields")),
    };

    #[derive(Hash, Eq, PartialEq)]
    enum PropertyDesc {
        Root,
        Span,
        Regular(Ident),
    }

    let mut properties: LinkedHashMap<PropertyDesc, PropertyKind> = fields
        .into_iter()
        .map(|field| {
            let name = field.ident.expect("Fields must hava a name");
            let ty_path = match field.ty {
                Type::Path(type_path) => Ok(type_path),
                _ => return Err(DeriveError::InvalidFieldType(name.clone(), field.ty.span())),
            }?;
            let simple_ty_name = ty_path
                .path
                .segments
                .iter()
                .map(|segment| segment.ident.to_string())
                .collect::<Vec<_>>()
                .join("::");

            type LinkerRootFn = Option<Either<fn(TypePath) -> LinkerRoot, ()>>;
            let linker_root: LinkerRootFn = match simple_ty_name.as_str() {
                "Sid" => Some(Either::Left(LinkerRoot::Sid)),
                "Ident" => Some(Either::Left(LinkerRoot::Ident)),
                "HierSpan" => Some(Either::Right(())),
                _ => None,
            };

            let no_link = field.attrs.iter().any(|attribute| {
                attribute
                    .path()
                    .segments
                    .last()
                    .map(|it| it.ident == "no_span")
                    .unwrap_or(false)
            });

            let property = match linker_root {
                None => (
                    PropertyDesc::Regular(name.clone()),
                    PropertyKind::Regular(Property {
                        name,
                        no_link,
                        ty: ty_path,
                    }),
                ),
                Some(Either::Left(root)) => (
                    PropertyDesc::Root,
                    PropertyKind::Root(Property {
                        name,
                        no_link,
                        ty: root(ty_path),
                    }),
                ),
                Some(Either::Right(unit)) => (
                    PropertyDesc::Span,
                    PropertyKind::Span(Property {
                        name,
                        no_link,
                        ty: unit,
                    }),
                ),
            };
            Ok(property)
        })
        .collect::<DeriveResult<_>>()?;

    let root = properties
        .remove(&PropertyDesc::Root)
        .map(|property| match property {
            PropertyKind::Root(root) => root,
            _ => unreachable!(),
        });

    let span = match properties
        .remove(&PropertyDesc::Span)
        .ok_or(DeriveError::Missing(name.clone(), "span property"))?
    {
        PropertyKind::Span(span) => span,
        _ => unreachable!(),
    };

    let regular: Vec<RegularProperty> = properties
        .into_iter()
        .map(|(_, property)| match property {
            PropertyKind::Regular(property) => property,
            _ => unreachable!(),
        })
        .collect();

    let candidate = LinkerCandidate {
        name,
        root,
        span,
        properties: regular,
    };
    Ok(candidate)
}

pub fn derive_token_stream(input: DeriveInput) -> TokenStream {
    derive(input).unwrap_or_else(|error| error.into())
}

fn derive(input: DeriveInput) -> DeriveResult<TokenStream> {
    match input.data {
        Data::Struct(r#struct) => {
            let candidate = inspect_candidate(r#struct, input.ident.clone())?;
            let constructor = generate_constructor(&candidate)?;
            let trait_impl = generate_span_link_impl(&candidate)?;

            let derive = quote! {
                #constructor

                #trait_impl
            };
            Ok(derive)
        }
        Data::Enum(_) => Err(DeriveError::InvalidStruct(input.ident.clone(), "Enum")),
        Data::Union(_) => Err(DeriveError::InvalidStruct(input.ident.clone(), "Union")),
    }
}
