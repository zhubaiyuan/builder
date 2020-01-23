extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use std::fmt;
use syn::parenthesized;

use syn::parse::Result as SynResult;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn builder_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).expect("Could not parse type to derive Builder for");

    impl_builder_macro(ast)
}

fn impl_builder_macro(ty: syn::DeriveInput) -> TokenStream {
    match parse_builder_information(ty) {
        Ok(info) => info.into(),
        Err(e) => to_compile_errors(e).into(),
    }
}

fn to_compile_errors(errors: Vec<syn::Error>) -> proc_macro2::TokenStream {
    let compile_errors = errors.iter().map(syn::Error::to_compile_error);
    quote! { #(#compile_errors)* }
}

type MultiResult<T> = std::result::Result<T, Vec<syn::Error>>;

fn parse_builder_information(ty: syn::DeriveInput) -> MultiResult<BuilderInfo> {
    use syn::spanned::Spanned;
    use syn::Data;

    let span = ty.span();
    let syn::DeriveInput {
        ident,
        generics,
        data,
        attrs,
        ..
    } = ty;

    match data {
        Data::Struct(struct_) => parse_builder_struct(struct_, ident, generics, attrs, span),
        _ => Err(vec![syn::Error::new(
            span,
            "Can only derive `Builder` for a struct",
        )]),
    }
}

#[derive(Debug, Default)]
struct SyntaxErrors {
    inner: Vec<syn::Error>,
}

impl SyntaxErrors {
    fn add<D, T>(&mut self, tts: T, description: D)
    where
        D: fmt::Display,
        T: quote::ToTokens,
    {
        self.inner.push(syn::Error::new_spanned(tts, description));
    }

    fn extend(&mut self, errors: Vec<syn::Error>) {
        self.inner.extend(errors);
    }

    fn finish(self) -> MultiResult<()> {
        if self.inner.is_empty() {
            Ok(())
        } else {
            Err(self.inner)
        }
    }
}

struct BuilderInfo {
    name: syn::Ident,
    generics: syn::Generics,
    fields: Vec<(Option<syn::Ident>, syn::Type, Vec<BuilderAttribute>)>,
}

fn attributes_from_syn(attrs: Vec<syn::Attribute>) -> MultiResult<Vec<BuilderAttribute>> {
    use syn::parse2;

    let mut ours = Vec::new();
    let mut errs = Vec::new();

    let parsed_attrs = attrs.into_iter().filter_map(|attr| {
        if attr.path.is_ident("builder") {
            Some(parse2::<BuilderAttributeBody>(attr.tokens).map(|body| body.0))
        } else {
            None
        }
    });

    for attr in parsed_attrs {
        match attr {
            Ok(v) => ours.extend(v),
            Err(e) => errs.push(e),
        }
    }

    if errs.is_empty() {
        Ok(ours)
    } else {
        Err(errs)
    }
}

enum BuilderAttribute {
    Required(proc_macro2::TokenStream),
}

impl syn::parse::Parse for BuilderAttribute {
    fn parse(input: syn::parse::ParseStream) -> SynResult<Self> {
        use syn::Ident;

        let input_tts = input.cursor().token_stream();
        let name: Ident = input.parse()?;
        if name == "required" {
            Ok(BuilderAttribute::Required(input_tts))
        } else {
            Err(syn::Error::new(
                name.span(),
                "expected `required`",
            ))
        }
    }
}

struct BuilderAttributeBody(Vec<BuilderAttribute>);

impl syn::parse::Parse for BuilderAttributeBody {
    fn parse(input: syn::parse::ParseStream) -> SynResult<Self> {
        use syn::punctuated::Punctuated;
        use syn::token::Comma;

        let inside;
        parenthesized!(inside in input);

        let parse_comma_list = Punctuated::<BuilderAttribute, Comma>::parse_terminated;
        let list = parse_comma_list(&inside)?;

        Ok(BuilderAttributeBody(
            list.into_pairs().map(|p| p.into_value()).collect(),
        ))
    }
}
