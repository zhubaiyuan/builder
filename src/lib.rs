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

enum BuilderAttribute {
    Required(proc_macro2::TokenStream),
}
