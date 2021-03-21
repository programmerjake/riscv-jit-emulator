// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
use proc_macro2::TokenStream;
use quote::quote;
use std::env;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token::Paren,
    LitStr, Token,
};

mod instr_table;

mod kw {
    syn::custom_keyword!(env);
    syn::custom_keyword!(concat);
    syn::custom_keyword!(instr_table);
}

#[derive(Debug)]
enum StringSegment {
    Concat {
        concat: kw::concat,
        bang: Token![!],
        paren: Paren,
        segments: Punctuated<StringSegment, Token![,]>,
    },
    Env {
        env: kw::env,
        bang: Token![!],
        paren: Paren,
        var_name: LitStr,
    },
    String(LitStr),
}

impl StringSegment {
    fn to_str(&self) -> syn::Result<LitStr> {
        match self {
            StringSegment::Concat {
                concat, segments, ..
            } => {
                let mut span = None;
                let mut retval = String::new();
                for segment in segments {
                    let segment_str = segment.to_str()?;
                    if span.is_none() {
                        span = Some(segment_str.span());
                    }
                    retval.push_str(&segment_str.value());
                }
                Ok(LitStr::new(
                    &retval,
                    span.unwrap_or_else(|| concat.span.clone()),
                ))
            }
            StringSegment::Env { env, var_name, .. } => {
                let value =
                    env::var(var_name.value()).map_err(|e| syn::Error::new_spanned(var_name, e))?;
                Ok(LitStr::new(&value, env.span.clone()))
            }
            StringSegment::String(v) => Ok(v.clone()),
        }
    }
}

impl Parse for StringSegment {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::concat) {
            let inner;
            Ok(Self::Concat {
                concat: input.parse()?,
                bang: input.parse()?,
                paren: parenthesized!(inner in input),
                segments: inner.parse_terminated(StringSegment::parse)?,
            })
        } else if input.peek(kw::env) {
            let inner;
            Ok(Self::Env {
                env: input.parse()?,
                bang: input.parse()?,
                paren: parenthesized!(inner in input),
                var_name: inner.parse()?,
            })
        } else {
            Ok(Self::String(input.parse()?))
        }
    }
}

#[derive(Debug)]
struct DecoderInput {
    instr_table_kw: kw::instr_table,
    instr_table_eq: Token![=],
    instr_table: StringSegment,
}

impl Parse for DecoderInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(DecoderInput {
            instr_table_kw: input.parse()?,
            instr_table_eq: input.parse()?,
            instr_table: input.parse()?,
        })
    }
}

impl DecoderInput {
    fn to_tokens(&self) -> syn::Result<TokenStream> {
        let instr_table_path = self.instr_table.to_str()?;
        let instr_table = instr_table::parse_file(&instr_table_path)?;
        Ok(quote! {const INSTR_TABLE_PATH: &str = #instr_table_path;})
    }
}

#[proc_macro]
pub fn gen_decoder(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DecoderInput);
    match input.to_tokens() {
        Ok(retval) => retval,
        Err(err) => err.to_compile_error(),
    }
    .into()
}
