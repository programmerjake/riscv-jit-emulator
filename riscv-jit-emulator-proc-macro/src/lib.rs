// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use riscv_instructions::{ast, parse_instr_table, parse_rvc_instr_table};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
};

fn parse_instructions() -> impl Iterator<Item = ast::Instruction> {
    parse_instr_table()
        .sections
        .into_iter()
        .flat_map(|v| v.instructions)
        .chain(
            parse_rvc_instr_table()
                .sections
                .into_iter()
                .flat_map(|v| v.instructions),
        )
}

#[derive(Debug)]
struct DecoderInput {}

impl Parse for DecoderInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(DecoderInput {})
    }
}

#[derive(Clone, Copy, Debug)]
enum IdentKind {
    Type,
}

fn opcode_name_to_ident(prefix: &str, name: &ast::OpcodeName, kind: IdentKind) -> Ident {
    let mut retval = prefix.to_string();
    let mut beginning_of_word = true;
    for ch in name.name.chars() {
        match (kind, ch, beginning_of_word) {
            (IdentKind::Type, 'A'..='Z', true) => {
                retval.push(ch);
                beginning_of_word = false;
            }
            (IdentKind::Type, 'A'..='Z', false) | (IdentKind::Type, '0'..='9', false) => {
                retval.push(ch.to_ascii_lowercase());
            }
            (IdentKind::Type, '.', false) => {
                beginning_of_word = true;
            }
            _ => todo!(
                "unhandled opcode name char: ch={ch:?}, \
                 kind={kind:?}, beginning_of_word={beginning_of_word:?}",
                ch = ch,
                kind = kind,
                beginning_of_word = beginning_of_word,
            ),
        }
    }
    Ident::new(&retval, Span::call_site())
}

impl DecoderInput {
    fn to_tokens(&self) -> syn::Result<TokenStream> {
        let mut instructions = Vec::new();
        for instruction in parse_instructions() {
            let struct_name = opcode_name_to_ident("", &instruction.opcode, IdentKind::Type);
            dbg!(instruction);
            instructions.push(quote! {
                pub struct #struct_name {}
            });
        }
        Ok(quote! {
            #(#instructions)*
        })
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
