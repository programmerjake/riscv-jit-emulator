// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
use proc_macro2::TokenStream;
use quote::quote;
use riscv_jit_emulator_instruction_table_parser::{ast, parse_instr_table, parse_rvc_instr_table};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
};

mod instr_table;

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

impl DecoderInput {
    fn to_tokens(&self) -> syn::Result<TokenStream> {
        for instruction in parse_instructions() {
            dbg!(instruction);
            //todo!()
        }
        Ok(quote! {})
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
