use std::collections::{hash_map::Entry, HashMap, HashSet};

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
    fn parse(_input: ParseStream) -> syn::Result<Self> {
        Ok(DecoderInput {})
    }
}

#[derive(Clone, Copy, Debug)]
enum IdentKind {
    Type,
    VarOrField,
}

fn name_to_ident(prefix: &str, name: &str, kind: IdentKind) -> Ident {
    let mut retval = prefix.to_string();
    let mut beginning_of_word = true;
    let mut first = true;
    for ch in name.chars() {
        match (kind, ch, beginning_of_word) {
            (IdentKind::VarOrField, 'a'..='z', _) | (IdentKind::VarOrField, '0'..='9', _) => {
                if !first && beginning_of_word {
                    retval.push('_');
                }
                retval.push(ch);
                beginning_of_word = false;
            }
            (IdentKind::VarOrField, '\'', _) => {
                if !first && !beginning_of_word {
                    retval.push('_');
                }
                beginning_of_word = false;
            }
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
        first = false;
    }
    Ident::new(&retval, Span::call_site())
}

fn get_field_name_str(v: &ast::FieldDefName) -> &str {
    match v {
        ast::FieldDefName::Macro(m) if m.content == "rdprime" => "rd'",
        ast::FieldDefName::Macro(m) if m.content == "rsoneprime" => "rs1'",
        ast::FieldDefName::Macro(m) if m.content == "rstwoprime" => "rs2'",
        ast::FieldDefName::Macro(m) => todo!("unknown macro: {:?}", m),
        ast::FieldDefName::CharTokens(v, _) => &v.content,
    }
}

impl DecoderInput {
    fn to_tokens(&self) -> syn::Result<TokenStream> {
        let mut instructions = Vec::new();
        let mut instruction_enumerants = Vec::new();
        for instruction in parse_instructions() {
            match instruction.isa_module.base {
                ast::ISABase::RV32 => continue,
                ast::ISABase::RV64 => {}
            }
            match instruction.isa_module.extension {
                ast::ISAExtension::I
                | ast::ISAExtension::M
                | ast::ISAExtension::A
                | ast::ISAExtension::F
                | ast::ISAExtension::D
                | ast::ISAExtension::C
                | ast::ISAExtension::Zicsr => {}
                ast::ISAExtension::Q | ast::ISAExtension::Zifencei => continue,
            }
            let struct_name = name_to_ident("", &instruction.name.name, IdentKind::Type);
            let doc_string = &instruction.name.name;
            struct FieldDescriptor {
                is_flag: bool,
            }
            let mut field_descriptors: HashMap<&str, FieldDescriptor> = HashMap::new();
            let mut fields = Vec::new();
            for field in &instruction.fields {
                for field_name in field.field_def.body.names() {
                    let field_name_str = get_field_name_str(field_name);
                    match field_descriptors.entry(field_name_str) {
                        Entry::Occupied(mut entry) => {
                            entry.get_mut().is_flag = false;
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(FieldDescriptor {
                                is_flag: field.instruction_bit_range.bits.len() == 1,
                            });
                        }
                    }
                }
            }
            let mut field_names = HashSet::new();
            for field in &instruction.fields {
                for field_name in field.field_def.body.names() {
                    let field_name_str = get_field_name_str(field_name);
                    if !field_names.insert(field_name_str) {
                        continue;
                    }
                    let field_descriptor = field_descriptors.get(field_name_str).unwrap();
                    let name = name_to_ident("", field_name_str, IdentKind::VarOrField);
                    let ty = if field_descriptor.is_flag {
                        quote! {bool}
                    } else {
                        quote! {i32}
                    };
                    fields.push(quote! {
                        #[doc = #field_name_str]
                        pub #name: #ty,
                    });
                }
            }
            instructions.push(quote! {
                #[doc = #doc_string]
                #[derive(Copy, Clone, Debug)]
                pub struct #struct_name {
                    #(#fields)*
                }

                impl From<#struct_name> for Instruction {
                    fn from(v: #struct_name) -> Self {
                        Self::#struct_name(v)
                    }
                }
            });
            instruction_enumerants.push(quote! {
                #[doc = #doc_string]
                #struct_name(#struct_name),
            });
        }
        Ok(quote! {
            #(#instructions)*

            #[derive(Copy, Clone, Debug)]
            pub enum Instruction {
                #(#instruction_enumerants)*
            }
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
