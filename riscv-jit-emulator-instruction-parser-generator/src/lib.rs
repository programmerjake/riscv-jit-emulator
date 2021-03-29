use core::panic;
use std::{
    collections::{HashMap, HashSet},
    ops::RangeInclusive,
};

// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::quote;
use riscv_instructions::{ast, parse_instr_table, parse_rvc_instr_table};
use syn::parse::{Parse, ParseStream};

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

#[derive(Default)]
struct FieldDescriptor {
    bit_width: usize,
    exclusions: HashSet<u32>,
}

const BYTE_BIT_COUNT: usize = 8;

impl DecoderInput {
    fn to_tokens(&self) -> syn::Result<TokenStream> {
        let mut instructions = Vec::new();
        let mut instruction_enumerants = Vec::new();
        let mut instruction_parse_calls = Vec::new();
        let mut max_instruction_length = 0;
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
            let mut field_descriptors: HashMap<&str, FieldDescriptor> = HashMap::new();
            let mut fields = Vec::new();
            let mut total_instruction_bit_range = None;
            for field in &instruction.fields {
                total_instruction_bit_range = Some(total_instruction_bit_range.map_or(
                    field.instruction_bit_range.bits.clone(),
                    |total_instruction_bit_range: RangeInclusive<u8>| {
                        *field
                            .instruction_bit_range
                            .bits
                            .start()
                            .min(total_instruction_bit_range.start())
                            ..=*field
                                .instruction_bit_range
                                .bits
                                .end()
                                .max(total_instruction_bit_range.end())
                    },
                ));
                let needed_bit_width = match &field.field_def.body {
                    ast::FieldDefBody::Alternate(_) => field.instruction_bit_range.bits.len(),
                    ast::FieldDefBody::Slice(ast::FieldDefSlice { bit_ranges, .. }) => bit_ranges
                        .iter()
                        .map(|bit_range| usize::from(*bit_range.bits.end()) + 1)
                        .max()
                        .unwrap(),
                    ast::FieldDefBody::Wildcard(_) | ast::FieldDefBody::LiteralBinaryNumber(_) => 0,
                };
                for field_name in field.field_def.body.names() {
                    let field_name_str = get_field_name_str(field_name);
                    let field_descriptor = field_descriptors.entry(field_name_str).or_default();
                    field_descriptor.bit_width = needed_bit_width.max(field_descriptor.bit_width);
                    if field_name.zero_denied() {
                        field_descriptor.exclusions.insert(0);
                    }
                    for exclusion in &field.field_def.excluded_values {
                        field_descriptor
                            .exclusions
                            .insert(exclusion.parse().unwrap());
                    }
                }
            }
            let total_instruction_bit_range = total_instruction_bit_range.unwrap();
            assert_eq!(*total_instruction_bit_range.start(), 0);
            assert_eq!(total_instruction_bit_range.len() % BYTE_BIT_COUNT, 0);
            let instruction_length = total_instruction_bit_range.len() / BYTE_BIT_COUNT;
            max_instruction_length = max_instruction_length.max(instruction_length);
            let instruction_bits_type = match total_instruction_bit_range.len() {
                16 => quote! {u16},
                32 => quote! {u32},
                _ => panic!(
                    "unimplemented instruction bits type (u{})",
                    total_instruction_bit_range.len()
                ),
            };
            let mut parse_check_field_exclusions = Vec::new();
            let mut seen_field_names = HashSet::new();
            let mut parse_return_fields = Vec::new();
            let mut parse_instruction_fields = Vec::new();
            let mut parse_fields = Vec::new();
            let mut parse_declare_fields = Vec::new();
            for (field_index, field) in instruction.fields.iter().enumerate() {
                let field_index_lit = Literal::usize_unsuffixed(field_index);
                let instruction_field_shift =
                    Literal::u8_unsuffixed(*field.instruction_bit_range.bits.start());
                let instruction_field_mask_width =
                    Literal::usize_unsuffixed(field.instruction_bit_range.bits.len());
                parse_instruction_fields.push(quote! {
                    ((bits >> #instruction_field_shift) & ((1 << #instruction_field_mask_width) - 1))
                });
                for field_name in field.field_def.body.names() {
                    let field_name_str = get_field_name_str(field_name);
                    if !seen_field_names.insert(field_name_str) {
                        continue;
                    }
                    let field_descriptor = field_descriptors.get(field_name_str).unwrap();
                    let name = name_to_ident("", field_name_str, IdentKind::VarOrField);
                    let bit_width = field_descriptor.bit_width;
                    fields.push(quote! {
                        #[doc = #field_name_str]
                        pub #name: BitArray<#bit_width>,
                    });
                    for exclusion in &field_descriptor.exclusions {
                        let exclusion = Literal::u32_unsuffixed(*exclusion);
                        parse_check_field_exclusions.push(quote! {
                            if *#name.underlying() == #exclusion {
                                return None;
                            }
                        });
                    }
                    parse_return_fields.push(quote! {#name,});
                    parse_declare_fields.push(quote! {let mut #name = BitArray::new_unchecked(0);});
                }
                match &field.field_def.body {
                    ast::FieldDefBody::Alternate(a) => {
                        for name in &a.names {
                            let name =
                                name_to_ident("", get_field_name_str(name), IdentKind::VarOrField);
                            parse_fields.push(quote! {
                                #name = BitArray::new_unchecked(fields.#field_index_lit as _);
                            });
                        }
                    }
                    ast::FieldDefBody::Slice(s) => {
                        let mut bits_left = field.instruction_bit_range.bits.len();
                        for bit_range in &s.bit_ranges {
                            bits_left -= bit_range.bits.len();
                            let shift1 = Literal::usize_unsuffixed(bits_left);
                            let mask_width = Literal::usize_unsuffixed(bit_range.bits.len());
                            let shift2 = Literal::u8_unsuffixed(*bit_range.bits.start());
                            let name = name_to_ident(
                                "",
                                get_field_name_str(&s.name),
                                IdentKind::VarOrField,
                            );
                            parse_fields.push(quote! {
                                #name = BitArray::new_unchecked(
                                    (*#name.underlying() as u32 | (((fields.#field_index_lit >> #shift1) as u32 & ((1 << #mask_width) - 1)) << #shift2)) as _
                                );
                            });
                        }
                    }
                    ast::FieldDefBody::Wildcard(_) => {
                        parse_fields.push(quote! {
                            let _ = fields.#field_index_lit;
                        });
                    }
                    ast::FieldDefBody::LiteralBinaryNumber(num) => {
                        let num: TokenStream = ("0b".to_string() + &num.content).parse().unwrap();
                        parse_fields.push(quote! {
                            if fields.#field_index_lit != #num {
                                return None;
                            }
                        });
                    }
                }
            }
            let mut bytes = Vec::new();
            for i in 0..instruction_length {
                bytes.push(quote! {bytes[#i]});
            }
            instructions.push(quote! {
                #[doc = #doc_string]
                #[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
                pub struct #struct_name {
                    #(#fields)*
                }

                impl From<#struct_name> for Instruction {
                    fn from(v: #struct_name) -> Self {
                        Self::#struct_name(v)
                    }
                }

                impl #struct_name {
                    pub const BYTE_LENGTH: usize = #instruction_length;
                    pub const fn parse(bytes: &[u8]) -> Option<Self> {
                        #![allow(unused_assignments)]
                        if bytes.len() < Self::BYTE_LENGTH {
                            return None;
                        }
                        let bits = #instruction_bits_type::from_le_bytes([#(#bytes,)*]);
                        let fields = (#(#parse_instruction_fields),*);
                        #(#parse_declare_fields)*
                        #(#parse_fields)*
                        #(#parse_check_field_exclusions)*
                        Some(#struct_name {#(#parse_return_fields)*})
                    }
                }

                impl ParseInstruction for #struct_name {
                    const BYTE_LENGTH: usize = #struct_name::BYTE_LENGTH;
                    fn parse(bytes: &[u8]) -> Option<Self> {
                        #struct_name::parse(bytes)
                    }
                }
            });
            instruction_enumerants.push(quote! {
                #[doc = #doc_string]
                #struct_name(#struct_name),
            });
            instruction_parse_calls.push(quote! {
                if let Some(retval) = #struct_name::parse(bytes) {
                    return Some(Self::#struct_name(retval));
                }
            });
        }
        Ok(quote! {
            #(#instructions)*

            #[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
            pub enum Instruction {
                #(#instruction_enumerants)*
            }

            impl Instruction {
                /// max instruction length in bytes
                pub const MAX_BYTE_LENGTH: usize = #max_instruction_length;
                pub const fn parse(bytes: &[u8]) -> Option<Self> {
                    #(#instruction_parse_calls)*
                    None
                }
            }
        })
    }
}

pub fn gen_decoder() -> syn::Result<TokenStream> {
    let input = DecoderInput {};
    input.to_tokens()
}
