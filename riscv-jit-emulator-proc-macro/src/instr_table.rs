// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use riscv_jit_emulator_instruction_table_parser::parse;
use std::fs::read_to_string;
use syn::{self, LitStr};

pub(crate) fn parse_file(file_path: &LitStr) -> syn::Result<()> {
    let file_path_string = file_path.value();
    let input = read_to_string(&file_path_string).map_err(|e| {
        syn::Error::new_spanned(
            file_path,
            format!("failed to read file {:?}: {}", &file_path_string, e),
        )
    })?;
    let document =
        parse(&file_path_string, &input).map_err(|e| syn::Error::new_spanned(file_path, e))?;
    dbg!(document);
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;
    use proc_macro2::Span;

    #[test]
    fn test_parse_instr_table() {
        parse_file(&LitStr::new(
            concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../riscv-isa-manual/src/instr-table.tex"
            ),
            Span::call_site(),
        ))
        .unwrap();
    }
}
