// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use riscv_jit_emulator_instruction_table_parser::{ast, Parser, SourceCode};
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
    let source_code = SourceCode::new(&file_path_string, &input);
    let document = Parser::new(&source_code)
        .parse::<ast::Document>()
        .map_err(|e| syn::Error::new_spanned(file_path, e))?;
    dbg!(document);
    todo!()
}
