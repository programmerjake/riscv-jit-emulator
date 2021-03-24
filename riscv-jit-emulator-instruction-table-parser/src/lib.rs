// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
pub mod ast;
pub mod tex;

pub use ast::parse;

macro_rules! parse_path {
    ($path:expr) => {
        ast::parse($path, include_str!($path))
            .map_err(|e| e.to_string())
            .unwrap()
    };
}

pub fn parse_rvc_instr_table() -> ast::InstructionSet {
    parse_path!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/riscv-isa-manual/src/rvc-instr-table.tex"
    ))
}

pub fn parse_instr_table() -> ast::InstructionSet {
    parse_path!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/riscv-isa-manual/src/instr-table.tex"
    ))
}

#[cfg(test)]
mod test {
    #[test]
    fn test_parse_instr_table() {
        crate::parse_instr_table();
    }

    #[test]
    fn test_parse_rvc_instr_table() {
        crate::parse_rvc_instr_table();
    }
}
