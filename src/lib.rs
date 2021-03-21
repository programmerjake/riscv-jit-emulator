// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
use riscv_jit_emulator_proc_macro::gen_decoder;

gen_decoder!(
    instr_table = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/riscv-isa-manual/src/instr-table.tex"
    )
);
