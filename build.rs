// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use riscv_jit_emulator_instruction_parser_generator::gen_decoder;
use std::{
    env, fs,
    io::{self, Read, Write},
    path::Path,
    process::{Child, Command, Stdio},
    thread,
};

fn format_source(source: String) -> String {
    let rustfmt_path = which::which("rustfmt").unwrap();
    let mut command = Command::new(rustfmt_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    let stdin = command.stdin.take().unwrap();
    let reader_thread = thread::spawn(move || -> io::Result<(String, Child)> {
        let mut output = String::new();
        command.stdout.take().unwrap().read_to_string(&mut output)?;
        Ok((output, command))
    });
    { stdin }.write_all(source.as_bytes()).unwrap();
    let (output, mut command) = reader_thread.join().unwrap().unwrap();
    let exit_status = command.wait().unwrap();
    assert!(exit_status.success());
    output
}

fn main() {
    fs::write(
        Path::new(&env::var_os("OUT_DIR").unwrap()).join("decoder.rs"),
        format_source(gen_decoder().unwrap().to_string()),
    )
    .unwrap();
}
