// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use alloc::{collections::BTreeSet, format, string::String, vec::Vec};
use core::fmt;
use goblin::elf::{header, program_header, Elf, Reloc};
use std::dbg;

#[derive(Debug)]
pub enum ParseError {
    ElfError(goblin::error::Error),
    Error(String),
}

macro_rules! make_error {
    ($($msg:tt)*) => {
        ParseError::Error(format!($($msg)*))
    };
}

macro_rules! return_error {
    ($($msg:tt)*) => {
        return Err(make_error!($($msg)*))
    };
}

macro_rules! match_or_error {
    ($opt:expr, $($pat:pat)|+ $(if $cond:expr)? => $result:expr, $($msg:tt)*) => {
        match $opt {
            $($pat)|+ $(if $cond)? => $result,
            _ => return_error!($($msg)*),
        }
    };
}

macro_rules! unwrap_or_error {
    ($opt:expr, $($msg:tt)*) => {
        match_or_error!($opt, Some(v) => v, $($msg)*)
    };
}

impl From<goblin::error::Error> for ParseError {
    fn from(e: goblin::error::Error) -> Self {
        Self::ElfError(e)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ElfError(e) => e.fmt(f),
            Self::Error(e) => e.fmt(f),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Copy, Clone)]
struct Relocation(Reloc);

struct LoadedPage<'a> {
    relocations: BTreeSet<Relocation>,
    data: &'a [u8],
}

enum PageData<'a> {
    Loaded(LoadedPage<'a>),
    Zeroed,
}

struct Page<'a> {
    data: PageData<'a>,
}

pub struct Binary<'a> {
    memory_map: Vec<Option<Page<'a>>>,
}

impl<'a> Binary<'a> {
    pub fn parse(bytes: &'a [u8]) -> ParseResult<Self> {
        let elf = Elf::parse(bytes)?;
        match_or_error!((elf.is_64,elf.little_endian), (true, true) => (), "unsupported ELF type: expected 64-bit little-endian ELF");
        match_or_error!(elf.header.e_type, header::ET_EXEC => (), "unsupported ELF type: {:?}", header::et_to_str(elf.header.e_type));
        match_or_error!(elf.header.e_machine, header::EM_RISCV => (), "unsupported ELF machine: 0x{:x}", elf.header.e_machine);
        let interpreter = unwrap_or_error!(elf.interpreter, "PT_INTERP program header not found");
        match_or_error!(interpreter, "/lib/ld-linux-riscv64-lp64d.so.1" => (), "unsupported ELF interpreter");
        let dynamic = unwrap_or_error!(&elf.dynamic, "missing ELF dynamic section");
        for program_header in &elf.program_headers {
            if program_header.p_type != program_header::PT_LOAD {
                continue;
            }
            dbg!(program_header);
        }
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test1() {
        Binary::parse(include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/test_binaries/test1"
        )))
        .unwrap();
    }
}
