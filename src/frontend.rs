// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use alloc::{boxed::Box, collections::BTreeSet, format, string::String, vec::Vec};
use core::{
    cmp,
    convert::{TryFrom, TryInto},
    fmt,
    ops::{Deref, Range},
};
use goblin::elf::{header, program_header, Elf, Reloc};
use instruction_iterator::InstructionIterator;
use memory_map::MemoryMap;
use std::{dbg, println};

#[derive(Debug)]
pub enum ParseError {
    ElfError(goblin::error::Error),
    Error(String),
}

macro_rules! make_error {
    ($($msg:tt)*) => {
        ParseError::Error(alloc::format!($($msg)*))
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

mod instruction_iterator;
mod memory_map;

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

pub struct Binary<'a> {
    memory_map: MemoryMap<'a>,
}

impl<'a> Binary<'a> {
    fn get_program_header_virtual_address_range(
        program_header: &program_header::ProgramHeader,
    ) -> Option<Range<u64>> {
        Some(program_header.p_vaddr..program_header.p_vaddr.checked_add(program_header.p_memsz)?)
    }
    fn get_program_header_file_range(
        program_header: &program_header::ProgramHeader,
    ) -> Option<Range<usize>> {
        let start = program_header.p_offset.try_into().ok()?;
        let len = program_header.p_filesz.try_into().ok()?;
        Some(start..start.checked_add(len)?)
    }
    pub fn parse(bytes: &'a [u8]) -> ParseResult<Self> {
        let elf = Elf::parse(bytes)?;
        match_or_error!((elf.is_64,elf.little_endian), (true, true) => (), "unsupported ELF type: expected 64-bit little-endian ELF");
        match_or_error!(elf.header.e_type, header::ET_EXEC => (), "unsupported ELF type: {:?}", header::et_to_str(elf.header.e_type));
        match_or_error!(elf.header.e_machine, header::EM_RISCV => (), "unsupported ELF machine: 0x{:x}", elf.header.e_machine);
        let interpreter = unwrap_or_error!(elf.interpreter, "PT_INTERP program header not found");
        match_or_error!(interpreter, "/lib/ld-linux-riscv64-lp64d.so.1" => (), "unsupported ELF interpreter");
        let dynamic = unwrap_or_error!(&elf.dynamic, "missing ELF dynamic section");
        let mut memory_map = MemoryMap::new();
        for program_header in &elf.program_headers {
            if program_header.p_type != program_header::PT_LOAD {
                continue;
            }
            let address_range = unwrap_or_error!(
                Self::get_program_header_virtual_address_range(program_header),
                "invalid PT_LOAD program header virtual address range"
            );
            let bytes = unwrap_or_error!(
                Self::get_program_header_file_range(program_header).and_then(|v| bytes.get(v)),
                "invalid PT_LOAD program header file byte range"
            );
            memory_map.map_range_to_bytes(address_range, bytes)?;
        }
        dbg!(&memory_map);
        for (address, instruction) in (InstructionIterator {
            address: elf.header.e_entry,
            memory_map: &memory_map,
        }) {
            println!("0x{:04X}: {:?}", address, instruction);
        }
        dbg!(&elf.dynsyms);
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
