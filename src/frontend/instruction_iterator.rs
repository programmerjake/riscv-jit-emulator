// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use super::memory_map::MemoryMap;
use crate::decoder::Instruction;

#[derive(Debug, Clone)]
pub(crate) struct InstructionIterator<'a> {
    pub address: u64,
    pub memory_map: &'a MemoryMap<'a>,
}

impl Iterator for InstructionIterator<'_> {
    type Item = (u64, Instruction);

    fn next(&mut self) -> Option<Self::Item> {
        let mut bytes = [0; Instruction::MAX_BYTE_LENGTH];
        let read_len = self.memory_map.read_till_unmapped(self.address, &mut bytes);
        let instruction = Instruction::parse(&bytes[..read_len])?;
        let address = self.address;
        self.address += instruction.byte_length() as u64;
        Some((address, instruction))
    }
}
