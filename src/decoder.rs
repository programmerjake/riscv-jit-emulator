// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::bit_array::BitArray;

pub trait ParseInstruction: Copy {
    /// instruction length in bytes
    const BYTE_LENGTH: usize;
    fn parse(bytes: &[u8]) -> Option<Self>;
}

include!(concat!(env!("OUT_DIR"), "/decoder.rs"));

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_zero() {
        assert_eq!(Instruction::parse(&[0, 0, 0, 0]), None);
        assert_eq!(Instruction::parse(&[0, 0]), None);
        assert_eq!(Instruction::parse(&[0, 0, 0x12, 0x34]), None);
    }

    #[test]
    fn parse_ones() {
        assert_eq!(Instruction::parse(&[0xFF, 0xFF, 0xFF, 0xFF]), None);
    }

    #[test]
    fn parse_c_jr() {
        assert_eq!(
            Instruction::parse(&[0x82, 0x80]),
            Some(
                CJr {
                    rs1: BitArray::new_unchecked(1),
                }
                .into()
            )
        );
    }

    #[test]
    fn parse_jal() {
        assert_eq!(
            Instruction::parse(&[0xEF, 0x0, 0x0, 0x1]),
            Some(
                Jal {
                    rd: BitArray::new_unchecked(1),
                    imm: BitArray::new_unchecked(0x10)
                }
                .into()
            )
        );
    }

    #[test]
    fn parse_c_j() {
        assert_eq!(
            Instruction::parse(&[0x45, 0xAB]),
            Some(
                CJ {
                    imm: BitArray::new_unchecked(0x5B0)
                }
                .into()
            )
        );
        assert_eq!(
            Instruction::parse(&[0xCD, 0xB4]),
            Some(
                CJ {
                    imm: BitArray::new_unchecked(0xAE2)
                }
                .into()
            )
        );
        assert_eq!(
            Instruction::parse(&[0x31, 0xB9]),
            Some(
                CJ {
                    imm: BitArray::new_unchecked(0xC1C)
                }
                .into()
            )
        );
        assert_eq!(
            Instruction::parse(&[0x1, 0xA0]),
            Some(
                CJ {
                    imm: BitArray::new_unchecked(0)
                }
                .into()
            )
        );
        assert_eq!(
            Instruction::parse(&[0xFD, 0xBF]),
            Some(
                CJ {
                    imm: BitArray::new_unchecked(0xFFE)
                }
                .into()
            )
        );
    }
}
