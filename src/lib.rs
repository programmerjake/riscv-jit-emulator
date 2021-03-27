// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
use std::{cmp::Ordering, fmt, hash::Hash};

pub unsafe trait BitVecUnderlyingType {
    type Type: Copy + 'static + Send + Sync + Ord + Hash + Default;
    fn fmt_debug(v: &Self::Type, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

#[repr(transparent)]
pub struct BitVecT<T: ?Sized + BitVecUnderlyingType>(pub T::Type);

pub type BitVec<const SIZE: usize> = BitVecT<[(); SIZE]>;

impl<T: ?Sized + BitVecUnderlyingType> BitVecT<T> {
    pub fn new(v: T::Type) -> Self {
        Self(v)
    }
}

impl<T: ?Sized + BitVecUnderlyingType> Copy for BitVecT<T> {}

impl<T: ?Sized + BitVecUnderlyingType> Clone for BitVecT<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized + BitVecUnderlyingType> PartialEq for BitVecT<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: ?Sized + BitVecUnderlyingType> Eq for BitVecT<T> {}

impl<T: ?Sized + BitVecUnderlyingType> PartialOrd for BitVecT<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T: ?Sized + BitVecUnderlyingType> Ord for BitVecT<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T: ?Sized + BitVecUnderlyingType> Hash for BitVecT<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T: ?Sized + BitVecUnderlyingType> Default for BitVecT<T> {
    fn default() -> Self {
        Self(T::Type::default())
    }
}

impl<T: ?Sized + BitVecUnderlyingType> fmt::Debug for BitVecT<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        T::fmt_debug(&self.0, f)
    }
}

macro_rules! declare_bit_vec {
    ($underlying:ty, [$($width:literal,)*]) => {
        $(
            unsafe impl BitVecUnderlyingType for [(); $width] {
                type Type = $underlying;
                fn fmt_debug(v: &Self::Type, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "BitVec(0x{:X})", v)
                }
            }
        )*
    };
}

unsafe impl BitVecUnderlyingType for [(); 0] {
    type Type = ();

    fn fmt_debug(_v: &Self::Type, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BitVec(0)")
    }
}

declare_bit_vec! {u8, [1, 2, 3, 4, 5, 6, 7, 8, ]}
declare_bit_vec! {u16, [9, 10, 11, 12, 13, 14, 15, 16, ]}
declare_bit_vec! {u32, [
    17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
]}
declare_bit_vec! {u64, [
    33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,
    49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
]}
declare_bit_vec! {u128, [
    65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
    81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96,
    97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
    113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
]}

pub trait ParseInstruction: Copy {
    /// instruction length in bytes
    const LENGTH: usize;
    fn parse(bytes: [u8; Instruction::MAX_LENGTH]) -> Option<Self>;
}

include!(concat!(env!("OUT_DIR"), "/decoder.rs"));

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_zero() {
        assert_eq!(Instruction::parse([0, 0, 0, 0]), None);
        assert_eq!(Instruction::parse([0, 0, 0x12, 0x34]), None);
    }

    #[test]
    fn parse_ones() {
        assert_eq!(Instruction::parse([0xFF, 0xFF, 0xFF, 0xFF]), None);
    }

    #[test]
    fn parse_c_jr() {
        assert_eq!(
            Instruction::parse([0x82, 0x80, 0, 0]),
            Some(
                CJr {
                    rs1: BitVec::new(1),
                }
                .into()
            )
        );
    }

    #[test]
    fn parse_jal() {
        assert_eq!(
            Instruction::parse([0xEF, 0x0, 0x0, 0x1]),
            Some(
                Jal {
                    rd: BitVec::new(1),
                    imm: BitVec::new(0x10)
                }
                .into()
            )
        );
    }

    #[test]
    fn parse_c_j() {
        assert_eq!(
            Instruction::parse([0x45, 0xAB, 0, 0]),
            Some(
                CJ {
                    imm: BitVec::new(0x5B0)
                }
                .into()
            )
        );
        assert_eq!(
            Instruction::parse([0xCD, 0xB4, 0, 0]),
            Some(
                CJ {
                    imm: BitVec::new(0xAE2)
                }
                .into()
            )
        );
        assert_eq!(
            Instruction::parse([0x31, 0xB9, 0, 0]),
            Some(
                CJ {
                    imm: BitVec::new(0xC1C)
                }
                .into()
            )
        );
        assert_eq!(
            Instruction::parse([0x1, 0xA0, 0, 0]),
            Some(
                CJ {
                    imm: BitVec::new(0)
                }
                .into()
            )
        );
        assert_eq!(
            Instruction::parse([0xFD, 0xBF, 0, 0]),
            Some(
                CJ {
                    imm: BitVec::new(0xFFE)
                }
                .into()
            )
        );
    }
}
