// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
};

pub unsafe trait BitArrayUnderlyingType {
    type Type: Copy + 'static + Send + Sync + Ord + Hash + Default;
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct BitArrayT<UnderlyingType, const SIZE: usize> {
    value: UnderlyingType,
}

pub type BitArray<const SIZE: usize> =
    BitArrayT<<[(); SIZE] as BitArrayUnderlyingType>::Type, SIZE>;

impl<UnderlyingType, const SIZE: usize> BitArrayT<UnderlyingType, SIZE> {
    pub const fn new_unchecked(value: UnderlyingType) -> Self {
        Self { value }
    }
    pub const fn underlying(&self) -> &UnderlyingType {
        &self.value
    }
    pub fn into_underlying(self) -> UnderlyingType {
        self.value
    }
}

macro_rules! impl_bit_array_for_underlying_type {
    ($underlying_type:ident) => {
        impl_bit_array_for_underlying_type! {
            $underlying_type,
            [
                (u8, from_u8, from_u8_wrapping),
                (u16, from_u16, from_u16_wrapping),
                (u32, from_u32, from_u32_wrapping),
                (u64, from_u64, from_u64_wrapping),
                (u128, from_u128, from_u128_wrapping),
                (usize, from_usize, from_usize_wrapping),
            ]
        }
    };
    (
        $underlying_type:ident,
        [
            $(($src_type:ident, $from_src_fn:ident, $from_src_wrapping_fn:ident),)*
        ]
    ) => {
        impl<const SIZE: usize> BitArrayT<$underlying_type, SIZE> {
            pub const MASK: $underlying_type = {
                let ones: $underlying_type = !0;
                if SIZE == ones.count_ones() as usize {
                    ones
                } else {
                    let in_range = SIZE < ones.count_ones() as usize;
                    [(); 1][(!in_range) as usize];
                    (1 << SIZE) - 1
                }
            };
            pub const fn new(value: $underlying_type) -> Option<Self> {
                if (value & Self::MASK) != value {
                    None
                } else {
                    Some(Self::new_unchecked(value))
                }
            }
            pub const fn new_wrapping(value: $underlying_type) -> Self {
                Self::new_unchecked(value & Self::MASK)
            }
            $(
                pub const fn $from_src_fn(value: $src_type) -> Option<Self> {
                    if (value & Self::MASK as $src_type) != value {
                        None
                    } else {
                        Some(Self::new_unchecked(value as $underlying_type))
                    }
                }
                pub const fn $from_src_wrapping_fn(value: $src_type) -> Self {
                    Self::new_wrapping(value as $underlying_type)
                }
            )*
        }

        impl<const SIZE: usize> fmt::Debug for BitArrayT<$underlying_type, SIZE> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "BitArray(0x{:X})", self.value)
            }
        }
    };
}

impl_bit_array_for_underlying_type!(u8);
impl_bit_array_for_underlying_type!(u16);
impl_bit_array_for_underlying_type!(u32);
impl_bit_array_for_underlying_type!(u64);
impl_bit_array_for_underlying_type!(u128);

impl<UnderlyingType: PartialEq, const SIZE: usize> PartialEq for BitArrayT<UnderlyingType, SIZE> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<UnderlyingType: Eq, const SIZE: usize> Eq for BitArrayT<UnderlyingType, SIZE> {}

impl<UnderlyingType: PartialOrd, const SIZE: usize> PartialOrd for BitArrayT<UnderlyingType, SIZE> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<UnderlyingType: Ord, const SIZE: usize> Ord for BitArrayT<UnderlyingType, SIZE> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value)
    }
}

impl<UnderlyingType: Hash, const SIZE: usize> Hash for BitArrayT<UnderlyingType, SIZE> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}

impl<UnderlyingType: Default, const SIZE: usize> Default for BitArrayT<UnderlyingType, SIZE> {
    fn default() -> Self {
        Self::new_unchecked(UnderlyingType::default())
    }
}

macro_rules! declare_bit_vec {
    ($underlying:ty, [$($width:literal,)*]) => {
        $(
            unsafe impl BitArrayUnderlyingType for [(); $width] {
                type Type = $underlying;
            }
        )*
    };
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
