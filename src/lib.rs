// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]

extern crate alloc;
#[cfg(any(test, feature = "std"))]
extern crate std;

pub mod backend;
pub mod bit_array;
pub mod decoder;
pub mod frontend;
pub mod virtual_filesystem;
