// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

pub mod os_str;
pub mod path;

use alloc::{borrow::Cow, string::String, vec::Vec};
use core::{
    borrow::BorrowMut,
    fmt, mem,
    ops::{Deref, DerefMut},
    str,
};

pub trait VirtualFilesystem {}
