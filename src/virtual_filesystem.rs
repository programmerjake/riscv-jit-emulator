// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

macro_rules! impl_str_partial_eq_ord {
    (PartialOrd<$rhs_ty:ty> for $self_ty:ty; ($self:ident, $other:ident) -> ($self_expr:expr, $rhs_expr:expr)) => {
        impl PartialEq<$rhs_ty> for $self_ty {
            fn eq(&$self, $other: &$rhs_ty) -> bool {
                let lhs: &OsStr = $self_expr;
                let rhs: &OsStr = $rhs_expr;
                lhs == rhs
            }
        }

        impl PartialOrd<$rhs_ty> for $self_ty {
            fn partial_cmp(&$self, $other: &$rhs_ty) -> Option<core::cmp::Ordering> {
                let lhs: &OsStr = $self_expr;
                let rhs: &OsStr = $rhs_expr;
                Some(lhs.cmp(rhs))
            }
        }
    };
}

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
