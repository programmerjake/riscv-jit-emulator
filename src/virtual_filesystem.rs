use core::fmt;

use self::{os_str::OsStr, path::Path};
use alloc::{boxed::Box, collections::BTreeSet, sync::Arc, vec::Vec};

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

#[derive(Debug)]
enum FSErrorBody {
    // TODO: finish
}

#[derive(Debug)]
pub struct FSError {
    body: FSErrorBody,
}

pub trait File: Send + Sync + fmt::Debug + 'static {
    fn read_all(&self) -> Result<Vec<u8>, FSError>;
}

pub trait SymbolicLink: Send + Sync + fmt::Debug + 'static {
    fn target(&self) -> &Path;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum EntryType {
    Normal,
    Directory,
    SymbolicLink,
}

#[derive(Clone, Debug)]
pub enum OpenedEntry {
    Normal(Arc<dyn File>),
    Directory(Arc<dyn Directory>),
    SymbolicLink(Arc<dyn SymbolicLink>),
}

pub trait DirectoryEntry<'a>: fmt::Debug + 'a {
    fn name(&self) -> &OsStr;
    fn entry_type(&self) -> EntryType;
    fn open(&self) -> Result<OpenedEntry, FSError>;
}

pub trait DirectoryEntries: Send + Sync + fmt::Debug {
    /// get the next directory entries, reading not more than `count` entries. On end-of-directory, return `Ok(&[])`.
    fn next<'a>(&'a mut self, count: usize) -> Result<&'a [&'a dyn DirectoryEntry<'a>], FSError>;
}

pub trait Directory: Send + Sync + fmt::Debug {
    fn get(&self, name: &OsStr) -> Result<Option<Box<dyn DirectoryEntry<'static>>>, FSError>;
    fn entries(&self) -> Box<dyn DirectoryEntries>;
}

pub trait Filesystem: Send + Sync + fmt::Debug {
    fn root(&self) -> &Arc<dyn Directory>;
}
