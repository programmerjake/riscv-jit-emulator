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

pub trait AsFile {
    fn as_file(&self) -> &dyn File;
}

impl<T: File> AsFile for T {
    fn as_file(&self) -> &dyn File {
        self
    }
}

pub trait File: Send + Sync + fmt::Debug + 'static + AsFile {}

pub trait AsReadableFile: AsFile {
    fn as_rd_file(&self) -> &dyn ReadableFile;
}

impl<T: ReadableFile> AsReadableFile for T {
    fn as_rd_file(&self) -> &dyn ReadableFile {
        self
    }
}

pub trait ReadableFile: File + AsReadableFile {
    fn read_all(&self) -> Result<Vec<u8>, FSError>;
    // TODO: finish
}

pub trait AsWritableFile: AsFile {
    fn as_wr_file(&self) -> &dyn WritableFile;
}

impl<T: WritableFile> AsWritableFile for T {
    fn as_wr_file(&self) -> &dyn WritableFile {
        self
    }
}

pub trait WritableFile: File + AsWritableFile {
    // TODO: finish
}

pub trait AsRWFile: AsReadableFile + AsWritableFile {
    fn as_rw_file(&self) -> &dyn RWFile;
}

impl<T: RWFile> AsRWFile for T {
    fn as_rw_file(&self) -> &dyn RWFile {
        self
    }
}

pub trait RWFile: ReadableFile + WritableFile + AsRWFile {}

impl<T: ?Sized + ReadableFile + WritableFile + AsRWFile> RWFile for T {}

pub trait SymbolicLink: Send + Sync + fmt::Debug + 'static {
    fn target(&self) -> &Path;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum EntryType {
    Normal,
    Directory,
    SymbolicLink,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct FileOpenFlags {
    _private: (),
}

pub trait DirectoryEntry<'a>: fmt::Debug + 'a {
    fn clone(&self) -> Box<dyn DirectoryEntry<'static>>;
    fn name(&self) -> &OsStr;
    fn entry_type(&self) -> EntryType;
    fn unlink(&self) -> Result<(), FSError>;
    fn open_file_no_access(&self, flags: FileOpenFlags) -> Result<Box<dyn File>, FSError>;
    fn open_file_readable(&self, flags: FileOpenFlags) -> Result<Box<dyn ReadableFile>, FSError>;
    fn open_file_writable(&self, flags: FileOpenFlags) -> Result<Box<dyn WritableFile>, FSError>;
    fn open_file_rw(&self, flags: FileOpenFlags) -> Result<Box<dyn RWFile>, FSError>;
}

pub trait DirectoryEntries: Send + Sync + fmt::Debug {
    /// get the next directory entries, reading not more than `count` entries. On end-of-directory, return `Ok(&[])`.
    fn next<'a>(&'a mut self, count: usize) -> Result<&'a [&'a dyn DirectoryEntry<'a>], FSError>;
}

pub trait Directory: Send + Sync + fmt::Debug {
    fn create_hard_link(
        &self,
        name: &OsStr,
        file: &dyn File,
    ) -> Result<Box<dyn DirectoryEntry<'static>>, FSError>;
    fn get(&self, name: &OsStr) -> Result<Option<Box<dyn DirectoryEntry<'static>>>, FSError>;
    fn entries(&self) -> Box<dyn DirectoryEntries>;
}

pub trait Filesystem: Send + Sync + fmt::Debug + FilesystemExt {
    fn root(&self) -> &Arc<dyn Directory>;
}

pub trait FilesystemExt {}

impl<T: ?Sized + Filesystem> FilesystemExt for T {}
