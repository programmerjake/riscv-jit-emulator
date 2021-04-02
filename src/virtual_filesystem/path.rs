// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use super::os_str::{OsStr, OsString};
use alloc::{borrow::Cow, boxed::Box, string::String};
use core::{
    fmt, mem,
    ops::{Deref, DerefMut},
};

pub enum Component<'a> {
    RootDir,
    CurDir,
    ParentDir,
    Normal(&'a OsStr),
}

pub struct Components<'a> {
    path: &'a OsStr,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Path(OsStr);

impl Path {
    pub fn new<T: AsRef<OsStr> + ?Sized>(v: &T) -> &Self {
        Self::from_os_str(v.as_ref())
    }
    pub fn from_os_str(v: &OsStr) -> &Path {
        // Safety: Path has same representation as OsStr
        unsafe { mem::transmute(v) }
    }
    pub fn from_os_str_mut(v: &mut OsStr) -> &mut Path {
        // Safety: Path has same representation as OsStr
        unsafe { mem::transmute(v) }
    }
    pub fn from_boxed_os_str(v: Box<OsStr>) -> Box<Path> {
        // Safety: Path has same representation as OsStr
        unsafe { mem::transmute(v) }
    }
    pub fn to_str(&self) -> Option<&str> {
        self.0.to_str()
    }
    pub fn to_string_lossy<'a>(&'a self) -> Cow<'a, str> {
        self.0.to_string_lossy()
    }
    pub fn to_path_buf(&self) -> PathBuf {
        PathBuf(self.0.to_os_string())
    }
    pub fn is_absolute(&self) -> bool {
        self.has_root()
    }
    pub fn is_relative(&self) -> bool {
        !self.is_absolute()
    }
    pub fn has_root(&self) -> bool {
        self.0.as_bytes().starts_with(b"/")
    }
    pub fn components<'a>(&'a self) -> Components<'a> {
        todo!()
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PathBuf(OsString);

impl Deref for PathBuf {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        Path::from_os_str(&self.0)
    }
}

impl DerefMut for PathBuf {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Path::from_os_str_mut(&mut self.0)
    }
}

impl AsRef<OsStr> for PathBuf {
    fn as_ref(&self) -> &OsStr {
        &self.0
    }
}

impl AsMut<OsStr> for PathBuf {
    fn as_mut(&mut self) -> &mut OsStr {
        &mut self.0
    }
}

impl AsRef<Path> for PathBuf {
    fn as_ref(&self) -> &Path {
        self
    }
}

impl AsMut<Path> for PathBuf {
    fn as_mut(&mut self) -> &mut Path {
        self
    }
}
