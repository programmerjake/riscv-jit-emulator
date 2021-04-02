// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use alloc::{
    borrow::{Cow, ToOwned},
    boxed::Box,
    string::String,
    vec::Vec,
};
use core::{
    borrow::{Borrow, BorrowMut},
    cmp::Ordering,
    fmt, iter, mem,
    ops::{Deref, DerefMut},
    slice, str,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct OsStr([u8]);

impl OsStr {
    pub fn new<T: AsRef<OsStr> + ?Sized>(value: &T) -> &OsStr {
        value.as_ref()
    }
    pub fn to_str(&self) -> Option<&str> {
        str::from_utf8(self.as_bytes()).ok()
    }
    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.as_bytes())
    }
    pub fn to_os_string(&self) -> OsString {
        OsString(self.as_bytes().to_vec())
    }
    pub fn into_os_string(self: Box<OsStr>) -> OsString {
        OsString(self.into_bytes().into())
    }
    pub const fn is_empty(&self) -> bool {
        self.as_bytes().is_empty()
    }
    pub const fn len(&self) -> usize {
        self.as_bytes().len()
    }
    pub fn from_bytes(value: &[u8]) -> &OsStr {
        unsafe { mem::transmute(value) }
    }
    pub fn from_bytes_mut(value: &mut [u8]) -> &mut OsStr {
        unsafe { mem::transmute(value) }
    }
    pub const fn as_bytes(&self) -> &[u8] {
        &self.0
    }
    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }
    pub fn into_bytes(self: Box<OsStr>) -> Box<[u8]> {
        unsafe { Box::from_raw(Box::into_raw(self) as *mut [u8]) }
    }
    pub fn from_boxed_bytes(bytes: Box<[u8]>) -> Box<OsStr> {
        unsafe { Box::from_raw(Box::into_raw(bytes) as *mut OsStr) }
    }
    pub fn bytes(&self) -> iter::Copied<slice::Iter<'_, u8>> {
        self.as_bytes().iter().copied()
    }
}

impl AsRef<OsStr> for OsStr {
    fn as_ref(&self) -> &OsStr {
        self
    }
}

impl AsRef<OsStr> for str {
    fn as_ref(&self) -> &OsStr {
        OsStr::from_bytes(self.as_bytes())
    }
}

impl AsRef<OsStr> for String {
    fn as_ref(&self) -> &OsStr {
        OsStr::from_bytes(self.as_bytes())
    }
}

impl fmt::Debug for OsStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buf = &self.0;
        write!(f, "\"")?;
        loop {
            let valid_up_to = str::from_utf8(buf)
                .err()
                .map_or(buf.len(), |e| e.valid_up_to());
            let (valid, rest) = buf.split_at(valid_up_to);
            for ch in str::from_utf8(valid).unwrap().chars() {
                write!(f, "{}", ch.escape_debug())?;
            }
            if let Some((&byte, rest)) = rest.split_first() {
                write!(f, "\\x{:02X}", byte)?;
                buf = rest;
            } else {
                break;
            }
        }
        write!(f, "\"")
    }
}

impl Default for &'_ OsStr {
    fn default() -> Self {
        OsStr::from_bytes(&[])
    }
}

impl From<&'_ OsStr> for Box<OsStr> {
    fn from(v: &'_ OsStr) -> Self {
        OsStr::from_boxed_bytes(v.as_bytes().into())
    }
}

impl<'a> From<&'a OsStr> for Cow<'a, OsStr> {
    fn from(v: &'a OsStr) -> Self {
        Cow::Borrowed(v)
    }
}

impl PartialEq<&'_ OsStr> for OsString {
    fn eq(&self, other: &&'_ OsStr) -> bool {
        **self == **other
    }
}

impl PartialEq<&'_ OsStr> for Cow<'_, OsStr> {
    fn eq(&self, other: &&'_ OsStr) -> bool {
        **self == **other
    }
}

impl PartialEq<Cow<'_, OsStr>> for OsStr {
    fn eq(&self, other: &Cow<'_, OsStr>) -> bool {
        *self == **other
    }
}

impl PartialEq<Cow<'_, OsStr>> for &'_ OsStr {
    fn eq(&self, other: &Cow<'_, OsStr>) -> bool {
        **self == **other
    }
}

impl PartialEq<OsStr> for str {
    fn eq(&self, other: &OsStr) -> bool {
        OsStr::from_bytes(self.as_bytes()) == other
    }
}

impl PartialEq<OsStr> for OsString {
    fn eq(&self, other: &OsStr) -> bool {
        **self == *other
    }
}

impl PartialEq<OsStr> for Cow<'_, OsStr> {
    fn eq(&self, other: &OsStr) -> bool {
        **self == *other
    }
}

impl PartialEq<OsString> for OsStr {
    fn eq(&self, other: &OsString) -> bool {
        *self == **other
    }
}

impl PartialEq<OsString> for &'_ OsStr {
    fn eq(&self, other: &OsString) -> bool {
        **self == **other
    }
}

impl PartialEq<str> for OsStr {
    fn eq(&self, other: &str) -> bool {
        self == OsStr::from_bytes(other.as_bytes())
    }
}

impl PartialOrd<&'_ OsStr> for OsString {
    fn partial_cmp(&self, other: &&'_ OsStr) -> Option<Ordering> {
        todo!()
    }
}

impl PartialOrd<&'_ OsStr> for Cow<'_, OsStr> {
    fn partial_cmp(&self, other: &&'_ OsStr) -> Option<Ordering> {
        (**self).partial_cmp(*other)
    }
}

impl PartialOrd<Cow<'_, OsStr>> for OsStr {
    fn partial_cmp(&self, other: &Cow<'_, OsStr>) -> Option<Ordering> {
        (*self).partial_cmp(&**other)
    }
}

impl PartialOrd<Cow<'_, OsStr>> for &'_ OsStr {
    fn partial_cmp(&self, other: &Cow<'_, OsStr>) -> Option<Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl PartialOrd<OsStr> for str {
    fn partial_cmp(&self, other: &OsStr) -> Option<Ordering> {
        OsStr::from_bytes(self.as_bytes()).partial_cmp(other)
    }
}

impl PartialOrd<OsStr> for OsString {
    fn partial_cmp(&self, other: &OsStr) -> Option<Ordering> {
        (**self).partial_cmp(other)
    }
}

impl PartialOrd<OsStr> for Cow<'_, OsStr> {
    fn partial_cmp(&self, other: &OsStr) -> Option<Ordering> {
        (**self).partial_cmp(other)
    }
}

impl PartialOrd<OsString> for OsStr {
    fn partial_cmp(&self, other: &OsString) -> Option<Ordering> {
        (*self).partial_cmp(&**other)
    }
}

impl PartialOrd<OsString> for &'_ OsStr {
    fn partial_cmp(&self, other: &OsString) -> Option<Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl PartialOrd<str> for OsStr {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        (*self).partial_cmp(OsStr::from_bytes(other.as_bytes()))
    }
}

impl ToOwned for OsStr {
    type Owned = OsString;

    fn to_owned(&self) -> Self::Owned {
        self.to_os_string()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OsString(Vec<u8>);

impl AsRef<OsStr> for OsString {
    fn as_ref(&self) -> &OsStr {
        self
    }
}

impl Deref for OsString {
    type Target = OsStr;

    fn deref(&self) -> &Self::Target {
        OsStr::from_bytes(&self.0)
    }
}

impl DerefMut for OsString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        OsStr::from_bytes_mut(&mut self.0)
    }
}

impl Borrow<OsStr> for OsString {
    fn borrow(&self) -> &OsStr {
        self
    }
}

impl BorrowMut<OsStr> for OsString {
    fn borrow_mut(&mut self) -> &mut OsStr {
        self
    }
}
