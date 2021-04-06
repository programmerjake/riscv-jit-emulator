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
    convert::Infallible,
    fmt, iter, mem,
    ops::{Deref, DerefMut, Index, IndexMut},
    slice::{self, SliceIndex},
    str::{self, FromStr},
};

mod sealed {
    pub trait SealedPattern {}
}

pub trait Pattern: sealed::SealedPattern + Sized {
    fn find(haystack: &[u8], needle: Self) -> Option<usize>;
    fn rfind(haystack: &[u8], needle: Self) -> Option<usize>;
    fn strip_prefix(haystack: &[u8], needle: Self) -> Option<&[u8]>;
    fn strip_suffix(haystack: &[u8], needle: Self) -> Option<&[u8]>;
}

impl<T: FnMut(u8) -> bool> sealed::SealedPattern for T {}

impl<T: FnMut(u8) -> bool> Pattern for T {
    fn find(haystack: &[u8], needle: Self) -> Option<usize> {
        haystack.iter().copied().position(needle)
    }
    fn rfind(haystack: &[u8], needle: Self) -> Option<usize> {
        haystack.iter().copied().rposition(needle)
    }
    fn strip_prefix(haystack: &[u8], mut needle: Self) -> Option<&[u8]> {
        match haystack.split_first() {
            Some((&first, retval)) if needle(first) => Some(retval),
            _ => None,
        }
    }
    fn strip_suffix(haystack: &[u8], mut needle: Self) -> Option<&[u8]> {
        match haystack.split_last() {
            Some((&first, retval)) if needle(first) => Some(retval),
            _ => None,
        }
    }
}

impl sealed::SealedPattern for u8 {}

impl Pattern for u8 {
    fn find(haystack: &[u8], needle: Self) -> Option<usize> {
        Pattern::find(haystack, |v| v == needle)
    }

    fn rfind(haystack: &[u8], needle: Self) -> Option<usize> {
        Pattern::rfind(haystack, |v| v == needle)
    }

    fn strip_prefix(haystack: &[u8], needle: Self) -> Option<&[u8]> {
        Pattern::strip_prefix(haystack, |v| v == needle)
    }

    fn strip_suffix(haystack: &[u8], needle: Self) -> Option<&[u8]> {
        Pattern::strip_suffix(haystack, |v| v == needle)
    }
}

impl sealed::SealedPattern for &'_ OsStr {}

impl Pattern for &'_ OsStr {
    fn find(haystack: &[u8], needle: Self) -> Option<usize> {
        twoway::find_bytes(haystack, &needle.0)
    }

    fn rfind(haystack: &[u8], needle: Self) -> Option<usize> {
        twoway::rfind_bytes(haystack, &needle.0)
    }

    fn strip_prefix(haystack: &[u8], needle: Self) -> Option<&[u8]> {
        haystack.strip_prefix(&needle.0)
    }

    fn strip_suffix(haystack: &[u8], needle: Self) -> Option<&[u8]> {
        haystack.strip_suffix(&needle.0)
    }
}

macro_rules! impl_pattern_for_string_like {
    ($ty:ty) => {
        impl sealed::SealedPattern for $ty {}

        impl Pattern for $ty {
            fn find(haystack: &[u8], needle: Self) -> Option<usize> {
                let needle: &OsStr = needle.as_ref();
                Pattern::find(haystack, needle)
            }
            fn rfind(haystack: &[u8], needle: Self) -> Option<usize> {
                let needle: &OsStr = needle.as_ref();
                Pattern::rfind(haystack, needle)
            }
            fn strip_prefix(haystack: &[u8], needle: Self) -> Option<&[u8]> {
                let needle: &OsStr = needle.as_ref();
                Pattern::strip_prefix(haystack, needle)
            }
            fn strip_suffix(haystack: &[u8], needle: Self) -> Option<&[u8]> {
                let needle: &OsStr = needle.as_ref();
                Pattern::strip_suffix(haystack, needle)
            }
        }
    };
}

// we don't implement `Pattern` for `[u8]` since
// that confuses between matching a substring and matching a character set
impl_pattern_for_string_like!(&'_ mut OsStr);
impl_pattern_for_string_like!(&'_ str);
impl_pattern_for_string_like!(&'_ mut str);
impl_pattern_for_string_like!(Box<OsStr>);
impl_pattern_for_string_like!(String);
impl_pattern_for_string_like!(OsString);
impl_pattern_for_string_like!(&'_ OsString);

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
        OsString(self.into_boxed_bytes().into())
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
    pub fn into_boxed_bytes(self: Box<OsStr>) -> Box<[u8]> {
        unsafe { Box::from_raw(Box::into_raw(self) as *mut [u8]) }
    }
    pub fn from_boxed_bytes(bytes: Box<[u8]>) -> Box<OsStr> {
        unsafe { Box::from_raw(Box::into_raw(bytes) as *mut OsStr) }
    }
    pub fn bytes(&self) -> iter::Copied<slice::Iter<'_, u8>> {
        self.as_bytes().iter().copied()
    }
    pub fn starts_with<T: Pattern>(&self, needle: T) -> bool {
        self.strip_prefix(needle).is_some()
    }
    pub fn ends_with<T: Pattern>(&self, needle: T) -> bool {
        self.strip_suffix(needle).is_some()
    }
    pub fn get<I: SliceIndex<[u8], Output = [u8]>>(&self, index: I) -> Option<&Self> {
        self.0.get(index).map(Self::from_bytes)
    }
    pub fn get_mut<I: SliceIndex<[u8], Output = [u8]>>(&mut self, index: I) -> Option<&mut Self> {
        self.0.get_mut(index).map(Self::from_bytes_mut)
    }
    pub fn split_at(&self, index: usize) -> (&Self, &Self) {
        let (a, b) = self.0.split_at(index);
        (Self::from_bytes(a), Self::from_bytes(b))
    }
    pub fn split_at_mut(&mut self, index: usize) -> (&mut Self, &mut Self) {
        let (a, b) = self.0.split_at_mut(index);
        (Self::from_bytes_mut(a), Self::from_bytes_mut(b))
    }
    pub fn strip_prefix<T: Pattern>(&self, needle: T) -> Option<&Self> {
        T::strip_prefix(&self.0, needle).map(Self::from_bytes)
    }
    pub fn strip_suffix<T: Pattern>(&self, needle: T) -> Option<&Self> {
        T::strip_suffix(&self.0, needle).map(Self::from_bytes)
    }
    pub fn find<T: Pattern>(&self, needle: T) -> Option<usize> {
        T::find(&self.0, needle)
    }
    pub fn rfind<T: Pattern>(&self, needle: T) -> Option<usize> {
        T::rfind(&self.0, needle)
    }
    pub fn contains<T: Pattern>(&self, needle: T) -> bool {
        self.find(needle).is_some()
    }
}

impl<I: SliceIndex<[u8], Output = [u8]>> Index<I> for OsStr {
    type Output = OsStr;

    #[track_caller]
    fn index(&self, index: I) -> &Self::Output {
        Self::from_bytes(self.0.index(index))
    }
}

impl<I: SliceIndex<[u8], Output = [u8]>> IndexMut<I> for OsStr {
    #[track_caller]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        Self::from_bytes_mut(self.0.index_mut(index))
    }
}

impl AsRef<OsStr> for OsStr {
    fn as_ref(&self) -> &OsStr {
        self
    }
}

impl AsRef<[u8]> for OsStr {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl AsMut<[u8]> for OsStr {
    fn as_mut(&mut self) -> &mut [u8] {
        self.as_bytes_mut()
    }
}

impl AsMut<OsStr> for OsStr {
    fn as_mut(&mut self) -> &mut OsStr {
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

impl Default for &'_ mut OsStr {
    fn default() -> Self {
        OsStr::from_bytes_mut(&mut [])
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

impl_str_partial_eq_ord!(PartialOrd<&'_ OsStr> for OsString; (self, other) -> (self, other));
impl_str_partial_eq_ord!(PartialOrd<&'_ OsStr> for Cow<'_, OsStr>; (self, other) -> (self, other));
impl_str_partial_eq_ord!(PartialOrd<Cow<'_, OsStr>> for OsStr; (self, other) -> (self, other));
impl_str_partial_eq_ord!(PartialOrd<Cow<'_, OsStr>> for OsString; (self, other) -> (self, other));
impl_str_partial_eq_ord!(PartialOrd<Cow<'_, OsStr>> for &'_ OsStr; (self, other) -> (self, other));
impl_str_partial_eq_ord!(PartialOrd<OsStr> for str; (self, other) -> (self.as_ref(), other));
impl_str_partial_eq_ord!(PartialOrd<OsString> for str; (self, other) -> (self.as_ref(), other));
impl_str_partial_eq_ord!(PartialOrd<OsString> for &'_ str; (self, other) -> (self.as_ref(), other));
impl_str_partial_eq_ord!(PartialOrd<OsStr> for OsString; (self, other) -> (self, other));
impl_str_partial_eq_ord!(PartialOrd<OsStr> for Cow<'_, OsStr>; (self, other) -> (self, other));
impl_str_partial_eq_ord!(PartialOrd<OsString> for OsStr; (self, other) -> (self, other));
impl_str_partial_eq_ord!(PartialOrd<OsString> for &'_ OsStr; (self, other) -> (self, other));
impl_str_partial_eq_ord!(PartialOrd<OsString> for Cow<'_, OsStr>; (self, other) -> (self, other));
impl_str_partial_eq_ord!(PartialOrd<str> for OsStr; (self, other) -> (self, other.as_ref()));
impl_str_partial_eq_ord!(PartialOrd<str> for OsString; (self, other) -> (self, other.as_ref()));
impl_str_partial_eq_ord!(PartialOrd<&'_ str> for OsString; (self, other) -> (self, other.as_ref()));

impl ToOwned for OsStr {
    type Owned = OsString;

    fn to_owned(&self) -> Self::Owned {
        self.to_os_string()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct OsString(Vec<u8>);

impl fmt::Debug for OsString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl AsRef<OsStr> for OsString {
    fn as_ref(&self) -> &OsStr {
        self
    }
}

impl AsMut<OsStr> for OsString {
    fn as_mut(&mut self) -> &mut OsStr {
        self
    }
}

impl AsMut<[u8]> for OsString {
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }
}

impl AsRef<[u8]> for OsString {
    fn as_ref(&self) -> &[u8] {
        &self.0
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

impl OsString {
    pub fn new() -> Self {
        OsString(Vec::new())
    }
    pub fn as_os_str(&self) -> &OsStr {
        self
    }
    pub fn as_mut_os_str(&mut self) -> &mut OsStr {
        self
    }
    pub fn into_string(self) -> Result<String, OsString> {
        String::from_utf8(self.0).map_err(|e| OsString(e.into_bytes()))
    }
    pub fn push<T: AsRef<OsStr>>(&mut self, v: T) {
        self.0.extend(v.as_ref().as_bytes());
    }
    pub fn with_capacity(capacity: usize) -> Self {
        OsString(Vec::with_capacity(capacity))
    }
    pub fn clear(&mut self) {
        self.0.clear();
    }
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }
    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional);
    }
    pub fn reserve_exact(&mut self, additional: usize) {
        self.0.reserve_exact(additional);
    }
    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit();
    }
    pub fn into_boxed_os_str(self) -> Box<OsStr> {
        OsStr::from_boxed_bytes(self.0.into_boxed_slice())
    }
    pub fn from_vec(v: Vec<u8>) -> Self {
        OsString(v)
    }
    pub fn into_vec(self) -> Vec<u8> {
        self.0
    }
    pub fn as_mut_vec(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
}

impl<T: ?Sized + AsRef<OsStr>> From<&'_ T> for OsString {
    fn from(v: &'_ T) -> Self {
        v.as_ref().to_os_string()
    }
}

impl<'a> From<&'a OsString> for Cow<'a, OsStr> {
    fn from(v: &'a OsString) -> Self {
        Cow::Borrowed(v)
    }
}

impl From<Box<OsStr>> for OsString {
    fn from(v: Box<OsStr>) -> Self {
        v.into_os_string()
    }
}

impl From<Cow<'_, OsStr>> for OsString {
    fn from(v: Cow<'_, OsStr>) -> Self {
        v.into_owned()
    }
}

impl From<OsString> for Box<OsStr> {
    fn from(v: OsString) -> Self {
        v.into_boxed_os_str()
    }
}

impl From<OsString> for Cow<'_, OsStr> {
    fn from(v: OsString) -> Self {
        Cow::Owned(v)
    }
}

impl From<String> for OsString {
    fn from(v: String) -> Self {
        OsString(v.into_bytes())
    }
}

impl FromStr for OsString {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(s.into())
    }
}

impl<I: SliceIndex<[u8], Output = [u8]>> Index<I> for OsString {
    type Output = OsStr;

    #[track_caller]
    fn index(&self, index: I) -> &Self::Output {
        OsStr::from_bytes(self.0.index(index))
    }
}

impl<I: SliceIndex<[u8], Output = [u8]>> IndexMut<I> for OsString {
    #[track_caller]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        OsStr::from_bytes_mut(self.0.index_mut(index))
    }
}
