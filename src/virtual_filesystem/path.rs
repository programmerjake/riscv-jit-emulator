// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use super::os_str::{OsStr, OsString};
use alloc::{
    borrow::{Cow, ToOwned},
    boxed::Box,
    string::String,
};
use core::{
    borrow::{Borrow, BorrowMut},
    cmp::Ordering,
    fmt,
    iter::FusedIterator,
    mem,
    ops::{Deref, DerefMut},
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Component<'a> {
    RootDir,
    CurDir,
    ParentDir,
    Normal(&'a OsStr),
}

impl<'a> Component<'a> {
    pub fn as_os_str(self) -> &'a OsStr {
        match self {
            Self::RootDir => OsStr::new("/"),
            Self::CurDir => OsStr::new("."),
            Self::ParentDir => OsStr::new(".."),
            Self::Normal(v) => v,
        }
    }
}

impl AsRef<OsStr> for Component<'_> {
    fn as_ref(&self) -> &OsStr {
        self.as_os_str()
    }
}

impl AsRef<Path> for Component<'_> {
    fn as_ref(&self) -> &Path {
        Path::new(self)
    }
}

#[derive(Clone, Debug)]
enum ComponentsPrefix {
    RootDir,
    CurDir,
    None,
}

#[derive(Clone, Debug)]
pub struct Components<'a> {
    path: &'a OsStr,
    prefix: ComponentsPrefix,
}

impl<'a> Components<'a> {
    fn next_non_slash_part(&mut self) -> Option<&'a OsStr> {
        let end = self.path.find(b'/').unwrap_or(self.path.len());
        let (part, rest) = self.path.split_at(end);
        self.path = rest;
        self.trim();
        if part.is_empty() || part == "." {
            None
        } else {
            Some(part)
        }
    }
    fn next_back_non_slash_part(&mut self) -> Option<&'a OsStr> {
        let start = self.path.rfind(b'/').map_or(0, |v| v + 1);
        let (rest, part) = self.path.split_at(start);
        self.path = rest;
        self.trim();
        if part.is_empty() || part == "." {
            None
        } else {
            Some(part)
        }
    }
    fn part_to_component(part: &'a OsStr) -> Component<'a> {
        match part.as_bytes() {
            b".." => Component::ParentDir,
            _ => Component::Normal(part),
        }
    }
    fn strip_leading_slashes(&mut self) {
        let index = self.path.find(|v| v != b'/').unwrap_or(self.path.len());
        self.path = &self.path[index..];
    }
    fn strip_trailing_slashes(&mut self) {
        if let Some(index) = self.path.rfind(|v| v != b'/') {
            self.path = &self.path[..=index];
        } else {
            self.path = OsStr::new("");
        }
    }
    fn trim(&mut self) {
        self.strip_trailing_slashes();
        while let Some(path) = self.path.strip_suffix("/.") {
            self.path = path;
            self.strip_trailing_slashes();
        }
        match self.prefix {
            ComponentsPrefix::RootDir => {
                if let Some(first_non_slash) = self.path.find(|v| v != b'/') {
                    self.path = &self.path[first_non_slash - 1..];
                } else {
                    self.path = OsStr::new("/");
                }
            }
            ComponentsPrefix::CurDir => {
                if self.path == "" {
                    self.path = OsStr::new(".");
                }
            }
            ComponentsPrefix::None => {
                self.strip_leading_slashes();
                while let Some(path) = self.path.strip_prefix("./") {
                    self.path = path;
                    self.strip_leading_slashes();
                }
                if self.path == "." {
                    self.path = OsStr::new("");
                }
            }
        }
    }
    pub fn as_path(&self) -> &'a Path {
        Path::new(self.path)
    }
}

impl AsRef<OsStr> for Components<'_> {
    fn as_ref(&self) -> &OsStr {
        self.path
    }
}

impl AsRef<Path> for Components<'_> {
    fn as_ref(&self) -> &Path {
        self.as_path()
    }
}

impl<'a> Iterator for Components<'a> {
    type Item = Component<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match mem::replace(&mut self.prefix, ComponentsPrefix::None) {
            ComponentsPrefix::None => {}
            ComponentsPrefix::RootDir => {
                self.trim();
                return Some(Component::RootDir);
            }
            ComponentsPrefix::CurDir => {
                self.trim();
                return Some(Component::CurDir);
            }
        }
        if let Some(part) = self.next_non_slash_part() {
            Some(Self::part_to_component(part))
        } else {
            self.path = OsStr::new("");
            None
        }
    }
}

impl<'a> DoubleEndedIterator for Components<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(part) = self.next_back_non_slash_part() {
            return Some(Self::part_to_component(part));
        }
        let retval = match mem::replace(&mut self.prefix, ComponentsPrefix::None) {
            ComponentsPrefix::None => None,
            ComponentsPrefix::RootDir => Some(Component::RootDir),
            ComponentsPrefix::CurDir => Some(Component::CurDir),
        };
        self.path = OsStr::new("");
        retval
    }
}

impl FusedIterator for Components<'_> {}

impl PartialEq for Components<'_> {
    fn eq(&self, other: &Self) -> bool {
        Iterator::eq(self.clone(), other.clone())
    }
}

impl Eq for Components<'_> {}

impl PartialOrd for Components<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Components<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        Iterator::cmp(self.clone(), other.clone())
    }
}

#[derive(Clone, Debug)]
pub struct Iter<'a>(Components<'a>);

impl<'a> Iter<'a> {
    pub fn as_path(&self) -> &'a Path {
        self.0.as_path()
    }
}

impl AsRef<OsStr> for Iter<'_> {
    fn as_ref(&self) -> &OsStr {
        self.0.as_ref()
    }
}

impl AsRef<Path> for Iter<'_> {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}

impl<'a> DoubleEndedIterator for Iter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        Some(self.0.next_back()?.as_os_str())
    }
}

impl FusedIterator for Iter<'_> {}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a OsStr;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.0.next()?.as_os_str())
    }
}

#[derive(Clone, Debug)]
pub struct Ancestors<'a>(Option<&'a Path>);

impl FusedIterator for Ancestors<'_> {}

impl<'a> Iterator for Ancestors<'a> {
    type Item = &'a Path;

    fn next(&mut self) -> Option<Self::Item> {
        let retval = self.0?;
        self.0 = retval.parent();
        Some(retval)
    }
}

fn iter_strip_prefix<Retval, Match>(retval: Retval, match_iter: Match) -> Option<Retval::IntoIter>
where
    Retval: IntoIterator,
    Match: IntoIterator,
    Retval::Item: PartialEq<Match::Item>,
{
    let mut retval = retval.into_iter();
    for match_item in match_iter {
        if retval.next()? != match_item {
            return None;
        }
    }
    Some(retval)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StripPrefixError(());

impl fmt::Display for StripPrefixError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "prefix not found".fmt(f)
    }
}

pub struct Display<'a>(&'a Path);

impl fmt::Debug for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Path(OsStr);

impl Path {
    pub fn new<T: AsRef<OsStr> + ?Sized>(v: &T) -> &Self {
        Self::from_os_str(v.as_ref())
    }
    pub fn as_os_str(&self) -> &OsStr {
        &self.0
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
    pub fn into_boxed_os_str(self: Box<Self>) -> Box<OsStr> {
        // Safety: Path has same representation as OsStr
        unsafe { mem::transmute(self) }
    }
    pub fn into_os_string(self: Box<Self>) -> OsString {
        self.into_boxed_os_str().into_os_string()
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
        self.0.starts_with("/")
    }
    pub fn components<'a>(&'a self) -> Components<'a> {
        let mut retval = Components {
            path: &self.0,
            prefix: if self.has_root() {
                ComponentsPrefix::RootDir
            } else if self.0.starts_with("./") || &self.0 == "." {
                ComponentsPrefix::CurDir
            } else {
                ComponentsPrefix::None
            },
        };
        retval.trim();
        retval
    }
    pub fn parent(&self) -> Option<&Self> {
        let mut components = self.components();
        match components.next_back()? {
            Component::RootDir => None,
            Component::CurDir | Component::ParentDir | Component::Normal(_) => {
                Some(components.as_path())
            }
        }
    }
    pub fn file_name(&self) -> Option<&OsStr> {
        if let Component::Normal(retval) = self.components().next_back()? {
            Some(retval)
        } else {
            None
        }
    }
    pub fn strip_prefix<P: AsRef<Path>>(&self, base: P) -> Result<&Path, StripPrefixError> {
        todo!()
    }
    pub fn starts_with<P: AsRef<Path>>(&self, base: P) -> bool {
        todo!()
    }
    pub fn ends_with<P: AsRef<Path>>(&self, child: P) -> bool {
        todo!()
    }
    pub fn file_stem(&self) -> Option<&OsStr> {
        todo!()
    }
    pub fn extension(&self) -> Option<&OsStr> {
        todo!()
    }
    pub fn join<P: AsRef<Path>>(&self, path: P) -> PathBuf {
        todo!()
    }
    pub fn with_file_name<S: AsRef<OsStr>>(&self, file_name: S) -> PathBuf {
        todo!()
    }
    pub fn with_extension<S: AsRef<OsStr>>(&self, extension: S) -> PathBuf {
        todo!()
    }
    pub fn iter(&self) -> Iter<'_> {
        Iter(self.components())
    }
    pub fn display(&self) -> Display<'_> {
        Display(self)
    }
    pub fn into_path_buf(self: Box<Path>) -> PathBuf {
        PathBuf(self.into_boxed_os_str().into_os_string())
    }
}

impl AsRef<OsStr> for Path {
    fn as_ref(&self) -> &OsStr {
        &self.0
    }
}

impl AsRef<Path> for OsStr {
    fn as_ref(&self) -> &Path {
        Path::from_os_str(self)
    }
}

impl AsRef<Path> for Cow<'_, OsStr> {
    fn as_ref(&self) -> &Path {
        Path::from_os_str(self)
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Borrow<Path> for PathBuf {
    fn borrow(&self) -> &Path {
        self
    }
}

impl BorrowMut<Path> for PathBuf {
    fn borrow_mut(&mut self) -> &mut Path {
        self
    }
}

impl ToOwned for Path {
    type Owned = PathBuf;

    fn to_owned(&self) -> Self::Owned {
        self.to_path_buf()
    }
}

impl From<&'_ Path> for Box<Path> {
    fn from(v: &Path) -> Self {
        Path::from_boxed_os_str(Box::<OsStr>::from(&v.0))
    }
}

impl<'a> From<&'a Path> for Cow<'a, Path> {
    fn from(v: &'a Path) -> Self {
        Cow::Borrowed(v)
    }
}

impl<'a> IntoIterator for &'a Path {
    type Item = &'a OsStr;

    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PathBuf(OsString);
// TODO: add all members and impls for PathBuf

impl fmt::Debug for PathBuf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

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

impl AsMut<Path> for PathBuf {
    fn as_mut(&mut self) -> &mut Path {
        self
    }
}

impl AsRef<Path> for PathBuf {
    fn as_ref(&self) -> &Path {
        self
    }
}

impl AsRef<Path> for Path {
    fn as_ref(&self) -> &Path {
        self
    }
}

impl AsRef<Path> for OsString {
    fn as_ref(&self) -> &Path {
        Path::from_os_str(self)
    }
}

impl AsRef<Path> for str {
    fn as_ref(&self) -> &Path {
        Path::new(self)
    }
}

impl AsRef<Path> for String {
    fn as_ref(&self) -> &Path {
        Path::new(self)
    }
}

impl From<OsString> for PathBuf {
    fn from(v: OsString) -> Self {
        PathBuf(v)
    }
}

impl From<PathBuf> for OsString {
    fn from(v: PathBuf) -> Self {
        v.0
    }
}

impl_str_partial_eq_ord!(PartialOrd<&'_ OsStr> for PathBuf; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<&'_ Path> for PathBuf; (self, other) -> (&self.0, &other.0));
impl_str_partial_eq_ord!(PartialOrd<Path> for PathBuf; (self, other) -> (&self.0, &other.0));
impl_str_partial_eq_ord!(PartialOrd<PathBuf> for Path; (self, other) -> (&self.0, &other.0));
impl_str_partial_eq_ord!(PartialOrd<PathBuf> for &'_ Path; (self, other) -> (&self.0, &other.0));
impl_str_partial_eq_ord!(PartialOrd<Path> for Cow<'_, Path>; (self, other) -> (&self.0, &other.0));
impl_str_partial_eq_ord!(PartialOrd<Cow<'_, Path>> for Path; (self, other) -> (&self.0, &other.0));
impl_str_partial_eq_ord!(PartialOrd<Cow<'_, Path>> for &'_ Path; (self, other) -> (&self.0, &other.0));
impl_str_partial_eq_ord!(PartialOrd<&'_ OsStr> for Path; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<Cow<'_, OsStr>> for Path; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<Cow<'_, OsStr>> for &'_ Path; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<OsStr> for PathBuf; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<OsString> for PathBuf; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<OsStr> for Path; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<OsString> for Path; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<OsStr> for &'_ Path; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<OsString> for &'_ Path; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<OsStr> for Cow<'_, Path>; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<OsString> for Cow<'_, Path>; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<&'_ Path> for OsStr; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<&'_ Path> for Cow<'_, OsStr>; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<&'_ Path> for Cow<'_, Path>; (self, other) -> (&self.0, &other.0));
impl_str_partial_eq_ord!(PartialOrd<&'_ Path> for OsString; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<&'_ OsStr> for Cow<'_, Path>; (self, other) -> (&self.0, other));
impl_str_partial_eq_ord!(PartialOrd<Cow<'_, Path>> for OsStr; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<Cow<'_, Path>> for OsString; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<Cow<'_, Path>> for &'_ OsStr; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<Path> for OsStr; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<Path> for OsString; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<Path> for &'_ OsStr; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<Path> for Cow<'_, OsStr>; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<PathBuf> for OsStr; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<PathBuf> for OsString; (self, other) -> (self, &other.0));
impl_str_partial_eq_ord!(PartialOrd<PathBuf> for &'_ OsStr; (self, other) -> (self, &other.0));

#[cfg(test)]
mod test {
    use std::{
        dbg,
        path::{Component as StdComponent, Path as StdPath},
        println,
        vec::Vec,
    };

    fn convert_from_std_component(std_component: StdComponent) -> super::Component {
        match std_component {
            StdComponent::Prefix(_) => unreachable!(),
            StdComponent::CurDir => super::Component::CurDir,
            StdComponent::ParentDir => super::Component::ParentDir,
            StdComponent::RootDir => super::Component::RootDir,
            StdComponent::Normal(v) => {
                super::Component::Normal(super::OsStr::new(v.to_str().unwrap()))
            }
        }
    }

    fn test_components(text: &str) {
        let std_components = match text.find(|c| c != '/') {
            None | Some(0..=1) => StdPath::new(text),
            // linux normalizes multiple leading slashes to a single one,
            // work around std::path::Path not doing that normalization
            Some(index) => StdPath::new(&text[index - 1..]),
        }
        .components();
        let std_components_list: Vec<_> = std_components.clone().collect();
        dbg!(&std_components_list);
        let step_count = std_components_list.len() + 1;
        for seed in 0..1u32 << step_count {
            println!("seed=0b{:b}", seed);
            let mut components = super::Path::new(text).components();
            let mut std_components = std_components.clone();
            assert_eq!(
                dbg!(std_components.as_path().to_str().unwrap()),
                components.as_path().to_str().unwrap()
            );
            for step in 0..step_count {
                let from_back = (seed & 1 << step) != 0;
                let expected = if from_back {
                    std_components.next_back()
                } else {
                    std_components.next()
                };
                let expected = dbg!(expected).map(convert_from_std_component);
                let actual = if from_back {
                    components.next_back()
                } else {
                    components.next()
                };
                assert_eq!(expected, actual);
                assert_eq!(
                    dbg!(std_components.as_path().to_str().unwrap()),
                    components.as_path().to_str().unwrap()
                );
            }
        }
    }

    #[test]
    fn test_components_empty() {
        test_components("");
    }

    #[test]
    fn test_components_slash() {
        test_components("/");
    }

    #[test]
    fn test_components_slash_slash() {
        test_components("//");
    }

    #[test]
    fn test_components_slash_slash_dot() {
        test_components("//.");
    }

    #[test]
    fn test_components_slash_slash_dot_slash() {
        test_components("//./");
    }

    #[test]
    fn test_components_dot_slash() {
        test_components("./");
    }

    #[test]
    fn test_components_dot_slash_slash() {
        test_components(".//");
    }

    #[test]
    fn test_components_dot() {
        test_components(".");
    }

    #[test]
    fn test_components_slash_slash_dot_dot() {
        test_components("//..");
    }

    #[test]
    fn test_components_slash_slash_dot_dot_slash() {
        test_components("//../");
    }

    #[test]
    fn test_components_dot_dot_slash() {
        test_components("../");
    }

    #[test]
    fn test_components_dot_dot_slash_slash() {
        test_components("..//");
    }

    #[test]
    fn test_components_dot_dot() {
        test_components("..");
    }

    #[test]
    fn test_components_a() {
        test_components("a");
    }

    #[test]
    fn test_components_a_b() {
        test_components("a/b");
    }

    #[test]
    fn test_components_a_slash() {
        test_components("a/");
    }

    #[test]
    fn test_components_dot_a() {
        test_components("./a");
    }

    #[test]
    fn test_components_a_dot_b() {
        test_components("a/./b");
    }

    #[test]
    fn test_components_a_dot_dot_b() {
        test_components("a/../b");
    }

    #[test]
    fn test_components_slash_a_dot_b_dot() {
        test_components("/a/./b/.");
    }

    #[test]
    fn test_components_slash_slash_a_b() {
        test_components("//a/b");
    }

    #[test]
    fn test_components_slash_slash_slash_a_b() {
        test_components("///a/b");
    }
}
