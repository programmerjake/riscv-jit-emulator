// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use super::os_str::{OsStr, OsString};
use alloc::{borrow::Cow, boxed::Box, string::String};
use core::{
    fmt,
    iter::FusedIterator,
    mem,
    ops::{Deref, DerefMut},
    slice,
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
    bytes: &'a OsStr,
    prefix: ComponentsPrefix,
}

impl<'a> Components<'a> {
    fn next_non_slash_part(&mut self) -> Option<&'a OsStr> {
        let start = self.bytes.find(|v| v != b'/').unwrap_or(self.bytes.len());
        self.bytes = &self.bytes[start..];
        let end = self.bytes.find(b'/').unwrap_or(self.bytes.len());
        let (part, rest) = self.bytes.split_at(end);
        self.bytes = rest;
        if part.is_empty() {
            None
        } else {
            Some(part)
        }
    }
    fn next_back_non_slash_part(&mut self) -> Option<&'a OsStr> {
        let end = self.bytes.rfind(|v| v != b'/').map_or(0, |v| v + 1);
        self.bytes = &self.bytes[..end];
        let start = self.bytes.rfind(b'/').map_or(0, |v| v + 1);
        let (rest, part) = self.bytes.split_at(start);
        self.bytes = rest;
        if part.is_empty() {
            None
        } else {
            Some(part)
        }
    }
    fn part_to_component(part: &'a OsStr) -> Option<Component<'a>> {
        match part.as_bytes() {
            b"." => None,
            b".." => Some(Component::ParentDir),
            _ => Some(Component::Normal(part)),
        }
    }
}

impl<'a> Iterator for Components<'a> {
    type Item = Component<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match mem::replace(&mut self.prefix, ComponentsPrefix::None) {
            ComponentsPrefix::None => {}
            ComponentsPrefix::RootDir => return Some(Component::RootDir),
            ComponentsPrefix::CurDir => return Some(Component::CurDir),
        }
        while let Some(part) = self.next_non_slash_part() {
            if let Some(retval) = Self::part_to_component(part) {
                return Some(retval);
            }
        }
        None
    }
}

impl<'a> DoubleEndedIterator for Components<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        while let Some(part) = self.next_back_non_slash_part() {
            if let Some(retval) = Self::part_to_component(part) {
                return Some(retval);
            }
        }
        match mem::replace(&mut self.prefix, ComponentsPrefix::None) {
            ComponentsPrefix::None => None,
            ComponentsPrefix::RootDir => Some(Component::RootDir),
            ComponentsPrefix::CurDir => Some(Component::CurDir),
        }
    }
}

impl FusedIterator for Components<'_> {}

impl PartialEq for Components<'_> {
    fn eq(&self, other: &Self) -> bool {
        Iterator::eq(self.clone(), other.clone())
    }
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
        self.0.starts_with("/")
    }
    pub fn components<'a>(&'a self) -> Components<'a> {
        Components {
            bytes: &self.0,
            prefix: if self.has_root() {
                ComponentsPrefix::RootDir
            } else if self.0.starts_with("./") || &self.0 == "." {
                ComponentsPrefix::CurDir
            } else {
                ComponentsPrefix::None
            },
        }
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
        let std_components: Vec<_> = StdPath::new(text).components().collect();
        dbg!(&std_components);
        let step_count = std_components.len() + 1;
        for seed in 0..1u32 << step_count {
            println!("seed=0b{:b}", seed);
            let mut components = super::Path::new(text).components();
            let mut std_components = std_components.iter().copied().enumerate();
            for step in 0..step_count {
                let from_back = (seed & 1 << step) != 0;
                let expected = if from_back {
                    std_components.next_back()
                } else {
                    std_components.next()
                };
                let expected = dbg!(expected).map(|v| convert_from_std_component(v.1));
                let actual = if from_back {
                    components.next_back()
                } else {
                    components.next()
                };
                assert_eq!(expected, actual);
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
    fn test_components_a_dot_b() {
        test_components("a/./b");
    }

    #[test]
    fn test_components_a_dot_dot_b() {
        test_components("a/../b");
    }
}
