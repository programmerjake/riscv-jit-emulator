// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use fmt::Display;
use once_cell::unsync::OnceCell;
use std::{borrow::Cow, error::Error, fmt, iter, ops::Range};
use unicode_width::UnicodeWidthChar;

pub mod ast;

pub struct SourceCode<'a> {
    file_name: &'a str,
    contents: &'a str,
    line_start_byte_indexes: OnceCell<Vec<usize>>,
}

impl fmt::Debug for SourceCode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceCode")
            .field("file_name", &self.file_name)
            .finish()
    }
}

impl<'a> SourceCode<'a> {
    pub fn new(file_name: &'a str, contents: &'a str) -> Self {
        Self {
            file_name,
            contents,
            line_start_byte_indexes: OnceCell::new(),
        }
    }
    /// byte indexes of line starts
    /// always starts with 0
    pub fn line_start_byte_indexes(&self) -> &[usize] {
        self.line_start_byte_indexes.get_or_init(|| {
            let mut line_start_byte_indexes = vec![0];
            for (index, byte) in self.contents.bytes().enumerate() {
                if byte == b'\n' {
                    // don't need to specifically check for "\r\n" since
                    // line start still is right after '\n'
                    line_start_byte_indexes.push(index + 1);
                }
            }
            line_start_byte_indexes
        })
    }
    /// 0-based line number of the line containing byte_index
    pub fn line_index_of_containing_line(&self, byte_index: usize) -> usize {
        let line_start_byte_indexes = self.line_start_byte_indexes();
        match line_start_byte_indexes.binary_search(&byte_index) {
            Ok(index) => index,
            Err(index) => index - 1,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct SourceLocation<'a> {
    pub source_code: &'a SourceCode<'a>,
    pub byte_index: usize,
}

impl<'a> SourceLocation<'a> {
    pub const DEFAULT_TAB_WIDTH: usize = 8;
    /// Convert to `ResolvedSourceLocation`.
    /// This is a relatively expensive operation since line and column information needs to be calculated.
    pub fn resolve_with_tab_width(self, tab_width: usize) -> ResolvedSourceLocation {
        let file_name = self.source_code.file_name.into();
        let byte_index = self.byte_index;
        let contents = self.source_code.contents;
        let line_index = self
            .source_code
            .line_index_of_containing_line(self.byte_index);
        let line_start_index = self.source_code.line_start_byte_indexes()[line_index];
        let line_number = line_index + 1;
        let column_number =
            1 + contents[line_start_index..byte_index]
                .chars()
                .fold(0, |col, ch| {
                    // col is zero-based
                    if ch == '\t' {
                        if tab_width == 0 {
                            col
                        } else {
                            (col + tab_width) / tab_width * tab_width
                        }
                    } else {
                        col + ch.width().unwrap_or(0)
                    }
                });
        ResolvedSourceLocation {
            file_name,
            byte_index,
            line_number,
            column_number,
        }
    }
    /// Convert to `ResolvedSourceLocation`.
    /// This is a relatively expensive operation since line and column information needs to be calculated.
    pub fn resolve(self) -> ResolvedSourceLocation {
        self.resolve_with_tab_width(Self::DEFAULT_TAB_WIDTH)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct ResolvedSourceLocation {
    /// the file name for the source code
    pub file_name: String,
    /// the 0-based index of the source code byte where the error occurred
    pub byte_index: usize,
    /// the 1-based line number where the error occurred
    pub line_number: usize,
    /// the 1-based column number where the error occurred
    pub column_number: usize,
}

impl fmt::Display for ResolvedSourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file_name, self.line_number, self.column_number
        )
    }
}

#[derive(Clone, Copy)]
pub struct Span<'a> {
    pub source_code: &'a SourceCode<'a>,
    pub start_byte_index: usize,
    /// the byte index for the first character after the end of this span.
    pub end_byte_index: usize,
}

impl fmt::Debug for Span<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}:{}..{}={:?}",
            self.source_code.file_name,
            self.start_byte_index,
            self.end_byte_index,
            self.source_text()
        )
    }
}

impl<'a> Span<'a> {
    pub fn start(self) -> SourceLocation<'a> {
        SourceLocation {
            source_code: self.source_code,
            byte_index: self.start_byte_index,
        }
    }
    /// gets the `SourceLocation` for the first character after the end of this span.
    pub fn end(self) -> SourceLocation<'a> {
        SourceLocation {
            source_code: self.source_code,
            byte_index: self.end_byte_index,
        }
    }
    pub fn source_text(self) -> &'a str {
        &self.source_code.contents[self.start_byte_index..self.end_byte_index]
    }
}

#[derive(Debug)]
pub struct ParseError {
    location: ResolvedSourceLocation,
    message: Cow<'static, str>,
    source: Option<Box<dyn Error + Send + Sync>>,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: error: {}", self.location, self.message)
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.source {
            Some(v) => Some(&**v),
            None => None,
        }
    }
}

#[derive(Debug)]
pub struct UnresolvedParseError<'a> {
    location: SourceLocation<'a>,
    message: Cow<'static, str>,
    source: Option<Box<dyn Error + Send + Sync>>,
}

impl fmt::Display for UnresolvedParseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            location,
            message,
            source: _,
        } = self;
        ParseError {
            location: location.resolve(),
            message: message.clone(),
            source: None,
        }
        .fmt(f)
    }
}

impl<'a> Error for UnresolvedParseError<'a> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.source {
            Some(v) => Some(&**v),
            None => None,
        }
    }
}

impl<'a> From<UnresolvedParseError<'a>> for ParseError {
    fn from(e: UnresolvedParseError<'a>) -> Self {
        let UnresolvedParseError {
            location,
            message,
            source,
        } = e;
        ParseError {
            location: location.resolve(),
            message,
            source,
        }
    }
}

pub trait Parse<'a>: Sized {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>>;
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    location: SourceLocation<'a>,
}

pub trait ParseAlternatives<'a, T> {
    type Output;
    fn parse_alternatives(
        &mut self,
        alternatives: T,
    ) -> Result<Self::Output, UnresolvedParseError<'a>>;
}

macro_rules! make_parse_alternatives {
    ()=>{};
    ($arg0_name:ident: $arg0_type:ident, $($arg_name:ident: $arg_type:ident, )*) => {
        impl<'a, R, $arg0_type $(, $arg_type)*> ParseAlternatives<'a, ($arg0_type, $($arg_type, )*)> for Parser<'a>
        where
            $arg0_type: FnMut(&mut Self) -> Result<R, UnresolvedParseError<'a>>,
            $($arg_type: FnMut(&mut Self) -> Result<R, UnresolvedParseError<'a>>,)*
        {
            type Output = R;
            fn parse_alternatives(
                &mut self,
                alternatives: ($arg0_type, $($arg_type, )*),
            ) -> Result<Self::Output, UnresolvedParseError<'a>> {
                let (mut $arg0_name, $(mut $arg_name,)*) = alternatives;
                let alternatives: &mut [&mut dyn FnMut(&mut Self) -> Result<R, UnresolvedParseError<'a>>] = &mut [&mut $arg0_name, $(&mut $arg_name,)*];
                self.parse_alternatives_iter(alternatives)
            }
        }
        make_parse_alternatives!($($arg_name: $arg_type,)*);
    };
}

make_parse_alternatives!(
    arg0: Arg0,
    arg1: Arg1,
    arg2: Arg2,
    arg3: Arg3,
    arg4: Arg4,
    arg5: Arg5,
    arg6: Arg6,
    arg7: Arg7,
    arg8: Arg8,
    arg9: Arg9,
    arg10: Arg10,
    arg11: Arg11,
    arg12: Arg12,
    arg13: Arg13,
    arg14: Arg14,
    arg15: Arg15,
    arg16: Arg16,
    arg17: Arg17,
    arg18: Arg18,
    arg19: Arg19,
    arg20: Arg20,
    arg21: Arg21,
    arg22: Arg22,
    arg23: Arg23,
    arg24: Arg24,
    arg25: Arg25,
    arg26: Arg26,
    arg27: Arg27,
    arg28: Arg28,
    arg29: Arg29,
);

impl<'a> Parser<'a> {
    pub fn new(source_code: &'a SourceCode<'a>) -> Self {
        Self {
            location: SourceLocation {
                source_code,
                byte_index: 0,
            },
        }
    }
    pub fn location(&self) -> SourceLocation<'a> {
        self.location
    }
    pub fn span<T, E>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, E>,
    ) -> Result<(T, Span<'a>), E> {
        let start_byte_index = self.location.byte_index;
        let retval = f(self)?;
        Ok((
            retval,
            Span {
                source_code: self.location.source_code,
                start_byte_index,
                end_byte_index: self.location.byte_index,
            },
        ))
    }
    pub fn remaining_text(&self) -> &'a str {
        &self.location.source_code.contents[self.location.byte_index..]
    }
    pub fn preceding_text(&self) -> &'a str {
        &self.location.source_code.contents[..self.location.byte_index]
    }
    pub fn advance(&mut self, byte_count: usize) {
        self.location.byte_index += byte_count;
    }
    pub fn parse<T: Parse<'a>>(&mut self) -> Result<T, UnresolvedParseError<'a>> {
        T::parse(self)
    }
    pub fn parse_opt<T, F: FnOnce(&mut Self) -> Result<T, UnresolvedParseError<'a>>>(
        &mut self,
        f: F,
    ) -> Option<T> {
        let mut temp_parser = self.clone();
        let retval = f(&mut temp_parser).ok()?;
        *self = temp_parser;
        Some(retval)
    }
    pub fn parse_alternatives_iter<
        R,
        F: FnOnce(&mut Self) -> Result<R, UnresolvedParseError<'a>>,
        I: IntoIterator<Item = F>,
    >(
        &mut self,
        alternatives: I,
    ) -> Result<R, UnresolvedParseError<'a>> {
        let mut iter = alternatives.into_iter();
        let mut try_alternative = |f: F| -> Result<R, UnresolvedParseError<'a>> {
            let mut temp_parser = self.clone();
            let retval = f(&mut temp_parser)?;
            *self = temp_parser;
            Ok(retval)
        };
        let mut error = match try_alternative(iter.next().expect("no alternatives")) {
            Ok(retval) => return Ok(retval),
            Err(e) => e,
        };
        for alternative in iter {
            match try_alternative(alternative) {
                Ok(retval) => return Ok(retval),
                Err(e) => {
                    if e.location.byte_index > error.location.byte_index {
                        error = e;
                    }
                }
            };
        }
        Err(error)
    }
    pub fn parse_many0_with_fn<F: FnMut(&mut Self) -> Result<(), UnresolvedParseError<'a>>>(
        &mut self,
        mut f: F,
    ) -> UnresolvedParseError<'a> {
        let mut last_good_parser = self.clone();
        let error = loop {
            match f(self) {
                Ok(()) => {}
                Err(error) => break error,
            }
            last_good_parser = self.clone();
        };
        *self = last_good_parser;
        error
    }
    pub fn parse_many0_with<T: Parse<'a>, P: FnMut(T)>(
        &mut self,
        mut push: P,
    ) -> UnresolvedParseError<'a> {
        self.parse_many0_with_fn(|parser| {
            push(T::parse(parser)?);
            Ok(())
        })
    }
    pub fn parse_many0_with_err<T: Parse<'a>>(&mut self) -> (Vec<T>, UnresolvedParseError<'a>) {
        let mut retval = Vec::new();
        let err = self.parse_many0_with(|v| retval.push(v));
        (retval, err)
    }
    pub fn parse_many1_with_err<T: Parse<'a>>(
        &mut self,
    ) -> Result<(Vec<T>, UnresolvedParseError<'a>), UnresolvedParseError<'a>> {
        let mut retval = vec![T::parse(self)?];
        let err = self.parse_many0_with(|v| retval.push(v));
        Ok((retval, err))
    }
    pub fn parse_many0<T: Parse<'a>>(&mut self) -> Vec<T> {
        self.parse_many0_with_err().0
    }
    pub fn parse_many1<T: Parse<'a>>(&mut self) -> Result<Vec<T>, UnresolvedParseError<'a>> {
        Ok(self.parse_many1_with_err()?.0)
    }
    pub fn peek<T: Parse<'a>>(&self) -> bool {
        T::parse(&mut self.clone()).is_ok()
    }
    pub fn error_str(&self, message: &'static str) -> UnresolvedParseError<'a> {
        UnresolvedParseError {
            location: self.location,
            message: Cow::Borrowed(message),
            source: None,
        }
    }
    pub fn error_string(&self, message: String) -> UnresolvedParseError<'a> {
        UnresolvedParseError {
            location: self.location,
            message: Cow::Owned(message),
            source: None,
        }
    }
    pub fn error_full(
        &self,
        message: Cow<'static, str>,
        source: Option<Box<dyn Error + Send + Sync>>,
    ) -> UnresolvedParseError<'a> {
        UnresolvedParseError {
            location: self.location,
            message,
            source,
        }
    }
}
