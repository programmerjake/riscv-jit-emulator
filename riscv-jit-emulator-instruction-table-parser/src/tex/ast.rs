// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use serde::Serialize;
use std::{borrow::Cow, fmt};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Pos {
    pub byte_index: usize,
}

pub trait GetPos {
    fn pos(&self) -> Pos;
}

impl<T: ?Sized + GetPos> GetPos for &'_ T {
    fn pos(&self) -> Pos {
        (**self).pos()
    }
}

impl<T: ?Sized + GetPos> GetPos for &'_ mut T {
    fn pos(&self) -> Pos {
        (**self).pos()
    }
}

impl GetPos for Pos {
    fn pos(&self) -> Pos {
        *self
    }
}

macro_rules! impl_get_pos_simple {
    ($name:ident) => {
        impl GetPos for $name {
            fn pos(&self) -> Pos {
                self.pos
            }
        }
    };
}

impl Serialize for Pos {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        format!("{:?}", self).serialize(serializer)
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.byte_index)
    }
}

impl Pos {
    pub fn new(byte_index: usize) -> Self {
        Self { byte_index }
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct Escape {
    pub pos: Pos,
}

impl_get_pos_simple!(Escape);

#[derive(Serialize, Debug, Clone)]
pub struct BeginGroup {
    pub pos: Pos,
}

impl_get_pos_simple!(BeginGroup);

#[derive(Serialize, Debug, Clone)]
pub struct EndGroup {
    pub pos: Pos,
}

impl_get_pos_simple!(EndGroup);

#[derive(Serialize, Debug, Clone)]
pub struct AlignmentTab {
    pub pos: Pos,
}

impl_get_pos_simple!(AlignmentTab);

#[derive(Serialize, Debug, Clone)]
pub struct MacroParameter {
    pub pos: Pos,
}

impl_get_pos_simple!(MacroParameter);

#[derive(Serialize, Debug, Clone)]
pub struct CommentStart {
    pub pos: Pos,
}

impl_get_pos_simple!(CommentStart);

#[derive(Serialize, Debug, Clone)]
pub struct MathShift {
    pub pos: Pos,
}

impl_get_pos_simple!(MathShift);

#[derive(Serialize, Debug, Clone)]
pub struct Superscript {
    pub pos: Pos,
}

impl_get_pos_simple!(Superscript);

#[derive(Serialize, Debug, Clone)]
pub struct Subscript {
    pub pos: Pos,
}

impl_get_pos_simple!(Subscript);

#[derive(Serialize, Debug, Clone)]
pub struct Ignore {
    pub pos: Pos,
}

impl_get_pos_simple!(Ignore);

#[derive(Serialize, Debug, Clone)]
pub struct AnyChar {
    pub pos: Pos,
}

impl_get_pos_simple!(AnyChar);

#[derive(Serialize, Debug, Clone)]
pub struct Punctuation {
    pub pos: Pos,
}

impl_get_pos_simple!(Punctuation);

#[derive(Serialize, Debug, Clone)]
pub struct Space {
    pub pos: Pos,
}

impl_get_pos_simple!(Space);

#[derive(Serialize, Debug, Clone)]
pub struct AsciiAlphabetic {
    pub pos: Pos,
}

impl_get_pos_simple!(AsciiAlphabetic);

#[derive(Serialize, Debug, Clone)]
pub struct AsciiDigit {
    pub pos: Pos,
}

impl_get_pos_simple!(AsciiDigit);

#[derive(Serialize, Debug, Clone)]
pub struct NewLine {
    pub pos: Pos,
}

impl_get_pos_simple!(NewLine);

#[derive(Serialize, Debug, Clone)]
pub struct CharToken {
    pub pos: Pos,
}

impl_get_pos_simple!(CharToken);

#[derive(Serialize, Debug, Clone)]
pub struct CharTokens {
    pub pos: Pos,
    pub content: String,
}

impl CharTokens {
    pub fn split_off(&mut self, index: usize) -> Self {
        let content = self.content.split_off(index);
        Self {
            pos: Pos {
                byte_index: self.pos.byte_index + index,
            },
            content,
        }
    }
    pub fn split_at(mut self, index: usize) -> (Self, Self) {
        let tail = self.split_off(index);
        (self, tail)
    }
}

impl_get_pos_simple!(CharTokens);

#[derive(Serialize, Debug, Clone)]
pub struct Document {
    pub content: Vec<Token>,
}

impl GetPos for Document {
    fn pos(&self) -> Pos {
        self.content
            .first()
            .map_or(Pos { byte_index: 0 }, GetPos::pos)
    }
}

macro_rules! declare_nested_access_fns {
    ($alternative:ident($alternative_fn:ident, $alternative_mut_fn:ident, $into_alternative_fn:ident), ($outer_alternative_fn:ident, $outer_alternative_mut_fn:ident, $outer_into_alternative_fn:ident)) => {
        pub fn $alternative_fn(&self) -> Option<&$alternative> {
            self.$outer_alternative_fn()?.$alternative_fn()
        }
        pub fn $alternative_mut_fn(&mut self) -> Option<&mut $alternative> {
            self.$outer_alternative_mut_fn()?.$alternative_mut_fn()
        }
        pub fn $into_alternative_fn(self) -> Option<$alternative> {
            self.$outer_into_alternative_fn()?.$into_alternative_fn()
        }
    };
}

macro_rules! declare_transparent_enum {
    (
        #[serde(tag = $tag:literal)]
        #[access_fns_macro = $access_fns_macro:ident]
        enum $name:ident {
            $(
                $(#[nested_access_fns = $nested_access_fns:ident])?
                $alternative:ident($alternative_fn:ident, $alternative_mut_fn:ident, $into_alternative_fn:ident),
            )+
        }
    ) => {
        #[derive(Serialize, Clone)]
        #[serde(tag = $tag)]
        pub enum $name {
            $($alternative($alternative),)+
        }

        #[allow(unused_macros)]
        macro_rules! $access_fns_macro {
            ($m:ident!($tt:tt)) => {
                $($m!($alternative($alternative_fn, $alternative_mut_fn, $into_alternative_fn), $tt);)+
            };
        }

        impl $name {
            $(
                pub fn $alternative_fn(&self) -> Option<&$alternative> {
                    if let Self::$alternative(v) = self {
                        Some(v)
                    } else {
                        None
                    }
                }
                pub fn $alternative_mut_fn(&mut self) -> Option<&mut $alternative> {
                    if let Self::$alternative(v) = self {
                        Some(v)
                    } else {
                        None
                    }
                }
                pub fn $into_alternative_fn(self) -> Option<$alternative> {
                    if let Self::$alternative(v) = self {
                        Some(v)
                    } else {
                        None
                    }
                }
                $(
                    $nested_access_fns!(declare_nested_access_fns!(($alternative_fn, $alternative_mut_fn, $into_alternative_fn)));
                )?
            )+
        }

        impl GetPos for $name {
            fn pos(&self) -> Pos {
                match self {
                    $(Self::$alternative(v) => v.pos(),)+
                }
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(Self::$alternative(v) => fmt::Debug::fmt(v, f),)+
                }
            }
        }
    };
}

declare_transparent_enum! {
    #[serde(tag = "special_macro_type")]
    #[access_fns_macro = special_macro_access_fns]
    enum SpecialMacro {
        Verb(verb, verb_mut, into_verb),
        VerbatimEnvironment(verbatim_environment, verbatim_environment_mut, into_verbatim_environment),
        DisplayMath(display_math, display_math_mut, into_display_math),
        ParenthesizedInlineMath(parenthesized_inline_math, parenthesized_inline_math_mut, into_parenthesized_inline_math),
        MathEnvironment(math_environment, math_environment_mut, into_math_environment),
        Environment(environment, environment_mut, into_environment),
    }
}

declare_transparent_enum! {
    #[serde(tag = "token_type")]
    #[access_fns_macro = token_access_fns]
    enum Token {
        #[nested_access_fns = special_macro_access_fns]
        SpecialMacro(special_macro, special_macro_mut, into_special_macro),
        Macro(macro_, macro_mut, into_macro),
        FullComment(full_comment, full_comment_mut, into_full_comment),
        Group(group, group_mut, into_group),
        DollarInlineMath(dollar_inline_math, dollar_inline_math_mut, into_dollar_inline_math),
        AlignmentTab(alignment_tab, alignment_tab_mut, into_alignment_tab),
        ParBreak(par_break, par_break_mut, into_par_break),
        MacroParameter(macro_parameter, macro_parameter_mut, into_macro_parameter),
        Ignore(ignore, ignore_mut, into_ignore),
        Number(number, number_mut, into_number),
        Whitespace(whitespace, whitespace_mut, into_whitespace),
        Punctuation(punctuation, punctuation_mut, into_punctuation),
        CharTokens(char_tokens, char_tokens_mut, into_char_tokens),
        BeginGroup(begin_group, begin_group_mut, into_begin_group),
        EndGroup(end_group, end_group_mut, into_end_group),
        MathShift(math_shift, math_shift_mut, into_math_shift),
    }
}

pub struct SplitCharTokensIter<Iter> {
    next_chars: Option<CharTokens>,
    iter: Iter,
}

impl<Iter: Iterator> SplitCharTokensIter<Iter> {
    pub fn new<T: IntoIterator<IntoIter = Iter>>(iter: T) -> Self {
        Self {
            next_chars: None,
            iter: iter.into_iter(),
        }
    }
    pub fn into(self) -> (Option<CharTokens>, Iter) {
        (self.next_chars, self.iter)
    }
    fn take_first_char(&mut self, next_chars: Option<CharTokens>) -> Option<CharTokens> {
        let next_chars = next_chars?;
        let ch = next_chars.content.chars().next()?;
        let (retval, rest) = next_chars.split_at(ch.len_utf8());
        if !rest.content.is_empty() {
            self.next_chars = Some(rest);
        }
        Some(retval)
    }
}

mod sealed {
    pub trait Sealed {}
    impl Sealed for super::Token {}
    impl Sealed for &'_ super::Token {}
}

pub trait SplitCharTokensIterHelper: Sized + sealed::Sealed {
    type Item;
    fn next<I: Iterator<Item = Self>>(this: &mut SplitCharTokensIter<I>) -> Option<Self::Item>;
}

impl SplitCharTokensIterHelper for Token {
    type Item = Token;

    fn next<I: Iterator<Item = Self>>(this: &mut SplitCharTokensIter<I>) -> Option<Self::Item> {
        let next_chars = this.next_chars.take();
        if let Some(retval) = this.take_first_char(next_chars) {
            return Some(Token::CharTokens(retval));
        }
        loop {
            match this.iter.next()? {
                Token::CharTokens(next_chars) => {
                    if let Some(retval) = this.take_first_char(Some(next_chars)) {
                        return Some(Token::CharTokens(retval));
                    }
                }
                retval => return Some(retval),
            }
        }
    }
}

impl<'a> SplitCharTokensIterHelper for &'a Token {
    type Item = Cow<'a, Token>;

    fn next<I: Iterator<Item = Self>>(this: &mut SplitCharTokensIter<I>) -> Option<Self::Item> {
        let next_chars = this.next_chars.take();
        if let Some(retval) = this.take_first_char(next_chars) {
            return Some(Cow::Owned(Token::CharTokens(retval)));
        }
        loop {
            match this.iter.next()? {
                Token::CharTokens(next_chars) => {
                    if let Some(retval) = this.take_first_char(Some(next_chars.clone())) {
                        return Some(Cow::Owned(Token::CharTokens(retval)));
                    }
                }
                retval => return Some(Cow::Borrowed(retval)),
            }
        }
    }
}

impl<I, Item> Iterator for SplitCharTokensIter<I>
where
    I: Iterator<Item = Item>,
    Item: SplitCharTokensIterHelper,
{
    type Item = Item::Item;

    fn next(&mut self) -> Option<Self::Item> {
        Item::next(self)
    }
}

declare_transparent_enum! {
    #[serde(tag = "math_token_type")]
    #[access_fns_macro = math_token_access_fns]
    enum MathToken {
        #[nested_access_fns = special_macro_access_fns]
        SpecialMacro(special_macro, special_macro_mut, into_special_macro),
        Macro(macro_, macro_mut, into_macro),
        FullComment(full_comment, full_comment_mut, into_full_comment),
        MathGroup(math_group, math_group_mut, into_math_group),
        AlignmentTab(alignment_tab, alignment_tab_mut, into_alignment_tab),
        MacroParameter(macro_parameter, macro_parameter_mut, into_macro_parameter),
        Superscript(superscript, superscript_mut, into_superscript),
        Subscript(subscript, subscript_mut, into_subscript),
        Ignore(ignore, ignore_mut, into_ignore),
        Whitespace(whitespace, whitespace_mut, into_whitespace),
        AnyChar(any_char, any_char_mut, into_any_char),
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct Verb {
    pub escape: Escape,
    pub env: String,
    pub delimiter: char,
    pub content: String,
}

impl GetPos for Verb {
    fn pos(&self) -> Pos {
        self.escape.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct ParenthesizedInlineMath {
    pub begin: BeginInlineMath,
    pub content: Vec<MathToken>,
    pub end: EndInlineMath,
}

impl ParenthesizedInlineMath {
    pub fn content_pos(&self) -> Pos {
        self.content.first().map_or(self.end.pos(), GetPos::pos)
    }
}

impl GetPos for ParenthesizedInlineMath {
    fn pos(&self) -> Pos {
        self.begin.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct DollarInlineMath {
    pub begin: MathShift,
    pub content: Vec<MathToken>,
    pub end: MathShift,
}

impl DollarInlineMath {
    pub fn content_pos(&self) -> Pos {
        self.content.first().map_or(self.end.pos(), GetPos::pos)
    }
}

impl GetPos for DollarInlineMath {
    fn pos(&self) -> Pos {
        self.begin.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct VerbatimEnvironment {
    pub begin: BeginEnvironment,
    pub name: VerbatimEnvironmentName,
    pub body: String,
    pub end: EndEnvironment,
}

impl GetPos for VerbatimEnvironment {
    fn pos(&self) -> Pos {
        self.begin.pos()
    }
}

#[derive(Serialize, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum VerbatimEnvironmentNameKind {
    VerbatimStar,
    Verbatim,
    FileContentsStar,
    FileContents,
    Comment,
    ListListing,
}

#[derive(Serialize, Debug, Clone)]
pub struct VerbatimEnvironmentName {
    pub pos: Pos,
    pub kind: VerbatimEnvironmentNameKind,
}

impl_get_pos_simple!(VerbatimEnvironmentName);

#[derive(Serialize, Debug, Clone)]
pub struct DisplayMath {
    pub pos: Pos,
    pub content: Vec<MathToken>,
}

impl_get_pos_simple!(DisplayMath);

#[derive(Serialize, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum MathEnvironmentNameKind {
    EquationStar,
    Equation,
    AlignStar,
    Align,
    AlignAtStar,
    AlignAt,
    GatherStar,
    Gather,
    MultiLineStar,
    MultiLine,
    FlAlignStar,
    FlAlign,
    Split,
    Math,
    DisplayMath,
}

#[derive(Serialize, Debug, Clone)]
pub struct MathEnvironmentName {
    pub pos: Pos,
    pub kind: MathEnvironmentNameKind,
}

impl_get_pos_simple!(MathEnvironmentName);

#[derive(Serialize, Debug, Clone)]
pub struct MathEnvironment {
    pub begin: BeginEnvironment,
    pub name: MathEnvironmentName,
    pub environment_comment: Option<SameLineComment>,
    pub body: Vec<MathToken>,
    pub end: EndEnvironment,
}

impl MathEnvironment {
    pub fn environment_comment_pos(&self) -> Pos {
        self.environment_comment
            .as_ref()
            .map_or_else(|| self.body_pos(), GetPos::pos)
    }
    pub fn body_pos(&self) -> Pos {
        self.body.first().map_or(self.end.pos(), GetPos::pos)
    }
}

impl GetPos for MathEnvironment {
    fn pos(&self) -> Pos {
        self.begin.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct Environment {
    pub begin: BeginEnvironment,
    pub name: CharTokens,
    pub body: Vec<Token>,
    pub end: EndEnvironment,
}

impl Environment {
    pub fn body_pos(&self) -> Pos {
        self.body.first().map_or(self.end.pos(), GetPos::pos)
    }
}

impl GetPos for Environment {
    fn pos(&self) -> Pos {
        self.begin.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct Number {
    pub pos: Pos,
    pub content: String,
}

impl_get_pos_simple!(Number);

#[derive(Serialize, Debug, Clone)]
pub struct Group {
    pub begin: BeginGroup,
    pub tokens: Vec<Token>,
    pub end: EndGroup,
}

impl Group {
    pub fn tokens_pos(&self) -> Pos {
        self.tokens.first().map_or(self.end.pos(), GetPos::pos)
    }
}

impl GetPos for Group {
    fn pos(&self) -> Pos {
        self.begin.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct MathGroup {
    pub begin: BeginGroup,
    pub tokens: Vec<MathToken>,
    pub end: EndGroup,
}

impl MathGroup {
    pub fn tokens_pos(&self) -> Pos {
        self.tokens.first().map_or(self.end.pos(), GetPos::pos)
    }
}

impl GetPos for MathGroup {
    fn pos(&self) -> Pos {
        self.begin.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct Whitespace {
    pub pos: Pos,
}

impl_get_pos_simple!(Whitespace);

declare_transparent_enum! {
    #[serde(tag = "comment_type")]
    #[access_fns_macro = full_comment_access_fns]
    enum FullComment {
        OwnLineComment(own_line_comment, own_line_comment_mut, into_own_line_comment),
        SameLineComment(same_line_comment, same_line_comment_mut, into_same_line_comment),
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct OwnLineComment {
    pub pos: Pos,
    pub leading_space: LeadingSpace,
    pub comment: Comment,
}

impl_get_pos_simple!(OwnLineComment);

#[derive(Serialize, Debug, Clone)]
pub struct SameLineComment {
    pub pos: Pos,
    pub leading_spaces: bool,
    pub comment: Comment,
}

impl_get_pos_simple!(SameLineComment);

#[derive(Serialize, Debug, Clone)]
pub struct LeadingSpace {
    pub pos: Pos,
    pub empty: bool,
}

impl_get_pos_simple!(LeadingSpace);

#[derive(Serialize, Debug, Clone)]
pub struct Comment {
    pub comment_start: CommentStart,
    pub content: String,
}

#[derive(Serialize, Debug, Clone)]
pub struct ParBreak {
    pub pos: Pos,
}

impl_get_pos_simple!(ParBreak);

#[derive(Serialize, Debug, Clone)]
pub struct MacroName {
    pub pos: Pos,
    pub content: String,
}

impl_get_pos_simple!(MacroName);

#[derive(Serialize, Debug, Clone)]
pub struct Macro {
    pub escape: Escape,
    pub name: MacroName,
}

impl GetPos for Macro {
    fn pos(&self) -> Pos {
        self.escape.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct BeginDisplayMath {
    pub escape: Escape,
}

impl GetPos for BeginDisplayMath {
    fn pos(&self) -> Pos {
        self.escape.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct EndDisplayMath {
    pub escape: Escape,
}

impl GetPos for EndDisplayMath {
    fn pos(&self) -> Pos {
        self.escape.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct BeginInlineMath {
    pub escape: Escape,
}

impl GetPos for BeginInlineMath {
    fn pos(&self) -> Pos {
        self.escape.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct EndInlineMath {
    pub escape: Escape,
}

impl GetPos for EndInlineMath {
    fn pos(&self) -> Pos {
        self.escape.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct BeginEnvironment {
    pub escape: Escape,
}

impl GetPos for BeginEnvironment {
    fn pos(&self) -> Pos {
        self.escape.pos()
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct EndEnvironment {
    pub escape: Escape,
}

impl GetPos for EndEnvironment {
    fn pos(&self) -> Pos {
        self.escape.pos()
    }
}
