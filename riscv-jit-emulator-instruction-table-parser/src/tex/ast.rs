// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use serde::Serialize;
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Pos {
    pub byte_index: usize,
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

#[derive(Serialize, Debug, Clone)]
pub struct BeginGroup {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct EndGroup {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct AlignmentTab {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct MacroParameter {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct CommentStart {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct MathShift {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct Superscript {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct Subscript {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct Ignore {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct AnyChar {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct Punctuation {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct Space {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct AsciiAlphabetic {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct AsciiDigit {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct NewLine {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct CharToken {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct CharTokens {
    pub pos: Pos,
    pub content: String,
}

#[derive(Serialize, Debug, Clone)]
pub struct Document {
    pub content: Vec<Token>,
}

macro_rules! declare_transparent_enum {
    (
        #[serde(tag = $tag:literal)]
        enum $name:ident {
            $($alternative:ident,)+
        }
    ) => {
        #[derive(Serialize, Clone)]
        #[serde(tag = $tag)]
        pub enum $name {
            $($alternative($alternative),)+
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
    #[serde(tag = "token_type")]
    enum Token {
        SpecialMacro,
        Macro,
        FullComment,
        Group,
        InlineMath,
        AlignmentTab,
        ParBreak,
        MacroParameter,
        Ignore,
        Number,
        Whitespace,
        Punctuation,
        CharTokens,
        BeginGroup,
        EndGroup,
        MathShift,
    }
}

impl Token {
    pub fn environment(&self) -> Option<&Environment> {
        if let Self::SpecialMacro(SpecialMacro::Environment(env)) = self {
            Some(env)
        } else {
            None
        }
    }
}

declare_transparent_enum! {
    #[serde(tag = "math_token_type")]
    enum MathToken {
        SpecialMacro,
        Macro,
        FullComment,
        MathGroup,
        AlignmentTab,
        MacroParameter,
        Superscript,
        Subscript,
        Ignore,
        Whitespace,
        AnyChar,
    }
}

declare_transparent_enum! {
    #[serde(tag = "special_macro_type")]
    enum SpecialMacro {
        Verb,
        VerbatimEnvironment,
        DisplayMath,
        InlineMath,
        MathEnvironment,
        Environment,
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct Verb {
    pub escape: Escape,
    pub env: String,
    pub delimiter: char,
    pub content: String,
}

#[derive(Serialize, Debug, Clone)]
pub struct InlineMath {
    pub begin: BeginInlineMath,
    pub content: Vec<MathToken>,
    pub end: EndInlineMath,
}

#[derive(Serialize, Debug, Clone)]
pub struct VerbatimEnvironment {
    pub begin: BeginEnvironment,
    pub name: VerbatimEnvironmentName,
    pub body: String,
    pub end: EndEnvironment,
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

#[derive(Serialize, Debug, Clone)]
pub struct DisplayMath {
    pub pos: Pos,
    pub content: Vec<MathToken>,
}

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

#[derive(Serialize, Debug, Clone)]
pub struct MathEnvironment {
    pub begin: BeginEnvironment,
    pub name: MathEnvironmentName,
    pub environment_comment: Option<SameLineComment>,
    pub body: Vec<MathToken>,
    pub end: EndEnvironment,
}

#[derive(Serialize, Debug, Clone)]
pub struct Environment {
    pub begin: BeginEnvironment,
    pub name: CharTokens,
    pub body: Vec<Token>,
    pub end: EndEnvironment,
}

#[derive(Serialize, Debug, Clone)]
pub struct Number {
    pub pos: Pos,
    pub content: String,
}

#[derive(Serialize, Debug, Clone)]
pub struct Group {
    pub begin: BeginGroup,
    pub tokens: Vec<Token>,
    pub end: EndGroup,
}

#[derive(Serialize, Debug, Clone)]
pub struct MathGroup {
    pub begin: BeginGroup,
    pub tokens: Vec<MathToken>,
    pub end: EndGroup,
}

#[derive(Serialize, Debug, Clone)]
pub struct Whitespace {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
#[serde(tag = "comment_type")]
pub enum FullComment {
    OwnLineComment(OwnLineComment),
    SameLineComment(SameLineComment),
}

#[derive(Serialize, Debug, Clone)]
pub struct OwnLineComment {
    pub pos: Pos,
    pub leading_space: LeadingSpace,
    pub comment: Comment,
}

#[derive(Serialize, Debug, Clone)]
pub struct SameLineComment {
    pub pos: Pos,
    pub leading_spaces: bool,
    pub comment: Comment,
}

#[derive(Serialize, Debug, Clone)]
pub struct LeadingSpace {
    pub pos: Pos,
    pub empty: bool,
}

#[derive(Serialize, Debug, Clone)]
pub struct Comment {
    pub comment_start: CommentStart,
    pub content: String,
}

#[derive(Serialize, Debug, Clone)]
pub struct ParBreak {
    pub pos: Pos,
}

#[derive(Serialize, Debug, Clone)]
pub struct MacroName {
    pub pos: Pos,
    pub content: String,
}

#[derive(Serialize, Debug, Clone)]
pub struct Macro {
    pub escape: Escape,
    pub name: MacroName,
}

#[derive(Serialize, Debug, Clone)]
pub struct BeginDisplayMath {
    pub escape: Escape,
}

#[derive(Serialize, Debug, Clone)]
pub struct EndDisplayMath {
    pub escape: Escape,
}

#[derive(Serialize, Debug, Clone)]
pub struct BeginInlineMath {
    pub escape: Escape,
}

#[derive(Serialize, Debug, Clone)]
pub struct EndInlineMath {
    pub escape: Escape,
}

#[derive(Serialize, Debug, Clone)]
pub struct BeginEnvironment {
    pub escape: Escape,
}

#[derive(Serialize, Debug, Clone)]
pub struct EndEnvironment {
    pub escape: Escape,
}
