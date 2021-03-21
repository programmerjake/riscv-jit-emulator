// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

// based on:
// https://github.com/siefkenj/latex-parser/blob/96f9bfe405008a0fd1da51dbba476e0353675090/src/grammars/latex.pegjs

use crate::{Parse, ParseAlternatives, Parser, Span, UnresolvedParseError};
use std::fmt;

macro_rules! declare_literal {
    ($name:ident($value:literal)) => {
        #[derive(Debug)]
        pub struct $name<'a> {
            pub span: Span<'a>,
        }

        impl<'a> $name<'a> {
            pub const VALUE: &'static str = $value;
        }

        impl<'a> Parse<'a> for $name<'a> {
            fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
                parser
                    .span(|parser| {
                        if parser.remaining_text().starts_with(Self::VALUE) {
                            parser.advance(Self::VALUE.len());
                            Ok(())
                        } else {
                            Err(parser.error_str(concat!("expected `", $value, "`")))
                        }
                    })
                    .map(|((), span)| $name { span })
            }
        }
    };
}

declare_literal!(Escape("\\"));
declare_literal!(BeginGroup("{"));
declare_literal!(EndGroup("}"));
declare_literal!(AlignmentTab("&"));
declare_literal!(MacroParameter("#"));
declare_literal!(CommentStart("%"));
declare_literal!(MathShift("$"));
declare_literal!(Superscript("^"));
declare_literal!(Subscript("_"));
declare_literal!(Ignore("\0"));

fn parse_char_category_helper<'a, R>(
    parser: &mut Parser<'a>,
    make_retval: impl FnOnce(Span<'a>) -> R,
    matches: impl FnOnce(char) -> bool,
    error_message: &'static str,
) -> Result<R, UnresolvedParseError<'a>> {
    parser
        .span(|parser| {
            let remaining_text = parser.remaining_text();
            let mut chars = remaining_text.chars();
            match chars.next() {
                Some(ch) if matches(ch) => {
                    parser.advance(remaining_text.len() - chars.as_str().len());
                    Ok(())
                }
                _ => Err(parser.error_str(error_message)),
            }
        })
        .map(|((), span)| make_retval(span))
}

macro_rules! declare_char_category {
    ($name:ident($($pat:pat)|* $(if $cond:expr)?)) => {
        #[derive(Debug)]
        pub struct $name<'a> {
            pub span: Span<'a>,
        }

        impl<'a> $name<'a> {
            pub fn matches(ch: char) -> bool {
                match ch {
                    $($pat)|* $(if $cond)? => true,
                    _ => false,
                }
            }
        }

        impl<'a> Parse<'a> for $name<'a> {
            fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
                parse_char_category_helper(
                    parser,
                    |span| Self { span },
                    Self::matches,
                    concat!("expected <", stringify!($name), ">"),
                )
            }
        }
    };
}

declare_char_category!(AnyChar(_));
declare_char_category!(Punctuation(
    '.' | ','
        | ';'
        | ':'
        | '-'
        | '*'
        | '/'
        | '('
        | ')'
        | '!'
        | '?'
        | '='
        | '+'
        | '<'
        | '>'
        | '['
        | ']'
));
declare_char_category!(Space(' ' | '\t'));
declare_char_category!(AsciiAlphabetic(ch if ch.is_ascii_alphabetic()));
declare_char_category!(AsciiDigit(ch if ch.is_ascii_digit()));

#[derive(Debug)]
pub struct NewLine<'a> {
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for NewLine<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        parser
            .span(|parser| {
                const CRLF: &str = "\r\n";
                const CR: &str = "\r";
                const LF: &str = "\n";
                if parser.remaining_text().starts_with(CRLF) {
                    parser.advance(CRLF.len());
                    Ok(())
                } else if parser.remaining_text().starts_with(CR) {
                    parser.advance(CR.len());
                    Ok(())
                } else if parser.remaining_text().starts_with(LF) {
                    parser.advance(LF.len());
                    Ok(())
                } else {
                    Err(parser.error_str("expected new-line"))
                }
            })
            .map(|((), span)| NewLine { span })
    }
}

#[derive(Debug)]
pub struct CharToken<'a> {
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for CharToken<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        let failed = parser.peek::<Escape>()
            || parser.peek::<CommentStart>()
            || parser.peek::<BeginGroup>()
            || parser.peek::<EndGroup>()
            || parser.peek::<MathShift>()
            || parser.peek::<AlignmentTab>()
            || parser.peek::<NewLine>()
            || parser.peek::<MacroParameter>()
            || parser.peek::<Ignore>()
            || parser.peek::<Space>()
            || parser.peek::<Punctuation>();
        if failed {
            Err(parser.error_str("expected char-token"))
        } else {
            let AnyChar { span } = parser.parse()?;
            Ok(CharToken { span })
        }
    }
}

#[derive(Debug)]
pub struct CharTokens<'a> {
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for CharTokens<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        parser
            .span(|parser| {
                parser.parse::<CharToken>()?;
                parser.parse_many0_with(|_: CharToken| {});
                Ok(())
            })
            .map(|((), span)| CharTokens { span })
    }
}

#[derive(Debug)]
pub struct Document<'a> {
    pub content: Vec<Token<'a>>,
}

impl<'a> Parse<'a> for Document<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        let (content, error) = parser.parse_many0_with_err();
        if parser.remaining_text().is_empty() {
            Ok(Self { content })
        } else {
            Err(error)
        }
    }
}

macro_rules! declare_token {
    ($($alternative:ident,)+) => {
        pub enum Token<'a> {
            $($alternative($alternative<'a>),)+
        }

        impl fmt::Debug for Token<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(Self::$alternative(v) => fmt::Debug::fmt(v, f),)+
                }
            }
        }

        impl<'a> Parse<'a> for Token<'a> {
            fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
                parser.parse_alternatives((
                    $(|parser: &mut Parser<'a>| parser.parse().map(Self::$alternative),)+
                ))
            }
        }
    };
}

declare_token! {
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

macro_rules! todo_parser {
    ($name:ident) => {
        #[derive(Debug)]
        pub struct $name<'a> {
            pub span: Span<'a>,
        }

        impl<'a> Parse<'a> for $name<'a> {
            fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
                Err(parser.error_str(concat!(stringify!($name), " is not implemented")))
            }
        }
    };
}

todo_parser!(SpecialMacro);
todo_parser!(InlineMath);
todo_parser!(Number);

#[derive(Debug)]
pub struct Group<'a> {
    pub begin_group: BeginGroup<'a>,
    pub tokens: Vec<Token<'a>>,
    pub end_group: EndGroup<'a>,
}

impl<'a> Parse<'a> for Group<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        Ok(Self {
            begin_group: parser.parse()?,
            tokens: {
                let mut tokens = Vec::new();
                parser.parse_many0_with_fn(|parser| {
                    if parser.peek::<EndGroup>() {
                        Err(parser.error_str("`}` not allowed here"))
                    } else {
                        tokens.push(parser.parse()?);
                        Ok(())
                    }
                });
                tokens
            },
            end_group: parser.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Whitespace<'a> {
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for Whitespace<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        parser
            .span(|parser| {
                if parser.peek::<NewLine>() {
                    parser.parse::<NewLine>()?;
                } else {
                    parser.parse::<Space>()?;
                    parser.parse_many0_with(|_: Space| {});
                    parser.parse_opt(|parser| {
                        parser.parse::<NewLine>()?;
                        if !parser.peek::<CommentStart>() {
                            parser.parse_many0_with(|_: Space| {});
                            if !parser.peek::<NewLine>() {
                                return Ok(());
                            }
                        }
                        Err(parser.error_str(""))
                    });
                }
                Ok(())
            })
            .map(|((), span)| Whitespace { span })
    }
}

#[derive(Debug)]
pub enum FullComment<'a> {
    OwnLineComment(OwnLineComment<'a>),
    SameLineComment(SameLineComment<'a>),
}

impl<'a> Parse<'a> for FullComment<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        parser.parse_alternatives((
            |parser: &mut Parser<'a>| parser.parse().map(FullComment::OwnLineComment),
            |parser: &mut Parser<'a>| parser.parse().map(FullComment::SameLineComment),
        ))
    }
}
#[derive(Debug)]
pub struct OwnLineComment<'a> {
    pub span: Span<'a>,
    pub leading_space: LeadingSpace<'a>,
    pub comment: Comment<'a>,
}

impl<'a> Parse<'a> for OwnLineComment<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        parser
            .span(|parser| {
                parser.parse_opt(|parser| {
                    parser.parse_many0_with(|_: Space| {});
                    parser.parse::<NewLine>()
                });
                let leading_space = parser.parse()?;
                let comment = parser.parse()?;
                Ok((leading_space, comment))
            })
            .map(|((leading_space, comment), span)| OwnLineComment {
                span,
                leading_space,
                comment,
            })
    }
}

#[derive(Debug)]
pub struct SameLineComment<'a> {
    pub span: Span<'a>,
    pub leading_spaces: Span<'a>,
    pub comment: Comment<'a>,
}

impl<'a> Parse<'a> for SameLineComment<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        parser
            .span(|parser| {
                let ((), leading_spaces) = parser.span(|parser| {
                    parser.parse_many0_with(|_: Space| {});
                    Ok(())
                })?;
                let comment = parser.parse()?;
                Ok((leading_spaces, comment))
            })
            .map(|((leading_spaces, comment), span)| SameLineComment {
                span,
                leading_spaces,
                comment,
            })
    }
}

#[derive(Debug)]
pub struct LeadingSpace<'a> {
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for LeadingSpace<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        parser
            .span(|parser| {
                if parser.preceding_text().is_empty()
                    || parser
                        .preceding_text()
                        .ends_with(|ch| ch == '\n' || ch == '\r')
                {
                    parser.parse_many0_with(|_: Space| {});
                    Ok(())
                } else {
                    Err(parser.error_str("Must be at the start of a line"))
                }
            })
            .map(|((), span)| LeadingSpace { span })
    }
}

#[derive(Debug)]
pub struct Comment<'a> {
    pub span: Span<'a>,
    pub comment_start: CommentStart<'a>,
    pub content: Span<'a>,
}

impl<'a> Parse<'a> for Comment<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        parser
            .span(|parser| {
                let comment_start = parser.parse()?;
                let ((), content) = parser.span(|parser| {
                    parser.parse_many0_with_fn(|parser| {
                        if parser.peek::<NewLine>() {
                            Err(parser.error_str("new line not allowed here"))
                        } else {
                            parser.parse::<AnyChar>()?;
                            Ok(())
                        }
                    });
                    if !parser.peek::<ParBreak>() {
                        parser.parse_alternatives((
                            |parser: &mut Parser<'a>| {
                                parser.parse::<NewLine>()?;
                                parser.parse_many0_with(|_: Space| {});
                                if parser.peek::<NewLine>() {
                                    Err(parser.error_str("new line not allowed here"))
                                } else {
                                    Ok(())
                                }
                            },
                            |parser: &mut Parser<'a>| {
                                if !parser.remaining_text().is_empty() {
                                    parser.parse::<NewLine>()?;
                                }
                                Ok(())
                            },
                        ))?;
                    }
                    Ok(())
                })?;
                Ok((comment_start, content))
            })
            .map(|((comment_start, content), span)| Comment {
                span,
                comment_start,
                content,
            })
    }
}

#[derive(Debug)]
pub struct ParBreak<'a> {
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for ParBreak<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        parser
            .span(|parser| {
                parser.parse_many0_with(|_: Space| {});
                parser.parse::<NewLine>()?;
                parser.parse_many0_with_fn(|parser| {
                    let mut parsed_anything = false;
                    parser.parse_many0_with(|_: Space| {
                        parsed_anything = true;
                    });
                    if parser.peek::<CommentStart>() {
                        Err(parser.error_str("comment not allowed here"))
                    } else if parsed_anything {
                        parser.parse_opt(|parser| parser.parse::<NewLine>());
                        Ok(())
                    } else {
                        parser.parse::<NewLine>()?;
                        Ok(())
                    }
                });
                Ok(())
            })
            .map(|((), span)| ParBreak { span })
    }
}

#[derive(Debug)]
pub struct MacroName<'a> {
    pub span: Span<'a>,
}

impl<'a> Parse<'a> for MacroName<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        parser
            .span(|parser| {
                if parser.peek::<AsciiAlphabetic>() {
                    parser.parse_many0_with(|_: AsciiAlphabetic| {});
                } else {
                    parser.parse::<AnyChar>()?;
                }
                Ok(())
            })
            .map(|((), span)| MacroName { span })
    }
}

#[derive(Debug)]
pub struct Macro<'a> {
    pub escape: Escape<'a>,
    pub name: MacroName<'a>,
}

impl<'a> Parse<'a> for Macro<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<Self, UnresolvedParseError<'a>> {
        Ok(Macro {
            escape: parser.parse()?,
            name: parser.parse()?,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{ParseError, SourceCode};

    fn parse_fully<'a, T: Parse<'a>>(source_code: &'a SourceCode<'a>) -> Result<T, ParseError> {
        let mut parser = Parser::new(source_code);
        let retval = parser.parse()?;
        if parser.remaining_text().is_empty() {
            Ok(retval)
        } else {
            Err(parser.error_str("extra text before end of file").into())
        }
    }

    #[test]
    fn test_parse() {
        dbg!(parse_fully::<Document>(&SourceCode::new(
            "test_file",
            r#"\abc"#
        )))
        .unwrap();
    }
}
