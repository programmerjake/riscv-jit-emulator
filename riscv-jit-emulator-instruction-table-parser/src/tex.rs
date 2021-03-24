// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information
use peg::{error::ParseError, str::LineCol};

pub mod ast;

trait IsStartOfLine {
    fn is_start_of_line(&self, index: usize) -> peg::RuleResult<()>;
}

impl IsStartOfLine for str {
    fn is_start_of_line(&self, index: usize) -> peg::RuleResult<()> {
        if index == 0 {
            return peg::RuleResult::Matched(index, ());
        }
        match self.as_bytes().get(index - 1) {
            Some(b'\r') | Some(b'\n') => peg::RuleResult::Matched(index, ()),
            _ => peg::RuleResult::Failed,
        }
    }
}

peg::parser! {
    // based on:
    // https://github.com/siefkenj/latex-parser/blob/96f9bfe405008a0fd1da51dbba476e0353675090/src/grammars/latex.pegjs
    grammar parser() for str {
        use crate::tex::ast::*;

        pub(crate) rule document() -> Document =
            content:(token()*) { Document { content } }

        rule token() -> Token =
            t:special_macro() { Token::SpecialMacro(t) }
            / t:macro_() { Token::Macro(t) }
            / t:full_comment() { Token::FullComment(t) }
            / t:group() { Token::Group(t) }
            / t:dollar_inline_math() { Token::DollarInlineMath(t) }
            / t:alignment_tab() { Token::AlignmentTab(t) }
            / t:par_break() { Token::ParBreak(t) }
            / t:macro_parameter() { Token::MacroParameter(t) }
            / t:ignore() { Token::Ignore(t) }
            / t:number() { Token::Number(t) }
            / t:whitespace() { Token::Whitespace(t) }
            / t:punctuation() { Token::Punctuation(t) }
            / t:char_tokens() { Token::CharTokens(t) }
            / t:begin_group() { Token::BeginGroup(t) }
            / t:end_group() { Token::EndGroup(t) }
            / t:math_shift() { Token::MathShift(t) }

        rule par_break() -> ParBreak =
            pos:pos() (space()* new_line())*<2,>
                // Comments eat the whitespace in front of them, so if a
                // par_break is followed by a comment, we don't want to eat that
                // whitespace.
                (space()* !comment_start())?
                { ParBreak { pos } }

        rule math_token() -> MathToken =
            t:special_macro() { MathToken::SpecialMacro(t) }
            / t:macro_() { MathToken::Macro(t) }
            / t:full_comment() { MathToken::FullComment(t) }
            / whitespace()* t:math_group() whitespace()* { MathToken::MathGroup(t) }
            / whitespace()* t:alignment_tab() whitespace()* { MathToken::AlignmentTab(t) }
            / whitespace()* t:macro_parameter() whitespace()* { MathToken::MacroParameter(t) }
            / whitespace()* t:superscript() whitespace()* { MathToken::Superscript(t) }
            / whitespace()* t:subscript() whitespace()* { MathToken::Subscript(t) }
            / t:ignore() { MathToken::Ignore(t) }
            / t:whitespace() { MathToken::Whitespace(t) }
            / t:number() { MathToken::Number(t) }
            / t:any_char() { MathToken::AnyChar(t) }

        rule char_tokens() -> CharTokens =
            pos:pos() content:$(char_token()+) {CharTokens { pos, content: content.into() }}

        rule char_token() -> CharToken =
            pos:pos()
            !(
                escape()
                / comment_start()
                / begin_group()
                / end_group()
                / math_shift()
                / alignment_tab()
                / new_line()
                / macro_parameter()
                / ignore()
                / space()
                / punctuation()
            )
            any_char() { CharToken { pos } }

        rule whitespace() -> Whitespace =
            pos:pos() (
                new_line() space()*
                / space()+ new_line() !comment_start() space()* !new_line()
                / space()+
            ) { Whitespace { pos } }

        rule number() -> Number =
            pos:pos() content:$(
                digit()+ ("." digit()*)?
                / "." digit()+
            ) { Number { pos, content: content.into() } }

        rule special_macro() -> SpecialMacro =
            v:verb() { SpecialMacro::Verb(v) }
            / v:verbatim_environment() { SpecialMacro::VerbatimEnvironment(v) }
            / v:display_math() { SpecialMacro::DisplayMath(v) }
            / v:parenthesized_inline_math() { SpecialMacro::ParenthesizedInlineMath(v) }
            / v:math_environment() { SpecialMacro::MathEnvironment(v) }
            / v:environment() { SpecialMacro::Environment(v) }

        rule verb() -> Verb =
            escape:escape()
            env:$("verb*" / "verb")
            delimiter:$([_])
            content:$(
                (
                    ch:$([_])
                    {?
                        if ch == delimiter {
                            Err("")
                        } else {
                            Ok(())
                        }
                    }
                )*
            )
            [_] {
                Verb {
                    escape,
                    env: env.into(),
                    delimiter: delimiter.chars().next().unwrap(),
                    content: content.into(),
                }
            }


        rule verbatim_environment() -> VerbatimEnvironment =
            begin:begin_environment()
            begin_group()
            name:verbatim_environment_name()
            end_group()
            body:$(
                (
                    !(
                        end_environment()
                        begin_group()
                        end_name:verbatim_environment_name()
                        end_group()
                        {?
                            if name.kind == end_name.kind {
                                Ok(())
                            } else {
                                Err("")
                            }
                        }
                    )
                    [_]
                )*
            )
            end:end_environment()
            begin_group()
            verbatim_environment_name()
            end_group()
            {
                VerbatimEnvironment {
                    begin,
                    name,
                    body: body.into(),
                    end,
                }
            }

        rule verbatim_environment_name() -> VerbatimEnvironmentName =
            // standard verbatim enviroments. `verbatim*` must be listed first
            pos:pos() kind:(
                "verbatim*" { VerbatimEnvironmentNameKind::VerbatimStar }
                / "verbatim" { VerbatimEnvironmentNameKind::Verbatim }
                / "filecontents*" { VerbatimEnvironmentNameKind::FileContentsStar }
                / "filecontents" { VerbatimEnvironmentNameKind::FileContents }
                // comment environment provided by \usepackage{verbatim}
                / "comment" { VerbatimEnvironmentNameKind::Comment }
                // lstlisting environment provided by \usepackage{listings}
                / "lstlisting" { VerbatimEnvironmentNameKind::ListListing }
            ) { VerbatimEnvironmentName { pos, kind } }

        rule display_math() -> DisplayMath =
            // display math with \[...\]
            pos:pos()
            begin_display_math()
            content:(!end_display_math() t:math_token() { t })*
            end_display_math() { DisplayMath { pos, content } }
            // display math with $$...$$
            / pos:pos()
            math_shift()
            math_shift()
            content:(!(math_shift() math_shift()) t:math_token() { t })*
            math_shift()
            math_shift() { DisplayMath { pos, content } }

        rule parenthesized_inline_math() -> ParenthesizedInlineMath =
            // inline math with \(...\)
            begin:begin_inline_math()
            content:(!end_inline_math() t:math_token() { t })*
            end:end_inline_math() { ParenthesizedInlineMath { begin, content, end } }

        rule dollar_inline_math() -> DollarInlineMath =
            begin:math_shift()
            content:(!math_shift() t:math_token() { t })+
            end:math_shift() { DollarInlineMath { begin, content, end } }

        rule macro_() -> Macro =
            escape:escape() name:macro_name() { Macro { escape, name } }

        rule macro_name() -> MacroName =
            pos:pos() content:$(letter()+ / [_]) { MacroName { pos, content: content.into() } }

        rule group() -> Group =
            begin:begin_group()
            tokens:(!end_group() t:token() { t })*
            end:end_group() { Group { begin, tokens, end } }

        rule environment() -> Environment =
            begin:begin_environment()
            begin_group()
            name:char_tokens()
            end_group()
            body:(
                !(
                    end_environment()
                    begin_group()
                    end_name:char_tokens()
                    end_group()
                    {?
                        if name.content == end_name.content {
                            Ok(())
                        } else {
                            Err("")
                        }
                    }
                )
                t:token() { t }
            )*
            end:end_environment()
            begin_group()
            char_tokens()
            end_group() {
                Environment {
                    begin,
                    name,
                    body,
                    end,
                }
            }

        rule math_environment() -> MathEnvironment =
            begin:begin_environment()
            begin_group()
            name:math_environment_name()
            end_group()
            environment_comment:same_line_comment()?
            body:(
                !(
                    end_environment()
                    begin_group()
                    end_name:math_environment_name()
                    end_group()
                    {?
                        if name.kind == end_name.kind {
                            Ok(())
                        } else {
                            Err("")
                        }
                    }
                )
                t:math_token() { t }
            )*
            end:end_environment()
            begin_group()
            math_environment_name()
            end_group() {
                MathEnvironment {
                    begin,
                    name,
                    environment_comment,
                    body,
                    end,
                }
            }

        // group that assumes you're in math mode.  If you use "\text{}" this isn't a good idea....
        rule math_group() -> MathGroup =
            begin:begin_group()
            tokens:(!end_group() t:math_token() { t })*
            end:end_group() { MathGroup { begin, tokens, end } }

        rule begin_display_math() -> BeginDisplayMath =
            escape:escape() "[" { BeginDisplayMath { escape } }

        rule end_display_math() -> EndDisplayMath =
            escape:escape() "]" { EndDisplayMath { escape } }

        rule begin_inline_math() -> BeginInlineMath =
            escape:escape() "(" { BeginInlineMath { escape } }

        rule end_inline_math() -> EndInlineMath =
            escape:escape() ")" { EndInlineMath { escape } }

        rule begin_environment() -> BeginEnvironment =
            escape:escape() "begin" { BeginEnvironment { escape } }

        rule end_environment() -> EndEnvironment =
            escape:escape() "end" { EndEnvironment { escape } }

        rule math_environment_name() -> MathEnvironmentName =
            pos:pos() kind:(
                "equation*" { MathEnvironmentNameKind::EquationStar }
                / "equation" { MathEnvironmentNameKind::Equation }
                / "align*" { MathEnvironmentNameKind::AlignStar }
                / "align" { MathEnvironmentNameKind::Align }
                / "alignat*" { MathEnvironmentNameKind::AlignAtStar }
                / "alignat" { MathEnvironmentNameKind::AlignAt }
                / "gather*" { MathEnvironmentNameKind::GatherStar }
                / "gather" { MathEnvironmentNameKind::Gather }
                / "multline*" { MathEnvironmentNameKind::MultiLineStar }
                / "multline" { MathEnvironmentNameKind::MultiLine }
                / "flalign*" { MathEnvironmentNameKind::FlAlignStar }
                / "flalign" { MathEnvironmentNameKind::FlAlign }
                / "split" { MathEnvironmentNameKind::Split }
                / "math" { MathEnvironmentNameKind::Math }
                / "displaymath" { MathEnvironmentNameKind::DisplayMath }
            ) { MathEnvironmentName { pos, kind } }

        // catcode 0
        rule escape() -> Escape =
            pos:pos() "\\" { Escape { pos } }

        // catcode 1
        rule begin_group() -> BeginGroup =
            pos:pos() "{" { BeginGroup { pos } }

        // catcode 2
        rule end_group() -> EndGroup =
            pos:pos() "}" { EndGroup { pos } }

        // catcode 3
        rule math_shift() -> MathShift =
            pos:pos() "$" { MathShift { pos } }

        // catcode 4
        rule alignment_tab() -> AlignmentTab =
            pos:pos() "&" { AlignmentTab { pos } }

        // catcode 5 (linux, os x, windows)
        rule new_line() -> NewLine =
            pos:pos() ("\r\n" / ['\r' | '\n']) { NewLine { pos } }

        // catcode 6
        rule macro_parameter() -> MacroParameter =
            pos:pos() "#" { MacroParameter { pos } }

        // catcode 7
        rule superscript() -> Superscript =
            pos:pos() "^" { Superscript { pos } }

        // catcode 8
        rule subscript() -> Subscript =
            pos:pos() "_" { Subscript { pos } }

        // catcode 9
        rule ignore() -> Ignore =
            pos:pos() "\0" { Ignore { pos } }

        // catcode 10
        rule space() -> Space =
            pos:pos() [' ' | '\t']+ { Space { pos } }

        // catcode 11
        rule letter() -> AsciiAlphabetic =
            pos:pos() ['a'..='z' | 'A'..='Z'] { AsciiAlphabetic { pos } }

        // catcode 12 (other)
        rule digit() -> AsciiDigit =
            pos:pos() ['0'..='9'] { AsciiDigit { pos } }

        // catcode 12
        rule punctuation() -> Punctuation =
            pos:pos() ch:$([
                '.' | ',' | ';' | ':' | '-' | '*' | '/' | '(' | ')' | '!'
                | '?' | '=' | '+' | '<' | '>' | '[' | ']'
            ]) { Punctuation { pos, ch: ch.chars().next().unwrap() } }

        // catcode 14, including the newline
        rule comment_start() -> CommentStart =
            pos:pos() "%" { CommentStart { pos } }

        // A comment consumes any whitespace that comes before it.
        // It can be the only thing on a line, or can come at the end of a line.
        // A comment will consume the newline that follows it, unless that newline
        // is part of a par_break.
        rule full_comment() -> FullComment =
            c:own_line_comment() { FullComment::OwnLineComment(c) }
            / c:same_line_comment() { FullComment::SameLineComment(c) }

        // A comment that appears on a line of its own
        rule own_line_comment() -> OwnLineComment =
            // `leading_space()` is whitespace that starts at the beginning fo a line.
            // A comment is `sameline` if it is on the same line as other content.
            // The existance of leading whitespace for a `sameline == false` comment
            // isn't important, but we record it anyways.
            //
            // We look for `(space() new_line())?` at the start so that we eat excess whitespace that occurs before
            // a comment on a new line. Otherwise, the newline itself is counted as whitespace. For example:
            // ```x
            //    %comment```
            // would be parsed as "x, <whitespace (from the newline)>, comment". We don't want this. We want
            // to parse it as "x, comment".
            pos:pos() (space()* new_line())? leading_space:leading_space() comment:comment() {
                OwnLineComment {
                    pos,
                    leading_space,
                    comment,
                }
            }

        // A comment that appears at the end of a line
        rule same_line_comment() -> SameLineComment =
            pos:pos() leading_spaces:space()* comment:comment() {
                SameLineComment {
                    pos,
                    leading_spaces: !leading_spaces.is_empty(),
                    comment,
                }
            }

        rule comment() -> Comment =
            // A comment normally consumes the next newline and all leading whitespace.
            // The exception is if the next line consists solely of a comment. In that case,
            // consume the newline but leave the whitespace (`full_comment` will eat the
            // leading whitspace)
            comment_start:comment_start()
            content:$((!new_line() [_])*)
            (
                &par_break() // par_breaks following a comment are preserved
                // if a comment is not followed by a par_break, the newline is consumed
                / new_line() space()* !comment_start()
                / new_line()
                / ![_]
            )
            { Comment { comment_start, content: content.into() } }

        // Whitespace at the start of a line only
        rule leading_space() -> LeadingSpace =
            pos:pos()
            start_of_line()
            content:$(space()*)
            { LeadingSpace { pos, empty: content.is_empty() } }

        rule start_of_line() =
            ##is_start_of_line()

        rule any_char() -> AnyChar =
            pos:pos() ch:$([_]) { AnyChar { pos, ch: ch.chars().next().unwrap() } }

        rule pos() -> Pos =
            p:position!() { Pos::new(p) }
    }
}

pub fn parse(input: &str) -> Result<ast::Document, ParseError<LineCol>> {
    parser::document(input)
}

#[cfg(test)]
mod test {
    use super::*;
    use serde::Serialize;
    use serde_json::{
        ser::{PrettyFormatter, Serializer},
        *,
    };
    use std::str;

    fn test_parse(input: &str, json: serde_json::Value) {
        let document = parse(input).unwrap();
        let actual_json = serde_json::to_value(&document).unwrap();
        let mut text = Vec::new();
        document
            .serialize(&mut Serializer::with_formatter(
                &mut text,
                PrettyFormatter::with_indent(b"    "),
            ))
            .unwrap();
        println!("{}", str::from_utf8(&text).unwrap());
        assert!(actual_json == json);
    }

    #[test]
    fn test_parse_macro() {
        test_parse(
            r#"\abc"#,
            json! {
                {
                    "content": [
                        {
                            "token_type": "Macro",
                            "escape": {
                                "pos": "@0"
                            },
                            "name": {
                                "pos": "@1",
                                "content": "abc"
                            }
                        }
                    ]
                }
            },
        );
    }

    #[test]
    fn test_parse_environment() {
        test_parse(
            r#"\begin{env}contents\begin{env2}contents2\end{env2}a\end{env}"#,
            json! {
                {
                    "content": [
                        {
                            "token_type": "SpecialMacro",
                            "special_macro_type": "Environment",
                            "begin": {
                                "escape": {
                                    "pos": "@0"
                                }
                            },
                            "name": {
                                "pos": "@7",
                                "content": "env"
                            },
                            "body": [
                                {
                                    "token_type": "CharTokens",
                                    "pos": "@11",
                                    "content": "contents"
                                },
                                {
                                    "token_type": "SpecialMacro",
                                    "special_macro_type": "Environment",
                                    "begin": {
                                        "escape": {
                                            "pos": "@19"
                                        }
                                    },
                                    "name": {
                                        "pos": "@26",
                                        "content": "env2"
                                    },
                                    "body": [
                                        {
                                            "token_type": "CharTokens",
                                            "pos": "@31",
                                            "content": "contents2"
                                        }
                                    ],
                                    "end": {
                                        "escape": {
                                            "pos": "@40"
                                        }
                                    }
                                },
                                {
                                    "token_type": "CharTokens",
                                    "pos": "@50",
                                    "content": "a"
                                }
                            ],
                            "end": {
                                "escape": {
                                    "pos": "@51"
                                }
                            }
                        }
                    ]
                }
            },
        );
    }
}
