// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::tex::ast;
use ast::Pos;
use peg::str::LineCol;
use peg::Parse;
use std::{error::Error, fmt};

#[derive(Debug)]
pub struct ParseError {
    pub file_name: String,
    pub line: usize,
    pub column: usize,
    pub byte_index: usize,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}: error: {}",
            self.file_name, self.line, self.column, self.message
        )
    }
}

impl Error for ParseError {}

pub type Result<T, E = ParseError> = std::result::Result<T, E>;

struct Parser<'a> {
    file_name: &'a str,
    input: &'a str,
}

macro_rules! err {
    ($this:expr, $pos:expr, $($fmt_args:tt)+) => {
        return Err($this.err_pos($pos, format!($($fmt_args)+)))
    };
}

macro_rules! err_if {
    ($cond:expr, $this:expr, $pos:expr, $($fmt_args:tt)+) => {
        if $cond {
            err!($this, $pos, $($fmt_args)+);
        }
    };
}

macro_rules! unwrap_or_continue {
    ($opt:expr) => {
        match $opt {
            Some(v) => v,
            None => continue,
        }
    };
}

impl<'a> Parser<'a> {
    fn new(file_name: &'a str, input: &'a str) -> Self {
        Self { file_name, input }
    }
    fn err_line_col(&self, line_col: LineCol, message: String) -> ParseError {
        ParseError {
            file_name: self.file_name.into(),
            line: line_col.line,
            column: line_col.column,
            byte_index: line_col.offset,
            message,
        }
    }
    fn err_pos(&self, pos: ast::Pos, message: String) -> ParseError {
        self.err_line_col(self.input.position_repr(pos.byte_index), message)
    }
    fn parse_instruction_set(&self, document: &ast::Document) -> Result<InstructionSet> {
        for token in document
            .content
            .iter()
            .find_map(ast::Token::environment)
            .into_iter()
            .flat_map(|v| &v.body)
        {
            todo!();
            unwrap_or_continue!(token.environment());
            if let ast::Token::SpecialMacro(ast::SpecialMacro::Environment(env)) = token {
                err_if!(
                    env.name.content != "table",
                    self,
                    env.name.pos,
                    "expected `table`"
                );
                for token in &env.body {
                    if let ast::Token::SpecialMacro(ast::SpecialMacro::Environment(env)) = token {
                        err_if!(env.name.content != "small", self, env.name.pos, "");
                        dbg!(env);
                        todo!()
                    }
                }
            }
        }
        err!(self, ast::Pos { byte_index: 0 }, "no environment found")
    }
}

#[derive(Debug, Clone)]
pub struct InstructionSet {}

pub fn parse(file_name: &str, input: &str) -> Result<InstructionSet> {
    let parser = Parser::new(file_name, input);
    let document = crate::tex::parse(input)
        .map_err(|e| parser.err_line_col(e.location, format!("expected: {}", e.expected)))?;
    parser.parse_instruction_set(&document)
}
