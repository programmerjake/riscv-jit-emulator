// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::tex::ast;
use ast::GetPos;
use peg::{str::LineCol, Parse};
use std::{borrow::Cow, error::Error, fmt};

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

macro_rules! unwrap_or {
    ($opt:expr, $($none:tt)+) => {
        match $opt {
            Some(v) => v,
            None => { $($none)+ }
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
    fn err_pos(&self, pos: impl ast::GetPos, message: String) -> ParseError {
        self.err_line_col(self.input.position_repr(pos.pos().byte_index), message)
    }
    fn expect_environment<'b>(
        &self,
        environment: &'b ast::Environment,
        name: &str,
    ) -> Result<&'b ast::Environment> {
        if environment.name.content == name {
            Ok(environment)
        } else {
            err!(self, environment, "expected `\\begin{{{}}}`", name);
        }
    }
    fn parse_tabular_column_definitions(
        &self,
        columns_group: &ast::Group,
    ) -> Result<Vec<ColumnDefinition>> {
        let mut tokens = ast::SplitCharTokensIter::new(&columns_group.tokens).peekable();
        let mut retval = Vec::new();
        loop {
            let kind = match unwrap_or!(tokens.next().as_deref(), break Ok(retval)) {
                ast::Token::CharTokens(c) if c.content == "l" => {
                    ColumnKind::LeftJustified { keyword_pos: c.pos }
                }
                ast::Token::CharTokens(c) if c.content == "p" => {
                    let width = tokens.next().map(Cow::into_owned);
                    let width_pos = width.as_ref().map(GetPos::pos);
                    let width = unwrap_or!(
                        width.and_then(|t| t.into_group()),
                        err!(
                            self,
                            width_pos.unwrap_or(columns_group.end.pos()),
                            "expected `{{`"
                        )
                    );
                    ColumnKind::ParagraphTop {
                        keyword_pos: c.pos,
                        width,
                    }
                }
                token => err!(self, token, "expected `tabular` column definition"),
            };
            retval.push(ColumnDefinition { kind });
        }
    }
    fn parse_tabular_env<'b>(&self, tabular_env: &'b ast::Environment) -> Result<()> {
        let mut tabular_body = tabular_env.body.iter();
        let mut first_token = tabular_body.next();
        match first_token.and_then(|t| Some(&*t.char_tokens()?.content)) {
            Some(pos) if matches!(pos, "t" | "b" | "c") => {
                first_token = None;
            }
            _ => {}
        }
        let column_definitions = self.parse_tabular_column_definitions(unwrap_or!(
            first_token
                .or_else(|| tabular_body.next())
                .and_then(ast::Token::group),
            err!(
                self,
                tabular_env.body_pos(),
                "expected `tabular` column definitions"
            )
        ))?;
        dbg!(&column_definitions);
        let mut column_index = 0;
        let mut columns = Vec::new();
        let mut cell: Option<()> = None; // TODO: use correct cell type
        while let Some(token) = tabular_body.next() {
            match token {
                ast::Token::AlignmentTab(alignment_tab) => {
                    column_index += 1;
                    err_if!(
                        column_index >= column_definitions.len(),
                        self,
                        alignment_tab,
                        "too many alignment tabs for column count"
                    );
                    columns.extend(cell.take());
                }
                ast::Token::Macro(macro_) if macro_.name.content == "\\" => {
                    todo!("start new row")
                }
                ast::Token::Whitespace(_) => {}
                _ => todo!("token={:?}", token),
            }
        }
        dbg!(&tabular_body);
        todo!()
    }
    fn parse_instruction_set(&self, document: &ast::Document) -> Result<InstructionSet> {
        for table_env in document.content.iter().filter_map(ast::Token::environment) {
            let table_env = self.expect_environment(table_env, "table")?;
            for small_env in table_env.body.iter().filter_map(ast::Token::environment) {
                let small_env = self.expect_environment(small_env, "small")?;
                for center_env in small_env.body.iter().filter_map(ast::Token::environment) {
                    let center_env = self.expect_environment(center_env, "center")?;
                    for tabular_env in center_env.body.iter().filter_map(ast::Token::environment) {
                        let tabular_env = self.expect_environment(tabular_env, "tabular")?;
                        let v = self.parse_tabular_env(tabular_env)?;
                        todo!();
                    }
                }
            }
        }
        err!(self, document, "no environment found")
    }
}

#[derive(Debug, Clone)]
pub struct InstructionSet {}

#[derive(Debug, Clone)]
pub enum ColumnKind {
    ParagraphTop {
        keyword_pos: ast::Pos,
        width: ast::Group,
    },
    LeftJustified {
        keyword_pos: ast::Pos,
    },
    // TODO: add rest
}

#[derive(Debug, Clone)]
pub struct ColumnDefinition {
    pub kind: ColumnKind,
}

pub fn parse(file_name: &str, input: &str) -> Result<InstructionSet> {
    let parser = Parser::new(file_name, input);
    let document = crate::tex::parse(input)
        .map_err(|e| parser.err_line_col(e.location, format!("expected: {}", e.expected)))?;
    parser.parse_instruction_set(&document)
}
