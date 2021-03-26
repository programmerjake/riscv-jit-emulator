// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use once_cell::unsync::OnceCell;
use peg::{str::LineCol, Parse};
use std::{
    borrow::Cow,
    cell::Cell,
    error::Error,
    fmt, mem,
    num::NonZeroUsize,
    ops::{Range, RangeInclusive},
    slice,
    str::FromStr,
};
use tex_parser::ast::{self, input_context::InputContext, GetPos};

macro_rules! opcode_constraint_separator {
    () => {
        None
    };
    (_) => {
        _
    };
    (',') => {
        Some(ast::Punctuation { ch: ',', .. })
    };
    (';') => {
        Some(ast::Punctuation { ch: ';', .. })
    };
}

macro_rules! opcode_constraint_field_def_name {
    ({$char_tokens:pat, zero_denied: $zero_denied:pat}) => {
        FieldDefName::CharTokens(
            $char_tokens,
            FieldZeroCondition {
                zero_denied: $zero_denied,
            },
        )
    };
}

macro_rules! opcode_constraint_field_def_slice {
    ({$name:tt, $bit_ranges:ident}) => {
        FieldDefSlice {
            name: opcode_constraint_field_def_name!($name),
            bit_ranges: $bit_ranges,
        }
    };
}

macro_rules! opcode_constraint_body {
    (RES) => {
        OpcodeNameFieldConstraintBody::ReservedForStandard
    };
    (NSE) => {
        OpcodeNameFieldConstraintBody::ReservedForCustom
    };
    (HINT) => {
        OpcodeNameFieldConstraintBody::Hint
    };
    ({$field:tt == $value:ident}) => {
        OpcodeNameFieldConstraintBody::FieldCondition {
            field: opcode_constraint_field_def_slice!($field),
            is_equality: true,
            value: $value,
        }
    };
    ({$field:tt != $value:ident}) => {
        OpcodeNameFieldConstraintBody::FieldCondition {
            field: opcode_constraint_field_def_slice!($field),
            is_equality: false,
            value: $value,
        }
    };
    ({isas: $isas:pat}) => {
        OpcodeNameFieldConstraintBody::ISAs($isas)
    };
}

macro_rules! opcode_constraint {
    ($body:tt $(, $separator:tt)?) => {
        OpcodeNameFieldConstraint {
            body: opcode_constraint_body!($body),
            separator: opcode_constraint_separator!($($separator)?),
            ..
        }
    };
}

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
    instr_table_name: InstrTableName,
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

struct TableRowIterator<'input, 'rest> {
    parser: &'rest Parser<'input>,
    tabular_body: std::slice::Iter<'rest, ast::Token>,
    pos_after_body: ast::Pos,
    column_definitions: &'rest [ColumnDefinition],
    column_ranges: Vec<ColumnRange>,
    column_range: Option<ColumnRange>,
    finished: bool,
}

impl GetPos for TableRowIterator<'_, '_> {
    fn pos(&self) -> ast::Pos {
        self.tabular_body
            .clone()
            .next()
            .map_or(self.pos_after_body, GetPos::pos)
    }
}

impl<'input, 'rest> TableRowIterator<'input, 'rest> {
    fn new(
        parser: &'rest Parser<'input>,
        tabular_body: std::slice::Iter<'rest, ast::Token>,
        pos_after_body: impl GetPos,
        column_definitions: &'rest [ColumnDefinition],
    ) -> Self {
        Self {
            parser,
            tabular_body,
            pos_after_body: pos_after_body.pos(),
            column_definitions,
            column_ranges: Vec::new(),
            column_range: None,
            finished: false,
        }
    }
    fn column_end_index(&self) -> usize {
        self.column_range
            .as_ref()
            .or(self.column_ranges.last())
            .map_or(0, |c| c.indexes.end)
    }
    fn columns_left(&self) -> usize {
        self.column_definitions.len() - self.column_end_index()
    }
    fn is_field_column(&self) -> bool {
        self.columns_left() > 1
    }
    fn is_opcode_column(&self) -> bool {
        self.columns_left() == 1
    }
    fn get_new_column_indexes(
        &mut self,
        new_column_width: usize,
        pos: impl GetPos,
        reason: impl FnOnce() -> String,
    ) -> Result<Range<usize>> {
        let start_index = self.column_end_index();
        let cols_left = self.columns_left();
        err_if!(
            new_column_width > cols_left,
            self.parser,
            pos,
            "{reason}: {cols_left} column(s) available out of {total} total",
            reason = reason(),
            cols_left = cols_left,
            total = self.column_definitions.len()
        );
        let end_index = start_index + new_column_width;
        Ok(start_index..end_index)
    }
    fn next_helper(&mut self) -> Result<Option<Vec<ColumnRange>>> {
        if self.finished {
            return Ok(None);
        }
        let mut columns = loop {
            let mut tabular_body_temp = self.tabular_body.clone();
            let token = unwrap_or!(tabular_body_temp.next(), {
                self.column_ranges.extend(self.column_range.take());
                self.finished = true;
                if self.column_ranges.is_empty() {
                    return Ok(None);
                }
                break mem::replace(&mut self.column_ranges, Vec::new());
            });
            match token {
                ast::Token::AlignmentTab(alignment_tab) => {
                    self.tabular_body = tabular_body_temp;
                    if let Some(column_range) = self.column_range.take() {
                        self.column_ranges.push(column_range);
                    } else {
                        let indexes = self.get_new_column_indexes(1, alignment_tab, || {
                            "this column would go past the table column count".into()
                        })?;
                        self.column_ranges.push(ColumnRange {
                            pos: alignment_tab.pos(),
                            indexes,
                            multi_column: None,
                            body: None,
                        })
                    }
                }
                ast::Token::Macro(macro_) if macro_.name.content == "\\" => {
                    self.tabular_body = tabular_body_temp;
                    self.column_ranges.extend(self.column_range.take());
                    break mem::replace(&mut self.column_ranges, Vec::new());
                }
                ast::Token::Macro(m) if m.name.content == "multicolumn" => {
                    self.tabular_body = tabular_body_temp;
                    err_if!(
                        self.column_range.is_some(),
                        self.parser,
                        m,
                        "`\\multicolumn` must be first thing in its column"
                    );
                    let (num_cols, num_cols_pos): (NonZeroUsize, _) =
                        self.parser.parse_number_in_group(
                            self.pos_after_body,
                            self.tabular_body.next().map(Cow::Borrowed),
                            || "expected `{{<number>}}` (like `{{12}}`)".into(),
                        )?;
                    let indexes =
                        self.get_new_column_indexes(num_cols.get(), num_cols_pos, || {
                            format!(
                                "`\\multicolumn{{{}}}` would go past the table column count",
                                num_cols
                            )
                        })?;
                    let cols_group = self.parser.unwrap_group_or_error(
                        self.pos_after_body,
                        self.tabular_body.next().map(Cow::Borrowed),
                    )?;
                    let mut multi_column = MultiColumn {
                        pos: cols_group.tokens_pos(),
                        kind: MultiColumnKind::Center,
                        vertical_bar_on_left: false,
                        vertical_bar_on_right: false,
                    };
                    let mut cols_group_str = match &*cols_group.tokens {
                        [ast::Token::CharTokens(char_tokens)] => &*char_tokens.content,
                        _ => err!(
                            self.parser,
                            cols_group.tokens_pos(),
                            "invalid `\\multicolumn` cols group"
                        ),
                    };
                    if let Some(rest) = cols_group_str.strip_prefix("|") {
                        cols_group_str = rest;
                        multi_column.vertical_bar_on_left = true;
                    }
                    if let Some(rest) = cols_group_str.strip_suffix("|") {
                        cols_group_str = rest;
                        multi_column.vertical_bar_on_right = true;
                    }
                    multi_column.kind = unwrap_or!(
                        MultiColumnKind::from_str(cols_group_str),
                        err!(
                            self.parser,
                            cols_group.tokens_pos(),
                            "invalid `\\multicolumn` cols group"
                        )
                    );
                    let body = self.parser.unwrap_group_or_error(
                        self.pos_after_body,
                        self.tabular_body.next().map(Cow::Borrowed),
                    )?;
                    self.parser
                        .skip_rest_of_column_body_else_error(&mut self.tabular_body, true)?;
                    let mut body_iter = body.tokens.iter();
                    let body = self.parser.parse_column_body(
                        &mut body_iter,
                        &body.end,
                        self.column_definitions,
                        false,
                        self.is_field_column(),
                        self.is_opcode_column(),
                    )?;
                    self.parser
                        .skip_rest_of_column_body_else_error(&mut body_iter, false)?;
                    self.column_range = Some(ColumnRange {
                        pos: m.pos(),
                        indexes,
                        multi_column: Some(multi_column),
                        body,
                    });
                }
                token if Parser::is_column_body_ignored(token) => {
                    self.tabular_body = tabular_body_temp;
                }
                token => {
                    assert!(self.column_range.is_none(), "token={:?}", token);
                    let is_field_column = self.is_field_column();
                    let is_opcode_column = self.is_opcode_column();
                    let body = self.parser.parse_column_body(
                        &mut self.tabular_body,
                        self.pos_after_body,
                        self.column_definitions,
                        true,
                        is_field_column,
                        is_opcode_column,
                    )?;
                    if body.is_none() {
                        continue;
                    }
                    let indexes = self.get_new_column_indexes(1, token, || {
                        "this column would go past the table column count".into()
                    })?;
                    self.parser
                        .skip_rest_of_column_body_else_error(&mut self.tabular_body, true)?;
                    self.column_range = Some(ColumnRange {
                        pos: token.pos(),
                        indexes,
                        multi_column: None,
                        body: body,
                    });
                }
            }
        };
        columns.reverse();
        for column in &mut columns {
            let rev_start = self.column_definitions.len() - column.indexes.end;
            let rev_end = self.column_definitions.len() - column.indexes.start;
            column.indexes = rev_start..rev_end;
        }
        Ok(Some(columns))
    }
    fn peekable(self) -> PeekableTableRowIterator<'input, 'rest> {
        PeekableTableRowIterator {
            iter: self,
            peek_value: None,
        }
    }
}

impl<'input, 'rest> Iterator for TableRowIterator<'input, 'rest> {
    type Item = Result<Vec<ColumnRange>>;

    fn next(&mut self) -> Option<Self::Item> {
        let retval = self.next_helper();
        if retval.is_err() {
            self.finished = true;
        }
        retval.transpose()
    }
}

struct PeekableTableRowIterator<'input, 'rest> {
    iter: TableRowIterator<'input, 'rest>,
    peek_value: Option<Vec<ColumnRange>>,
}

impl GetPos for PeekableTableRowIterator<'_, '_> {
    fn pos(&self) -> ast::Pos {
        self.peek_value
            .as_ref()
            .and_then(|v| v.first())
            .map_or_else(|| self.iter.pos(), |v| v.pos)
    }
}

impl<'input, 'rest> PeekableTableRowIterator<'input, 'rest> {
    fn peek(&mut self) -> Option<Result<&[ColumnRange]>> {
        if self.peek_value.is_none() {
            match self.iter.next()? {
                Ok(value) => {
                    self.peek_value = Some(value);
                }
                Err(e) => return Some(Err(e)),
            }
        }
        Some(Ok(self.peek_value.as_ref().unwrap()))
    }
}

impl<'input, 'rest> Iterator for PeekableTableRowIterator<'input, 'rest> {
    type Item = Result<Vec<ColumnRange>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peek_value) = self.peek_value.take() {
            return Some(Ok(peek_value));
        }
        self.iter.next()
    }
}

impl<'input> Parser<'input> {
    const COLUMN_START_PADDING: usize = 1;
    const COLUMN_END_PADDING: usize = 1;
    fn new(file_name: &'input str, input: &'input str, instr_table_name: InstrTableName) -> Self {
        Self {
            file_name,
            input,
            instr_table_name,
        }
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
    fn err_pos(&self, pos: impl GetPos, message: String) -> ParseError {
        self.err_line_col(self.input.position_repr(pos.pos().byte_index), message)
    }
    fn expect_environment<'env>(
        &self,
        environment: &'env ast::Environment,
        name: &str,
    ) -> Result<&'env ast::Environment> {
        if environment.name.content == name {
            Ok(environment)
        } else {
            err!(self, environment, "expected `\\begin{{{}}}`", name);
        }
    }
    fn unwrap_group_or_error<'t>(
        &self,
        pos_if_token_is_none: impl GetPos,
        token: Option<Cow<'t, ast::Token>>,
    ) -> Result<Cow<'t, ast::Group>> {
        let token_pos = token.as_ref().map(|t| t.pos());
        let group = token.and_then(|t| match t {
            Cow::Borrowed(t) => t.group().map(Cow::Borrowed),
            Cow::Owned(t) => t.into_group().map(Cow::Owned),
        });
        Ok(unwrap_or!(
            group,
            err!(
                self,
                token_pos.unwrap_or(pos_if_token_is_none.pos()),
                "expected `{{`"
            )
        ))
    }
    fn parse_number_in_group<T: FromStr>(
        &self,
        pos_if_token_is_none: impl GetPos,
        token: Option<Cow<'_, ast::Token>>,
        error_message: impl FnOnce() -> String,
    ) -> Result<(T, ast::Pos)>
    where
        T::Err: fmt::Display,
    {
        let num_cols_group = self.unwrap_group_or_error(pos_if_token_is_none, token)?;
        match &*num_cols_group.tokens {
            [ast::Token::Number(num_cols)] => Ok((
                num_cols.parse_with_err_arg(self, Parser::err_pos)?,
                num_cols.pos,
            )),
            _ => Err(self.err_pos(&*num_cols_group, error_message())),
        }
    }
    fn parse_tabular_column_definitions(
        &self,
        columns_group: &ast::Group,
    ) -> Result<Vec<ColumnDefinition>> {
        let mut tokens = ast::SplitCharTokensIter::new(&columns_group.tokens).peekable();
        let mut retval = Vec::new();
        loop {
            let kind = match unwrap_or!(tokens.next().as_deref(), break) {
                ast::Token::CharTokens(c) if c.content == "l" => {
                    ColumnKind::LeftJustified { keyword_pos: c.pos }
                }
                ast::Token::CharTokens(c) if c.content == "p" => {
                    let width = self
                        .unwrap_group_or_error(&columns_group.end, tokens.next())?
                        .into_owned();
                    ColumnKind::ParagraphTop {
                        keyword_pos: c.pos,
                        width,
                    }
                }
                token => err!(self, token, "expected `tabular` column definition"),
            };
            retval.push(ColumnDefinition {
                kind,
                merged_def: OnceCell::new(),
            });
        }
        err_if!(
            retval.len() < 2,
            self,
            columns_group,
            "there must be at least 2 columns"
        );
        retval.reverse();
        Ok(retval)
    }
    fn skip_rest_if_matches_else_error<'a, I: Iterator<Item = &'a ast::Token> + Clone>(
        &self,
        tokens: &mut I,
        mut matches: impl FnMut(&'a ast::Token) -> bool,
        mut is_terminator: impl FnMut(&'a ast::Token) -> bool,
        error_message: impl FnOnce(&'a ast::Token) -> String,
    ) -> Result<()> {
        loop {
            let mut temp_tokens = tokens.clone();
            let token = unwrap_or!(temp_tokens.next(), break Ok(()));
            if is_terminator(token) {
                break Ok(());
            } else if !matches(token) {
                return Err(self.err_pos(token, error_message(token)));
            }
            *tokens = temp_tokens;
        }
    }
    fn skip_whitespace<'a, I: Iterator<Item = &'a ast::Token> + Clone>(&self, tokens: &mut I) {
        while let Some(ast::Token::Whitespace(_)) = tokens.clone().next() {
            tokens.next();
        }
    }
    fn is_column_body_ignored(token: &ast::Token) -> bool {
        match token {
            ast::Token::Whitespace(_) | ast::Token::ParBreak(_) => true,
            _ => false,
        }
    }
    fn is_column_body_terminator(token: &ast::Token, is_top_level: bool) -> bool {
        match token {
            ast::Token::AlignmentTab(_) => is_top_level,
            ast::Token::Macro(m) if m.name.content == "\\" => is_top_level,
            _ => false,
        }
    }
    fn is_column_body_text(token: &ast::Token) -> bool {
        match token {
            ast::Token::CharTokens(_)
            | ast::Token::Group(_)
            | ast::Token::Whitespace(_)
            | ast::Token::Number(_) => true,
            ast::Token::Punctuation(ast::Punctuation { pos: _, ch }) if matches!(ch, '-' | '.') => {
                true
            }
            _ => false,
        }
    }
    fn is_column_body_bold_text(token: &ast::Token) -> bool {
        match token {
            ast::Token::CharTokens(_) => true,
            ast::Token::Punctuation(_) => true,
            ast::Token::Whitespace(_) => true,
            ast::Token::Macro(_) => true,
            ast::Token::Group(_) => true,
            _ => false,
        }
    }
    fn skip_rest_of_column_body_else_error<'a>(
        &self,
        tokens: &mut std::slice::Iter<ast::Token>,
        is_top_level: bool,
    ) -> Result<()> {
        self.skip_rest_if_matches_else_error(
            tokens,
            Self::is_column_body_ignored,
            |token| Self::is_column_body_terminator(token, is_top_level),
            |token| format!("unexpected token: {:?}", token),
        )
    }
    fn is_register_macro(macro_: &ast::Macro) -> bool {
        matches!(
            &*macro_.name.content,
            "rdprime" | "rsoneprime" | "rstwoprime"
        )
    }
    fn parse_field_def_name(
        &self,
        first_token: Option<&ast::Token>,
        tabular_body: &mut std::slice::Iter<ast::Token>,
        pos_after_body: impl GetPos,
    ) -> Result<FieldDefName> {
        let token = unwrap_or!(
            first_token.or_else(|| tabular_body.next()),
            err!(self, pos_after_body, "expected field name")
        );
        Ok(match token {
            ast::Token::Macro(macro_) if Self::is_register_macro(macro_) => {
                FieldDefName::Macro(macro_.name.clone())
            }
            ast::Token::CharTokens(char_tokens) => FieldDefName::CharTokens(
                char_tokens.clone(),
                FieldZeroCondition {
                    zero_denied: self.instr_table_name == InstrTableName::RvcInstrTable
                        && char_tokens.content.starts_with("nz"),
                },
            ),
            _ => err!(self, token, "expected field name"),
        })
    }
    fn match_token_or_error<'a, T: GetPos>(
        &self,
        matches: impl FnOnce(&T) -> bool,
        tabular_body: &mut std::slice::Iter<'a, T>,
        pos_after_body: impl GetPos,
        error_message: impl FnOnce() -> String,
    ) -> Result<&'a T> {
        let pos = match tabular_body.next() {
            Some(token) if matches(token) => return Ok(token),
            Some(token) => token.pos(),
            None => pos_after_body.pos(),
        };
        Err(self.err_pos(pos, error_message()))
    }
    fn parse_field_def_slice(
        &self,
        name: FieldDefName,
        tabular_body: &mut std::slice::Iter<ast::Token>,
        pos_after_body: impl GetPos,
    ) -> Result<FieldDefSlice> {
        match tabular_body.clone().next() {
            Some(ast::Token::Punctuation(ast::Punctuation { pos: _, ch: '[' })) => {
                tabular_body.next();
            }
            _ => {
                return Ok(FieldDefSlice {
                    name,
                    bit_ranges: Vec::new(),
                });
            }
        }
        let mut bit_ranges = Vec::new();
        loop {
            let (end, end_bit_pos): (u32, _) = match tabular_body.next() {
                Some(ast::Token::Number(num)) => {
                    (num.parse_with_err_arg(self, Self::err_pos)?, num.pos())
                }
                token => err!(
                    self,
                    token.map_or(pos_after_body.pos(), GetPos::pos),
                    "expected number"
                ),
            };
            if let Some(ast::Token::Punctuation(ast::Punctuation { pos: _, ch: ':' })) =
                tabular_body.clone().next()
            {
                tabular_body.next();
                let (start, start_bit_pos): (u32, _) = match tabular_body.next() {
                    Some(ast::Token::Number(num)) => {
                        (num.parse_with_err_arg(self, Self::err_pos)?, num.pos())
                    }
                    token => err!(
                        self,
                        token.map_or(pos_after_body.pos(), GetPos::pos),
                        "expected number"
                    ),
                };
                err_if!(
                    end < start,
                    self,
                    end_bit_pos,
                    "range end ({}) must not be less than range start ({})",
                    end,
                    start
                );
                bit_ranges.push(FieldBitRange {
                    bits: start..=end,
                    end_bit_pos,
                    start_bit_pos: Some(start_bit_pos),
                });
            } else {
                bit_ranges.push(FieldBitRange {
                    bits: end..=end,
                    end_bit_pos,
                    start_bit_pos: None,
                });
            }
            match unwrap_or!(
                tabular_body.next(),
                err!(self, pos_after_body, "missing `]`")
            ) {
                ast::Token::Punctuation(ast::Punctuation { pos: _, ch: ']' }) => {
                    break;
                }
                ast::Token::DollarInlineMath(ast::DollarInlineMath {
                    begin,
                    content,
                    end: _,
                }) => match &**content {
                    [ast::MathToken::Macro(ast::Macro { escape: _, name })]
                        if name.content == "vert" =>
                    {
                        continue;
                    }
                    _ => err!(self, begin, "expected `$\\vert$`"),
                },
                token => err!(self, token, "expected `$\\vert$` or `]`"),
            }
        }
        Ok(FieldDefSlice { name, bit_ranges })
    }
    fn parse_field_def_alternate(
        &self,
        field_def_name: FieldDefName,
        tabular_body: &mut std::slice::Iter<ast::Token>,
        pos_after_body: impl GetPos,
    ) -> Result<FieldDefAlternate> {
        let mut names = vec![field_def_name];
        while let Some(ast::Token::Punctuation(ast::Punctuation { pos: _, ch: '/' })) =
            tabular_body.clone().next()
        {
            tabular_body.next();
            names.push(self.parse_field_def_name(None, tabular_body, &pos_after_body)?);
        }
        Ok(FieldDefAlternate { names })
    }
    fn parse_field_def(
        &self,
        first_token: &ast::Token,
        tabular_body: &mut std::slice::Iter<ast::Token>,
        pos_after_body: impl GetPos,
    ) -> Result<FieldDef> {
        match first_token {
            ast::Token::Number(num) => {
                return Ok(FieldDef {
                    body: FieldDefBody::LiteralNumber(num.clone()),
                    excluded_values: Vec::new(),
                });
            }
            ast::Token::Punctuation(p) if p.ch == '-' => {
                let first_token = first_token.punctuation().unwrap();
                let mut content = String::from(first_token.ch);
                for _ in 1..3 {
                    let token = self
                        .match_token_or_error(
                            |t| matches!(t, ast::Token::Punctuation(p) if p.ch == '-'),
                            tabular_body,
                            &pos_after_body,
                            || "expected: `-`".into(),
                        )?
                        .punctuation()
                        .unwrap();
                    content.push(token.ch);
                }
                return Ok(FieldDef {
                    body: FieldDefBody::Wildcard(Wildcard {
                        pos: first_token.pos(),
                        content,
                    }),
                    excluded_values: Vec::new(),
                });
            }
            _ => {}
        }
        let field_def_name =
            self.parse_field_def_name(Some(first_token), tabular_body, &pos_after_body)?;
        let body = match tabular_body.clone().next() {
            Some(ast::Token::Punctuation(ast::Punctuation { pos: _, ch: '[' })) => {
                FieldDefBody::Slice(self.parse_field_def_slice(
                    field_def_name,
                    tabular_body,
                    &pos_after_body,
                )?)
            }
            _ => FieldDefBody::Alternate(self.parse_field_def_alternate(
                field_def_name,
                tabular_body,
                &pos_after_body,
            )?),
        };
        let excluded_values = match tabular_body
            .clone()
            .next()
            .and_then(ast::Token::dollar_inline_math)
            .map(|v| &*v.content)
        {
            Some([ast::MathToken::Macro(macro_)]) if macro_.name.content == "neq" => {
                tabular_body.next();
                match tabular_body.next() {
                    Some(ast::Token::Number(num)) => vec![num.clone()],
                    Some(ast::Token::DollarInlineMath(dollar_inline_math)) => {
                        let mut dollar_inline_math_body = dollar_inline_math.content.iter();
                        self.match_token_or_error(
                            |t| matches!(t, ast::MathToken::Macro(m) if m.name.content == "{"),
                            &mut dollar_inline_math_body,
                            &dollar_inline_math.end,
                            || "expected `\\{{`".into(),
                        )?;
                        let mut excluded_values = match dollar_inline_math_body.next() {
                            Some(ast::MathToken::Number(num)) => vec![num.clone()],
                            _ => err!(self, dollar_inline_math.content_pos(), "expected number"),
                        };
                        while let Some(token) = dollar_inline_math_body.next() {
                            if matches!(token, ast::MathToken::Macro(m) if m.name.content == "}") {
                                if let Some(token) = dollar_inline_math_body.next() {
                                    err!(self, token, "unexpected token");
                                }
                                break;
                            }
                            err_if!(
                                !matches!(
                                    token,
                                    ast::MathToken::AnyChar(ast::AnyChar { pos: _, ch: ',' })
                                ),
                                self,
                                token,
                                "expected `,` or `\\}}`"
                            );
                            match dollar_inline_math_body.next() {
                                Some(ast::MathToken::Number(num)) => {
                                    excluded_values.push(num.clone())
                                }
                                Some(token) => {
                                    err!(self, token, "expected number")
                                }
                                None => {
                                    err!(self, dollar_inline_math.end.pos(), "expected number")
                                }
                            }
                        }
                        excluded_values
                    }
                    token => err!(self,
                        token.map_or_else(||pos_after_body.pos(), GetPos::pos),
                        "expected number or list of numbers (like so: `$\\{{<num>[,<num>[,<num>[...]]]\\}}$`)"
                    ),
                }
            }
            _ => Vec::new(),
        };
        Ok(FieldDef {
            body,
            excluded_values,
        })
    }
    fn parse_opcode_name_field_constraint_isas(
        &self,
        first_isa: Option<ISABase>,
        tokens: &mut std::slice::Iter<ast::Token>,
        pos_after_body: impl GetPos,
    ) -> Result<Vec<ISABase>> {
        let mut retval = Vec::new();
        retval.extend(first_isa);
        while let Some(ast::Token::Punctuation(ast::Punctuation { ch: '/', .. })) =
            tokens.clone().next()
        {
            tokens.next();
            retval.push(match tokens.next() {
                Some(ast::Token::Number(num)) => match &*num.content {
                    "32" => ISABase::RV32I,
                    "64" => ISABase::RV64I,
                    "128" => continue,
                    _ => err!(self, num, "invalid ISA bit-count: expected 32, 64, or 128"),
                },
                token => err!(
                    self,
                    token
                        .map(GetPos::pos)
                        .unwrap_or_else(|| pos_after_body.pos()),
                    "expected ISA bit-count: expected 32, 64, or 128"
                ),
            });
        }
        Ok(retval)
    }
    fn parse_opcode_name_field_constraint(
        &self,
        tokens: &mut std::slice::Iter<ast::Token>,
        pos_after_body: impl GetPos,
    ) -> Result<OpcodeNameFieldConstraint> {
        self.skip_whitespace(tokens);
        let token = unwrap_or!(
            tokens.next(),
            err!(self, pos_after_body, "expected constraint")
        );
        let pos = token.pos();
        let body = match token {
            ast::Token::CharTokens(c) if c.content == "RES" => {
                OpcodeNameFieldConstraintBody::ReservedForStandard
            }
            ast::Token::CharTokens(c) if c.content == "NSE" => {
                OpcodeNameFieldConstraintBody::ReservedForCustom
            }
            ast::Token::CharTokens(c) if c.content == "HINT" => OpcodeNameFieldConstraintBody::Hint,
            ast::Token::CharTokens(c) if c.content == "RV32" => {
                OpcodeNameFieldConstraintBody::ISAs(self.parse_opcode_name_field_constraint_isas(
                    Some(ISABase::RV32I),
                    tokens,
                    pos_after_body,
                )?)
            }
            ast::Token::CharTokens(c) if c.content == "RV64" => {
                OpcodeNameFieldConstraintBody::ISAs(self.parse_opcode_name_field_constraint_isas(
                    Some(ISABase::RV64I),
                    tokens,
                    pos_after_body,
                )?)
            }
            ast::Token::CharTokens(c) if c.content == "RV128" => {
                OpcodeNameFieldConstraintBody::ISAs(self.parse_opcode_name_field_constraint_isas(
                    None,
                    tokens,
                    pos_after_body,
                )?)
            }
            ast::Token::CharTokens(c)
                if c.content.chars().next().map(|c| c.is_ascii_lowercase()) == Some(true) =>
            {
                let name = self.parse_field_def_name(Some(token), tokens, &pos_after_body)?;
                let field = self.parse_field_def_slice(name, tokens, &pos_after_body)?;
                let is_equality = match tokens.next() {
                    Some(ast::Token::DollarInlineMath(v)) if matches!(&*v.content, [ast::MathToken::Macro(m)] if m.name.content == "neq") => {
                        false
                    }
                    Some(ast::Token::Punctuation(p)) if p.ch == '=' => true,
                    token => err!(
                        self,
                        token.map_or_else(|| pos_after_body.pos(), GetPos::pos),
                        r#"expected: `=` or `$\neq$`"#
                    ),
                };
                let value = self
                    .match_token_or_error(
                        |t| t.number().is_some(),
                        tokens,
                        pos_after_body,
                        || "expected: number".into(),
                    )?
                    .number()
                    .unwrap()
                    .clone();
                OpcodeNameFieldConstraintBody::FieldCondition {
                    field,
                    is_equality,
                    value,
                }
            }
            _ => err!(self, token, "expected constraint"),
        };
        self.skip_whitespace(tokens);
        let separator = match tokens.clone().next() {
            Some(ast::Token::Punctuation(p @ ast::Punctuation { ch: ',', .. }))
            | Some(ast::Token::Punctuation(p @ ast::Punctuation { ch: ';', .. })) => {
                tokens.next();
                Some(p.clone())
            }
            _ => None,
        };
        Ok(OpcodeNameFieldConstraint {
            pos,
            body,
            separator,
        })
    }
    fn parse_opcode_name_field_constraints(
        &self,
        group: &ast::Group,
    ) -> Result<Vec<OpcodeNameFieldConstraint>> {
        let mut body = group.tokens.iter();
        self.match_token_or_error(
            |t| matches! (t, ast::Token::Macro(m) if m.name.content == "em"),
            &mut body,
            &group.end,
            || "expected: `\\em`".into(),
        )?;
        self.skip_whitespace(&mut body);
        self.match_token_or_error(
            |t| matches! (t, ast::Token::Macro(m) if m.name.content == "tiny"),
            &mut body,
            &group.end,
            || "expected: `\\tiny`".into(),
        )?;
        self.skip_whitespace(&mut body);
        self.match_token_or_error(
            |t| matches! (t, ast::Token::Punctuation(p) if p.ch == '('),
            &mut body,
            &group.end,
            || "expected: `(`".into(),
        )?;
        let mut constraints = Vec::new();
        loop {
            constraints.push(self.parse_opcode_name_field_constraint(&mut body, &group.end)?);
            self.skip_whitespace(&mut body);
            if body
                .clone()
                .next()
                .and_then(|p| p.punctuation())
                .map(|p| p.ch)
                == Some(')')
            {
                body.next();
                break;
            }
        }
        self.skip_whitespace(&mut body);
        self.skip_rest_if_matches_else_error(
            &mut body,
            |_| false,
            |_| false,
            |_| "expected `}`".into(),
        )?;
        Ok(constraints)
    }
    fn parse_opcode_name_field(
        &self,
        first_token: &ast::Token,
        tabular_body: &mut std::slice::Iter<ast::Token>,
        is_top_level: bool,
    ) -> Result<OpcodeNameField> {
        let pos = first_token.pos();
        let mut first_token = Some(first_token);
        let mut name = None;
        let mut group = None;
        while let Some(token) = first_token.or_else(|| tabular_body.clone().next()) {
            if Self::is_column_body_terminator(token, is_top_level) {
                assert!(first_token.is_none());
                break;
            }
            if first_token.take().is_none() {
                tabular_body.next();
            }
            match token {
                ast::Token::CharTokens(char_tokens) => name
                    .get_or_insert(String::new())
                    .push_str(&char_tokens.content),
                ast::Token::Punctuation(p) => {
                    name.get_or_insert(String::new()).push(p.ch);
                }
                ast::Token::Group(_) | ast::Token::Whitespace(_) => break,
                _ => err!(self, token, "expected `{{`"),
            }
        }
        while let Some(token) = tabular_body.clone().next() {
            if Self::is_column_body_terminator(token, is_top_level) {
                break;
            }
            tabular_body.next();
            match token {
                ast::Token::Whitespace(_) => {}
                ast::Token::Group(g) => {
                    group = Some(g);
                    break;
                }
                _ => err!(self, token, "expected: `{{`"),
            }
        }
        let constraints = if let Some(group) = group {
            self.parse_opcode_name_field_constraints(group)?
        } else {
            Vec::new()
        };
        Ok(OpcodeNameField {
            pos,
            name: unwrap_or!(name, err!(self, pos, "expected: opcode name")),
            constraints,
        })
    }
    fn parse_column_body(
        &self,
        tabular_body: &mut std::slice::Iter<ast::Token>,
        pos_after_body: impl GetPos,
        column_definitions: &[ColumnDefinition],
        is_top_level: bool,
        is_field_column: bool,
        is_opcode_column: bool,
    ) -> Result<Option<ColumnBody>> {
        loop {
            let token = unwrap_or!(tabular_body.clone().next(), return Ok(None));
            if Self::is_column_body_terminator(token, is_top_level) {
                return Ok(None);
            }
            tabular_body.next();
            if Self::is_column_body_ignored(token) {
                continue;
            }
            return Ok(Some(match token {
                ast::Token::Macro(macro_)
                    if matches!(&*macro_.name.content, "cline" | "whline") && is_top_level =>
                {
                    self.unwrap_group_or_error(
                        &pos_after_body,
                        tabular_body.next().map(Cow::Borrowed),
                    )?;
                    continue;
                }
                ast::Token::Macro(macro_) if Self::is_register_macro(macro_) => {
                    let field_def = self.parse_field_def(token, tabular_body, pos_after_body)?;
                    self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                    ColumnBody::FieldDef(field_def)
                }
                ast::Token::Macro(macro_) if macro_.name.content == "instbit" => {
                    let (bit, pos) = self.parse_number_in_group(
                        pos_after_body,
                        tabular_body.next().map(Cow::Borrowed),
                        || "expected `{{<bit-number>}}` (like `{{12}}`)".into(),
                    )?;
                    self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                    ColumnBody::InstBit(InstBit { pos, bit })
                }
                ast::Token::Macro(macro_) if macro_.name.content == "instbitrange" => {
                    let (end_bit, end_bit_pos) = self.parse_number_in_group(
                        &pos_after_body,
                        tabular_body.next().map(Cow::Borrowed),
                        || "expected `{{<start-bit-number>}}` (like `{{12}}`)".into(),
                    )?;
                    let (start_bit, start_bit_pos) = self.parse_number_in_group(
                        pos_after_body,
                        tabular_body.next().map(Cow::Borrowed),
                        || "expected `{{<end-bit-number>}}` (like `{{12}}`)".into(),
                    )?;
                    err_if!(
                        start_bit > end_bit,
                        self,
                        start_bit_pos,
                        "start bit must not be bigger than end bit"
                    );
                    self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                    ColumnBody::InstBitRange(InstBitRange {
                        bits: start_bit..=end_bit,
                        end_bit_pos,
                        start_bit_pos,
                    })
                }
                ast::Token::Macro(macro_) if macro_.name.content == "bf" => {
                    let mut tokens: Vec<ast::Token> = vec![token.clone()];
                    while let Some(token) = tabular_body
                        .clone()
                        .next()
                        .filter(|token| Self::is_column_body_bold_text(token))
                    {
                        tabular_body.next();
                        tokens.push(token.clone());
                    }
                    self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                    ColumnBody::ColumnBodyBoldText(ColumnBodyBoldText {
                        pos: tokens[0].pos(),
                        tokens,
                    })
                }
                ast::Token::Group(g) => {
                    self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                    ColumnBody::Group(g.clone())
                }
                _ if Self::is_column_body_text(token) => {
                    if is_field_column {
                        let field_def =
                            self.parse_field_def(token, tabular_body, pos_after_body)?;
                        self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                        return Ok(Some(ColumnBody::FieldDef(field_def)));
                    } else if is_opcode_column {
                        let opcode_name_field =
                            self.parse_opcode_name_field(token, tabular_body, is_top_level)?;
                        self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                        return Ok(Some(ColumnBody::OpcodeName(opcode_name_field)));
                    }
                    let mut tokens: Vec<ast::Token> = vec![token.clone()];
                    while let Some(token) = tabular_body
                        .clone()
                        .next()
                        .filter(|token| Self::is_column_body_text(token))
                    {
                        tabular_body.next();
                        tokens.push(token.clone());
                    }
                    self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                    ColumnBody::ColumnBodyText(ColumnBodyText {
                        pos: tokens[0].pos(),
                        tokens,
                    })
                }
                _ => err!(self, token, "unexpected token"),
            }));
        }
    }
    fn assert_column_is_empty(&self, column: &ColumnRange) -> Result<()> {
        err_if!(
            column.multi_column.is_some(),
            self,
            column.pos,
            "`\\multicolumn` is not allowed here"
        );
        err_if!(
            column.body.is_some(),
            self,
            column.pos,
            "column must be empty here"
        );
        Ok(())
    }
    fn parse_blank_row(&self, row_iterator: &mut PeekableTableRowIterator) -> Result<()> {
        let pos = row_iterator.pos();
        let columns = unwrap_or!(row_iterator.next(), err!(self, pos, "expected: blank row"))?;
        for column in columns.iter().rev() {
            self.assert_column_is_empty(column)?;
        }
        Ok(())
    }
    fn parse_instruction_bit_ranges_row(
        &self,
        row_iterator: &mut PeekableTableRowIterator,
        column_definitions: &[ColumnDefinition],
    ) -> Result<()> {
        let pos = row_iterator.pos();
        let columns = unwrap_or!(
            row_iterator.next(),
            err!(self, pos, "expected: instruction bit ranges row")
        )?;
        err_if!(
            columns.is_empty(),
            self,
            columns.last().map_or(pos, |v| v.pos),
            "not enough columns in instruction bit ranges row"
        );
        let (last_column, columns) = columns.split_last().unwrap();
        self.assert_column_is_empty(last_column)?;
        for column_range in columns {
            err_if!(
                column_range.indexes.len() != 1,
                self,
                column_range.pos,
                "spanning multiple columns using `\\multicolumn{{{}}}` is not allowed in instruction bit ranges row",
                column_range.indexes.len()
            );
            err_if!(
                column_range.body.is_none(),
                self,
                column_range.pos,
                "empty columns are not allowed in instruction bit ranges row"
            );
        }
        err_if!(
            columns.len()
                != column_definitions.len() - Self::COLUMN_START_PADDING - Self::COLUMN_END_PADDING,
            self,
            columns.last().map_or(pos, |v| v.pos),
            "not enough columns in instruction bit ranges row"
        );
        let mut last_range_end = None;
        let mut index = 0;
        while index < columns.len() {
            let mut column_range = index..index + 1;
            let column = &columns[index];
            let bit_range = match (
                column.body.as_ref().unwrap(),
                column.multi_column.as_ref().map(|v| v.kind),
            ) {
                (ColumnBody::InstBit(v), None)
                | (ColumnBody::InstBit(v), Some(MultiColumnKind::Center)) => v.clone().into(),
                (ColumnBody::InstBitRange(v), None) => v.clone(),
                (ColumnBody::InstBit(right), Some(MultiColumnKind::Right)) => {
                    err_if!(
                        index + 1 >= columns.len(),
                        self,
                        column.pos,
                        // first because we reverse the columns
                        "`\\multicolumn{{1}}{{r}}` is not valid here: this is the first column",
                    );
                    column_range = index..index + 2;
                    let left_column = &columns[index + 1];
                    err_if!(
                        Some(MultiColumnKind::Left)
                            != left_column.multi_column.as_ref().map(|v| v.kind),
                        self,
                        left_column.pos,
                        "expected: `\\multicolumn{{1}}{{l}}`",
                    );
                    if let ColumnBody::InstBit(left) = left_column.body.as_ref().unwrap() {
                        InstBitRange {
                            bits: right.bit..=left.bit,
                            end_bit_pos: left.pos,
                            start_bit_pos: right.pos,
                        }
                    } else {
                        err!(
                            self,
                            left_column.body.as_ref().unwrap().pos(),
                            "expected: `\\instbit`"
                        )
                    }
                }
                _ => err!(
                    self,
                    column.body.as_ref().unwrap().pos(),
                    "expected `\\instbit` or `\\instbitrange`"
                ),
            };
            if let Some((last_range_end, last_range_end_pos)) = mem::replace(
                &mut last_range_end,
                Some((*bit_range.bits.end(), bit_range.end_bit_pos)),
            ) {
                let next_bit_index = unwrap_or!(
                    last_range_end.checked_add(1),
                    err!(self, last_range_end_pos, "number too big")
                );
                err_if!(
                    *bit_range.bits.start() != next_bit_index,
                    self,
                    bit_range.start_bit_pos,
                    "expected {}",
                    next_bit_index
                );
            } else {
                err_if!(
                    *bit_range.bits.start() != 0,
                    self,
                    bit_range.start_bit_pos,
                    "expected 0"
                );
            }
            let mut padded_column_range = column_range.clone();
            padded_column_range.start += Self::COLUMN_START_PADDING;
            padded_column_range.end += Self::COLUMN_START_PADDING;
            let merged_def = MergedColumnDefinition {
                column_range: padded_column_range.clone(),
                bit_range,
            };
            for c in &column_definitions[padded_column_range.clone()] {
                c.merged_def.set(merged_def.clone()).ok().unwrap();
            }
            index = column_range.end;
        }
        Ok(())
    }
    fn parse_instruction_field(
        &self,
        column: ColumnRange,
        column_definitions: &[ColumnDefinition],
        is_fence_instruction_form: bool,
    ) -> Result<InstructionField> {
        let field_def = match column.body {
            Some(ColumnBody::FieldDef(field_def)) => field_def,
            _ => err!(self, column.pos, "expected: instruction field"),
        };
        let field_matches_name_or_num = |name: &str, num_len: usize| -> bool {
            field_def.only_char_tokens().map(|v| &*v.content) == Some(name)
                || matches!(&field_def.body, FieldDefBody::LiteralNumber(num) if num.content.len() == num_len)
        };
        if is_fence_instruction_form {
            let bits = match column.indexes {
                Range { start: 5, end: 6 } => {
                    err_if!(
                        !field_matches_name_or_num("succ", 4),
                        self,
                        column.pos,
                        "expected fence instruction's `succ` field"
                    );
                    Some(20..=23)
                }
                Range { start: 6, end: 9 } => {
                    err_if!(
                        !field_matches_name_or_num("pred", 4),
                        self,
                        column.pos,
                        "expected fence instruction's `pred` field"
                    );
                    Some(24..=27)
                }
                _ => None,
            };
            if let Some(bits) = bits {
                return Ok(InstructionField {
                    field_def,
                    instruction_bit_range: InstBitRange {
                        bits,
                        end_bit_pos: column.pos,
                        start_bit_pos: column.pos,
                    },
                });
            }
        }
        let column_pos = column.pos;
        let mismatch_err = || {
            self.err_pos(
                column_pos,
                "instruction field doesn't match up with instruction-bit-numbers row".into(),
            )
        };
        let start_column_definition = unwrap_or!(
            column_definitions[column.indexes.start].merged_def.get(),
            return Err(mismatch_err());
        );
        if start_column_definition.column_range.start != column.indexes.start {
            return Err(mismatch_err());
        }
        let last_column_definition = unwrap_or!(
            column_definitions[column.indexes.clone().last().unwrap()].merged_def.get(),
            return Err(mismatch_err());
        );
        if last_column_definition.column_range.end != column.indexes.end {
            return Err(mismatch_err());
        }
        let instruction_bit_range = InstBitRange {
            bits: *start_column_definition.bit_range.bits.start()
                ..=*last_column_definition.bit_range.bits.end(),
            end_bit_pos: last_column_definition.bit_range.end_bit_pos,
            start_bit_pos: start_column_definition.bit_range.start_bit_pos,
        };
        Ok(InstructionField {
            instruction_bit_range,
            field_def,
        })
    }
    fn parse_instruction_fields(
        &self,
        row: Vec<ColumnRange>,
        column_definitions: &[ColumnDefinition],
        is_fence_instruction_form: bool,
    ) -> Result<Vec<InstructionField>> {
        let first = row.first().expect("row already checked to be non-empty");
        err_if!(
            first.indexes.len() != Self::COLUMN_START_PADDING,
            self,
            first.pos,
            "spanning multiple columns using `\\multicolumn{{{}}}` is not allowed here",
            first.indexes.len()
        );
        let last = row.last().expect("row already checked to be non-empty");
        err_if!(
            last.indexes.len() != Self::COLUMN_END_PADDING,
            self,
            last.pos,
            "spanning multiple columns using `\\multicolumn{{{}}}` is not allowed here",
            last.indexes.len()
        );
        err_if!(
            row.len() < Self::COLUMN_START_PADDING + Self::COLUMN_END_PADDING,
            self,
            last.pos,
            "row doesn't have enough columns"
        );
        let row_len = row.len();
        let fields = row
            .into_iter()
            .take(row_len - Self::COLUMN_END_PADDING)
            .skip(Self::COLUMN_START_PADDING)
            .map(|column| {
                self.parse_instruction_field(column, column_definitions, is_fence_instruction_form)
            })
            .collect::<Result<_>>()?;
        Ok(fields)
    }
    fn parse_instruction_form_rows(
        &self,
        row_iterator: &mut PeekableTableRowIterator,
        column_definitions: &[ColumnDefinition],
    ) -> Result<Vec<InstructionForm>> {
        let mut instruction_forms = Vec::new();
        while let Some(row) = row_iterator.peek().transpose()? {
            let name = match row.first() {
                Some(ColumnRange {
                    body: Some(ColumnBody::OpcodeName(name)),
                    ..
                }) if name.constraints.is_empty() && name.name.ends_with("-type") => {
                    InstructionFormName {
                        pos: name.pos,
                        name: name.name.clone(),
                    }
                }
                _ => break,
            };
            let fields = self.parse_instruction_fields(
                row_iterator.next().unwrap().unwrap(),
                column_definitions,
                false,
            )?;
            instruction_forms.push(InstructionForm { name, fields });
        }
        Ok(instruction_forms)
    }
    fn parse_rvc_isa_extension(&self, opcode: &OpcodeNameField) -> Result<ISAExtension> {
        Ok(if opcode.name.starts_with("C.F") {
            if opcode.name.ends_with("W") || opcode.name.ends_with("WSP") {
                ISAExtension::F
            } else if opcode.name.ends_with("D") || opcode.name.ends_with("DSP") {
                ISAExtension::D
            } else {
                err!(
                    self,
                    opcode.pos,
                    "unrecognized compressed floating-point opcode: {:?}",
                    opcode.name
                );
            }
        } else {
            ISAExtension::I
        })
    }
    fn parse_instruction(
        &self,
        row: Vec<ColumnRange>,
        column_definitions: &[ColumnDefinition],
    ) -> Result<Vec<Instruction>> {
        let opcode = match row.first().expect("row already checked to be non-empty") {
            ColumnRange {
                body: Some(ColumnBody::OpcodeName(opcode)),
                ..
            } => opcode.clone(),
            column => err!(self, column.pos, "expected: opcode"),
        };
        let extension;
        let bases;
        match self.instr_table_name {
            InstrTableName::InstrTable => {
                todo!()
            }
            InstrTableName::RvcInstrTable => {
                extension = self.parse_rvc_isa_extension(&opcode)?;
                bases = ISABase::VALUES.to_vec();
            }
        }
        let OpcodeNameField {
            pos,
            name,
            constraints,
        } = opcode;
        let is_fence_instruction_form = matches!(&*name, "FENCE" | "FENCE.TSO" | "PAUSE");
        let instruction_name = InstructionName { pos, name };
        let mut fields =
            self.parse_instruction_fields(row, column_definitions, is_fence_instruction_form)?;
        let mut instructions = Vec::with_capacity(ISABase::VALUES.len());
        if constraints.is_empty() {
            for base in bases {
                instructions.push(Instruction {
                    isa_module: ISAModule { base, extension },
                    name: instruction_name.clone(),
                    fields: fields.clone(),
                });
            }
            return Ok(instructions);
        }
        err_if!(
            self.instr_table_name != InstrTableName::RvcInstrTable,
            self,
            constraints[0].pos(),
            "constraints not allowed here"
        );
        let bases = match &*constraints {
            [opcode_constraint!(HINT, ','), opcode_constraint!(
                {
                    {
                        { field_name, zero_denied: true },
                        bit_ranges
                    } != value
                }
            )] if instruction_name.name == "C.NOP"
                && bit_ranges.is_empty()
                && value.content == "0" =>
            {
                for field in &mut fields {
                    for name in field.field_def.body.names_mut() {
                        match name {
                            FieldDefName::CharTokens(
                                ast::CharTokens { content, .. },
                                FieldZeroCondition { zero_denied },
                            ) if *content == field_name.content => *zero_denied = false,
                            _ => {}
                        }
                    }
                }
                ISABase::VALUES.to_vec()
            }
            [opcode_constraint!(RES, ','), opcode_constraint!(
                {
                    {
                        { _, zero_denied: true },
                        bit_ranges1
                    } == value1
                }, ';'
            ), opcode_constraint!(HINT, ','), opcode_constraint!(
                {
                    {
                        { field_name, zero_denied: false },
                        bit_ranges2
                    } == value2
                }
            )] if instruction_name.name == "C.LUI"
                && bit_ranges1.is_empty()
                && value1.content == "0"
                && bit_ranges2.is_empty()
                && value2.content == "0" =>
            {
                for field in &mut fields {
                    if field
                        .field_def
                        .body
                        .find_matches_char_tokens(&field_name.content)
                        .is_none()
                    {
                        continue;
                    }
                    match &*field.field_def.excluded_values {
                        [num0, num2] if num0.content == "0" && num2.content == "2" => {
                            field.field_def.excluded_values.remove(0);
                        }
                        _ => err!(self, &field.field_def, "unexpected field value"),
                    }
                }
                ISABase::VALUES.to_vec()
            }
            [opcode_constraint!(RES, ','), opcode_constraint!(
                {
                    {
                        { _, zero_denied: true },
                        bit_ranges
                    } == value
                }, _
            )]
            | [opcode_constraint!(HINT, ','), opcode_constraint!(
                {
                    {
                        { _, zero_denied: true },
                        bit_ranges
                    } == value
                }
            )]
            | [opcode_constraint!(HINT, ','), opcode_constraint!(
                {
                    {
                        { _, zero_denied: false },
                        bit_ranges
                    } == value
                }
            )] if value.content == "0" && bit_ranges.is_empty() => ISABase::VALUES.to_vec(),
            [opcode_constraint!({ isas: isas }, _)] => isas.to_vec(),
            [opcode_constraint!({ isas: isas }, ';'), opcode_constraint!(RES, ','), opcode_constraint!(
                {
                    {
                        { field_name, zero_denied: false },
                        bit_ranges
                    } == value
                }
            )] if bit_ranges.is_empty() && value.content == "0" => {
                for field in &mut fields {
                    if field
                        .field_def
                        .body
                        .find_matches_char_tokens(&field_name.content)
                        .is_none()
                    {
                        continue;
                    }
                    for name in field.field_def.body.names_mut() {
                        match name {
                            FieldDefName::CharTokens(_, FieldZeroCondition { zero_denied }) => {
                                *zero_denied = true
                            }
                            _ => {}
                        }
                    }
                }
                isas.to_vec()
            }
            [opcode_constraint!({ isas: isas }), opcode_constraint!(NSE, ','), opcode_constraint!(
                {
                    {
                        { field_name, zero_denied: _ },
                        bit_ranges
                    } == value
                }
            )] if bit_ranges.len() == 1
                && *bit_ranges[0].bits.start() != 0
                && bit_ranges[0].bits.start() == bit_ranges[0].bits.end()
                && value.content == "1" =>
            {
                for base in ISABase::complement(isas) {
                    instructions.push(Instruction {
                        isa_module: ISAModule { base, extension },
                        name: instruction_name.clone(),
                        fields: fields.clone(),
                    });
                }
                let mut found = false;
                for field in &mut fields {
                    match &mut field.field_def {
                        FieldDef {
                            body:
                                FieldDefBody::Slice(FieldDefSlice {
                                    name,
                                    bit_ranges: bit_ranges2,
                                }),
                            excluded_values,
                        } if name.matches_char_tokens(&field_name.content)
                            && bit_ranges2.len() == 1
                            && bit_ranges[0].bits == bit_ranges2[0].bits
                            && excluded_values.is_empty() =>
                        {
                            found = true;
                            field.field_def.body = FieldDefBody::LiteralNumber(ast::Number {
                                content: "0".into(),
                                pos: field_name.pos(),
                            });
                            break;
                        }
                        _ => {}
                    }
                }
                err_if!(!found, self, field_name, "couldn't find matching field");
                isas.to_vec()
            }
            [opcode_constraint!({ isas: isas1 }, ';'), opcode_constraint!({ isas: isas2 }), opcode_constraint!(HINT)] =>
            {
                for i in isas1 {
                    err_if!(
                        isas2.iter().find(|&v| v == i).is_some(),
                        self,
                        constraints[0].pos,
                        "duplicate ISA bit-count"
                    );
                }
                isas1.iter().chain(isas2).copied().collect()
            }
            [opcode_constraint!({ isas: isas1 }, ';'), opcode_constraint!({ isas: isas2 }), opcode_constraint!(RES)] =>
            {
                for i in isas1 {
                    err_if!(
                        isas2.iter().find(|&v| v == i).is_some(),
                        self,
                        constraints[0].pos,
                        "duplicate ISA bit-count"
                    );
                }
                isas1.to_vec()
            }
            [opcode_constraint!(HINT, ','), opcode_constraint!(
                {
                    {
                        { reg_field_name, zero_denied: _ },
                        reg_bit_ranges
                    } == reg_value
                }, ';'
            ), opcode_constraint!({ isas: isas }), opcode_constraint!(NSE, ','), opcode_constraint!(
                {
                    {
                        { imm_field_name, zero_denied: _ },
                        imm_bit_ranges
                    } == imm_value
                }
            )] if instruction_name.name == "C.SLLI"
                && reg_bit_ranges.is_empty()
                && reg_value.content == "0"
                && imm_bit_ranges.len() == 1
                && *imm_bit_ranges[0].bits.start() != 0
                && imm_bit_ranges[0].bits.start() == imm_bit_ranges[0].bits.end()
                && imm_value.content == "1" =>
            {
                for field in &mut fields {
                    if field
                        .field_def
                        .body
                        .find_matches_char_tokens(&reg_field_name.content)
                        .is_none()
                    {
                        continue;
                    }
                    match &*field.field_def.excluded_values {
                        [num] if num.content == "0" => {
                            field.field_def.excluded_values = Vec::new();
                        }
                        _ => err!(self, &field.field_def, "unexpected field value"),
                    }
                }
                for base in ISABase::complement(isas) {
                    instructions.push(Instruction {
                        isa_module: ISAModule { base, extension },
                        name: instruction_name.clone(),
                        fields: fields.clone(),
                    });
                }
                let mut found = false;
                for field in &mut fields {
                    match &mut field.field_def {
                        FieldDef {
                            body:
                                FieldDefBody::Slice(FieldDefSlice {
                                    name,
                                    bit_ranges: bit_ranges2,
                                }),
                            excluded_values,
                        } if name.matches_char_tokens(&imm_field_name.content)
                            && bit_ranges2.len() == 1
                            && imm_bit_ranges[0].bits == bit_ranges2[0].bits
                            && excluded_values.is_empty() =>
                        {
                            found = true;
                            field.field_def.body = FieldDefBody::LiteralNumber(ast::Number {
                                content: "0".into(),
                                pos: imm_field_name.pos(),
                            });
                            break;
                        }
                        _ => {}
                    }
                }
                err_if!(!found, self, imm_field_name, "couldn't find matching field");
                isas.to_vec()
            }
            [opcode_constraint!({ isas: isas1 }, ';'), opcode_constraint!({ isas: isas2 }), opcode_constraint!(HINT, ';'), opcode_constraint!(HINT, ','), opcode_constraint!(
                {
                    {
                        { field_name, zero_denied: _ },
                        bit_ranges
                    } == value
                }
            )] if instruction_name.name == "C.SLLI64"
                && bit_ranges.is_empty()
                && value.content == "0" =>
            {
                for field in &mut fields {
                    if field
                        .field_def
                        .body
                        .find_matches_char_tokens(&field_name.content)
                        .is_none()
                    {
                        continue;
                    }
                    match &*field.field_def.excluded_values {
                        [num] if num.content == "0" => {
                            field.field_def.excluded_values = Vec::new();
                        }
                        _ => err!(self, &field.field_def, "unexpected field value"),
                    }
                }
                for i in isas1 {
                    err_if!(
                        isas2.iter().find(|&v| v == i).is_some(),
                        self,
                        constraints[0].pos,
                        "duplicate ISA bit-count"
                    );
                }
                isas1.iter().chain(isas2).copied().collect()
            }
            [opcode_constraint!(RES, ','), opcode_constraint!(
                {
                    {
                        { field_name, zero_denied: _ },
                        bit_ranges
                    } == value
                }
            )] if bit_ranges.is_empty() && value.content == "0" => {
                let mut found = false;
                for field in &mut fields {
                    if field
                        .field_def
                        .body
                        .find_matches_char_tokens(&field_name.content)
                        .is_some()
                        && field.field_def.excluded_values.len() == 1
                        && field.field_def.excluded_values[0].content == "0"
                    {
                        found = true;
                        break;
                    }
                }
                err_if!(!found, self, field_name, "couldn't find matching field");
                ISABase::VALUES.to_vec()
            }
            _ => err!(self, constraints[0].pos, "unrecognized constraint list"),
        };
        for base in bases {
            instructions.push(Instruction {
                isa_module: ISAModule { base, extension },
                name: instruction_name.clone(),
                fields: fields.clone(),
            });
        }
        Ok(instructions)
    }
    fn parse_instruction_set_section(
        &self,
        tabular_env: &ast::Environment,
    ) -> Result<InstructionSetSection> {
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
        let mut row_iterator =
            TableRowIterator::new(self, tabular_body, &tabular_env.end, &column_definitions)
                .peekable();
        self.parse_blank_row(&mut row_iterator)?;
        self.parse_instruction_bit_ranges_row(&mut row_iterator, &column_definitions)?;
        let forms = self.parse_instruction_form_rows(&mut row_iterator, &column_definitions)?;
        let mut instructions = Vec::new();
        for row in row_iterator {
            let row = row?;
            match &*row {
                [ColumnRange {
                    body: Some(ColumnBody::Group(_)),
                    ..
                }, ..]
                | [ColumnRange { body: None, .. }, ColumnRange { body: None, .. }]
                | [ColumnRange {
                    body: Some(ColumnBody::ColumnBodyBoldText(_)),
                    ..
                }, ..] => continue,
                _ => instructions.extend(self.parse_instruction(row, &column_definitions)?),
            }
        }
        Ok(InstructionSetSection {
            forms,
            instructions,
        })
    }
    fn parse_instruction_set(&self, document: &ast::Document) -> Result<InstructionSet> {
        let mut sections = Vec::new();
        for table_env in document.content.iter().filter_map(ast::Token::environment) {
            let table_env = self.expect_environment(table_env, "table")?;
            let mut section = None;
            for small_env in table_env.body.iter().filter_map(ast::Token::environment) {
                let small_env = self.expect_environment(small_env, "small")?;
                for center_env in small_env.body.iter().filter_map(ast::Token::environment) {
                    let center_env = self.expect_environment(center_env, "center")?;
                    for tabular_env in center_env.body.iter().filter_map(ast::Token::environment) {
                        let tabular_env = self.expect_environment(tabular_env, "tabular")?;
                        err_if!(
                            section.is_some(),
                            self,
                            tabular_env,
                            "multiple `\\begin{{tabular}}` in single `\\begin{{table}}`"
                        );
                        section = Some(self.parse_instruction_set_section(tabular_env)?);
                    }
                }
            }
            sections.push(unwrap_or!(
                section,
                err!(
                    self,
                    table_env,
                    "missing `\\begin{{tabular}}` in this `\\begin{{table}}`"
                )
            ));
        }
        err_if!(sections.is_empty(), self, document, "no environment found");
        Ok(InstructionSet { sections })
    }
}

#[derive(Debug, Clone)]
pub struct InstructionSetSection {
    pub forms: Vec<InstructionForm>,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct InstructionSet {
    pub sections: Vec<InstructionSetSection>,
}

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
pub struct MergedColumnDefinition {
    pub column_range: Range<usize>,
    pub bit_range: InstBitRange,
}

#[derive(Debug, Clone)]
pub struct ColumnDefinition {
    pub kind: ColumnKind,
    pub merged_def: OnceCell<MergedColumnDefinition>,
}

#[derive(Debug, Clone)]
pub struct InstBit {
    pub pos: ast::Pos,
    pub bit: u32,
}

#[derive(Debug, Clone)]
pub struct InstBitRange {
    pub bits: RangeInclusive<u32>,
    pub end_bit_pos: ast::Pos,
    pub start_bit_pos: ast::Pos,
}

impl From<InstBit> for InstBitRange {
    fn from(v: InstBit) -> Self {
        Self {
            bits: v.bit..=v.bit,
            end_bit_pos: v.pos,
            start_bit_pos: v.pos,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FieldBitRange {
    pub bits: RangeInclusive<u32>,
    pub end_bit_pos: ast::Pos,
    pub start_bit_pos: Option<ast::Pos>,
}

#[derive(Clone, Debug)]
pub struct FieldZeroCondition {
    pub zero_denied: bool,
}

#[derive(Debug, Clone)]
pub enum FieldDefName {
    Macro(ast::MacroName),
    CharTokens(ast::CharTokens, FieldZeroCondition),
}

impl FieldDefName {
    pub fn matches_char_tokens(&self, search_for: &str) -> bool {
        match self {
            FieldDefName::CharTokens(v, _) if v.content == search_for => true,
            _ => false,
        }
    }
}

impl GetPos for FieldDefName {
    fn pos(&self) -> ast::Pos {
        match self {
            FieldDefName::Macro(v) => v.pos(),
            FieldDefName::CharTokens(v, _) => v.pos(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FieldDefSlice {
    pub name: FieldDefName,
    pub bit_ranges: Vec<FieldBitRange>,
}

impl GetPos for FieldDefSlice {
    fn pos(&self) -> ast::Pos {
        self.name.pos()
    }
}

#[derive(Debug, Clone)]
pub struct FieldDefAlternate {
    /// must be non-empty
    pub names: Vec<FieldDefName>,
}

impl GetPos for FieldDefAlternate {
    fn pos(&self) -> ast::Pos {
        self.names[0].pos()
    }
}

#[derive(Debug, Clone)]
pub struct Wildcard {
    /// must be non-empty
    pub pos: ast::Pos,
    pub content: String,
}

impl GetPos for Wildcard {
    fn pos(&self) -> ast::Pos {
        self.pos
    }
}

#[derive(Debug, Clone)]
pub enum FieldDefBody {
    Alternate(FieldDefAlternate),
    Slice(FieldDefSlice),
    Wildcard(Wildcard),
    LiteralNumber(ast::Number),
}

impl FieldDefBody {
    pub fn names(&self) -> &[FieldDefName] {
        match self {
            FieldDefBody::Alternate(v) => &v.names,
            FieldDefBody::Slice(v) => slice::from_ref(&v.name),
            FieldDefBody::Wildcard(_) => &[],
            FieldDefBody::LiteralNumber(_) => &[],
        }
    }
    pub fn names_mut(&mut self) -> &mut [FieldDefName] {
        match self {
            FieldDefBody::Alternate(v) => &mut v.names,
            FieldDefBody::Slice(v) => slice::from_mut(&mut v.name),
            FieldDefBody::Wildcard(_) => &mut [],
            FieldDefBody::LiteralNumber(_) => &mut [],
        }
    }
    pub fn find_matches_char_tokens(&self, search_for: &str) -> Option<&FieldDefName> {
        self.names()
            .iter()
            .find(|name| name.matches_char_tokens(search_for))
    }
}

impl GetPos for FieldDefBody {
    fn pos(&self) -> ast::Pos {
        match self {
            FieldDefBody::Alternate(v) => v.pos(),
            FieldDefBody::Slice(v) => v.pos(),
            FieldDefBody::Wildcard(v) => v.pos(),
            FieldDefBody::LiteralNumber(v) => v.pos(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub body: FieldDefBody,
    pub excluded_values: Vec<ast::Number>,
}

impl FieldDef {
    pub fn only_char_tokens(&self) -> Option<&ast::CharTokens> {
        if !self.excluded_values.is_empty() {
            return None;
        }
        match &self.body {
            FieldDefBody::Alternate(FieldDefAlternate { names }) => match &**names {
                [FieldDefName::CharTokens(char_tokens, _)] => Some(char_tokens),
                _ => None,
            },
            _ => None,
        }
    }
}

impl GetPos for FieldDef {
    fn pos(&self) -> ast::Pos {
        self.body.pos()
    }
}

#[derive(Debug, Clone)]
pub struct ColumnBodyText {
    pub pos: ast::Pos,
    pub tokens: Vec<ast::Token>,
}

#[derive(Debug, Clone)]
pub enum OpcodeCondition {}

#[derive(Debug, Clone)]
pub enum OpcodeNameFieldConstraintBody {
    ISAs(Vec<ISABase>),
    FieldCondition {
        field: FieldDefSlice,
        is_equality: bool,
        value: ast::Number,
    },
    Hint,
    ReservedForStandard,
    ReservedForCustom,
}

#[derive(Debug, Clone)]
pub struct OpcodeNameFieldConstraint {
    pub pos: ast::Pos,
    pub body: OpcodeNameFieldConstraintBody,
    pub separator: Option<ast::Punctuation>,
}

impl GetPos for OpcodeNameFieldConstraint {
    fn pos(&self) -> ast::Pos {
        self.pos
    }
}

#[derive(Debug, Clone)]
pub struct OpcodeNameField {
    pub pos: ast::Pos,
    pub name: String,
    pub constraints: Vec<OpcodeNameFieldConstraint>,
}

#[derive(Debug, Clone)]
pub struct ColumnBodyBoldText {
    pub pos: ast::Pos,
    pub tokens: Vec<ast::Token>,
}

#[derive(Debug, Clone)]
pub enum ColumnBody {
    InstBit(InstBit),
    InstBitRange(InstBitRange),
    FieldDef(FieldDef),
    ColumnBodyText(ColumnBodyText),
    OpcodeName(OpcodeNameField),
    ColumnBodyBoldText(ColumnBodyBoldText),
    Group(ast::Group),
}

impl GetPos for ColumnBody {
    fn pos(&self) -> ast::Pos {
        match self {
            ColumnBody::InstBit(v) => v.pos,
            ColumnBody::InstBitRange(v) => v.end_bit_pos, // end is first in text
            ColumnBody::FieldDef(v) => v.pos(),
            ColumnBody::ColumnBodyText(v) => v.pos,
            ColumnBody::OpcodeName(v) => v.pos,
            ColumnBody::ColumnBodyBoldText(v) => v.pos,
            ColumnBody::Group(v) => v.pos(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MultiColumnKind {
    Left,
    Right,
    Center,
}

impl MultiColumnKind {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "l" => Some(Self::Left),
            "c" => Some(Self::Center),
            "r" => Some(Self::Right),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MultiColumn {
    pub pos: ast::Pos,
    pub kind: MultiColumnKind,
    pub vertical_bar_on_left: bool,
    pub vertical_bar_on_right: bool,
}

#[derive(Debug, Clone)]
pub struct ColumnRange {
    pub pos: ast::Pos,
    pub indexes: Range<usize>,
    pub multi_column: Option<MultiColumn>,
    pub body: Option<ColumnBody>,
}

#[derive(Debug, Clone)]
pub struct InstructionField {
    pub instruction_bit_range: InstBitRange,
    pub field_def: FieldDef,
}

#[derive(Debug, Clone)]
pub struct InstructionFormName {
    pub pos: ast::Pos,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct InstructionForm {
    pub name: InstructionFormName,
    pub fields: Vec<InstructionField>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ISABase {
    RV32I,
    RV64I,
}

impl ISABase {
    pub const VALUES: &'static [Self] = &[ISABase::RV32I, ISABase::RV64I];
    pub fn complement(values: &[Self]) -> Vec<Self> {
        let mut found = [false; Self::VALUES.len()];
        for &value in values {
            for (index, &value2) in Self::VALUES.iter().enumerate() {
                if value == value2 {
                    found[index] = true;
                    break;
                }
            }
        }
        Self::VALUES
            .iter()
            .enumerate()
            .filter_map(|(index, &value)| if found[index] { None } else { Some(value) })
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ISAExtension {
    I,
    M,
    A,
    F,
    D,
    Q,
    C,
    Zifencei,
    Zicsr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ISAModule {
    pub base: ISABase,
    pub extension: ISAExtension,
}

#[derive(Debug, Clone)]
pub struct InstructionName {
    pub pos: ast::Pos,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub isa_module: ISAModule,
    pub name: InstructionName,
    pub fields: Vec<InstructionField>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InstrTableName {
    InstrTable,
    RvcInstrTable,
}

pub fn parse(
    file_name: &str,
    input: &str,
    instr_table_name: InstrTableName,
) -> Result<InstructionSet> {
    ast::Pos::call_with_input_context(InputContext { file_name, input }, || {
        let parser = Parser::new(file_name, input, instr_table_name);
        let document = tex_parser::parse(input)
            .map_err(|e| parser.err_line_col(e.location, format!("expected: {}", e.expected)))?;
        parser.parse_instruction_set(&document)
    })
}
