// SPDX-License-Identifier: LGPL-2.1-or-later
// See Notices.txt for copyright information

use crate::tex::ast;
use ast::{input_context::InputContext, GetPos};
use once_cell::unsync::OnceCell;
use peg::{str::LineCol, Parse};
use std::{
    borrow::Cow,
    error::Error,
    fmt, mem,
    num::NonZeroUsize,
    ops::{Range, RangeInclusive},
    str::FromStr,
};

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
        pos_after_body: impl ast::GetPos,
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
    fn get_new_column_indexes(
        &mut self,
        new_column_width: usize,
        pos: impl ast::GetPos,
        reason: impl FnOnce() -> String,
    ) -> Result<Range<usize>> {
        let start_index = self.column_end_index();
        let cols_left = self.column_definitions.len() - start_index;
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
                    let body = self.parser.parse_column_body(
                        &mut self.tabular_body,
                        self.pos_after_body,
                        self.column_definitions,
                        true,
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
    fn new(file_name: &'input str, input: &'input str) -> Self {
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
        pos_if_token_is_none: impl ast::GetPos,
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
        pos_if_token_is_none: impl ast::GetPos,
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
            ast::Token::CharTokens(_) => true,
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
            token => todo!("token = {:?}", token),
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
    fn parse_field_def(
        &self,
        name: ast::CharTokens,
        tabular_body: &mut std::slice::Iter<ast::Token>,
        pos_after_body: impl ast::GetPos,
        column_definitions: &[ColumnDefinition],
        is_top_level: bool,
    ) -> Result<FieldDef> {
        let mut tabular_body_temp = tabular_body.clone();
        let mut bit_ranges = Vec::new();
        if let Some(ast::Token::Punctuation(ast::Punctuation { pos: _, ch: '[' })) =
            tabular_body_temp.next()
        {
            *tabular_body = tabular_body_temp;
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
                tabular_body_temp = tabular_body.clone();
                if let Some(ast::Token::Punctuation(ast::Punctuation { pos: _, ch: ':' })) =
                    tabular_body_temp.next()
                {
                    *tabular_body = tabular_body_temp;
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
                tabular_body_temp = tabular_body.clone();
                match unwrap_or!(
                    tabular_body_temp.next(),
                    err!(self, pos_after_body, "missing `]`")
                ) {
                    token if Self::is_column_body_ignored(token) => break,
                    ast::Token::Punctuation(ast::Punctuation { pos: _, ch: ']' }) => {
                        *tabular_body = tabular_body_temp;
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
                            *tabular_body = tabular_body_temp;
                            continue;
                        }
                        _ => err!(self, begin, "expected `$\\vert$`"),
                    },
                    token => err!(self, token, "expected `$\\vert$` or `]`"),
                }
            }
        }
        let bit_ranges = Some(bit_ranges).filter(|v| !v.is_empty());
        Ok(FieldDef { name, bit_ranges })
    }
    fn parse_column_body(
        &self,
        tabular_body: &mut std::slice::Iter<ast::Token>,
        pos_after_body: impl ast::GetPos,
        column_definitions: &[ColumnDefinition],
        is_top_level: bool,
    ) -> Result<Option<ColumnBody>> {
        loop {
            let mut tabular_body_peek = tabular_body.clone();
            let token = unwrap_or!(tabular_body_peek.next(), return Ok(None));
            dbg!(token);
            if Self::is_column_body_terminator(token, is_top_level) {
                return Ok(None);
            }
            if Self::is_column_body_ignored(token) {
                *tabular_body = tabular_body_peek;
                continue;
            }
            match token {
                ast::Token::Macro(macro_) if macro_.name.content == "cline" && is_top_level => {
                    *tabular_body = tabular_body_peek;
                    self.unwrap_group_or_error(
                        &pos_after_body,
                        tabular_body.next().map(Cow::Borrowed),
                    )?;
                    continue;
                }
                ast::Token::Macro(macro_) if macro_.name.content == "instbit" => {
                    *tabular_body = tabular_body_peek;
                    let (bit, pos) = self.parse_number_in_group(
                        pos_after_body,
                        tabular_body.next().map(Cow::Borrowed),
                        || "expected `{{<bit-number>}}` (like `{{12}}`)".into(),
                    )?;
                    self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                    return Ok(Some(ColumnBody::InstBit(InstBit { pos, bit })));
                }
                ast::Token::Macro(macro_) if macro_.name.content == "instbitrange" => {
                    *tabular_body = tabular_body_peek;
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
                    return Ok(Some(ColumnBody::InstBitRange(InstBitRange {
                        bits: start_bit..=end_bit,
                        end_bit_pos,
                        start_bit_pos,
                    })));
                }
                ast::Token::Macro(macro_) if macro_.name.content == "bf" => {
                    let mut tokens: Vec<ast::Token> = vec![token.clone()];
                    *tabular_body = tabular_body_peek.clone();
                    while let Some(token) = tabular_body_peek
                        .next()
                        .filter(|token| Self::is_column_body_bold_text(token))
                    {
                        *tabular_body = tabular_body_peek.clone();
                        tokens.push(token.clone());
                    }
                    self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                    return Ok(Some(ColumnBody::ColumnBodyBoldText(ColumnBodyBoldText {
                        pos: tokens[0].pos(),
                        tokens,
                    })));
                }
                token if Self::is_column_body_text(token) => {
                    let mut tokens: Vec<ast::Token> = vec![token.clone()];
                    *tabular_body = tabular_body_peek.clone();
                    while let Some(token) = tabular_body_peek
                        .next()
                        .filter(|token| Self::is_column_body_text(token))
                    {
                        *tabular_body = tabular_body_peek.clone();
                        tokens.push(token.clone());
                    }
                    if let [ast::Token::CharTokens(_)] = &*tokens {
                        let name = tokens.pop().unwrap().into_char_tokens().unwrap();
                        let field_def = self.parse_field_def(
                            name,
                            tabular_body,
                            pos_after_body,
                            column_definitions,
                            is_top_level,
                        )?;
                        self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                        return Ok(Some(ColumnBody::FieldDef(field_def)));
                    }
                    self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                    return Ok(Some(ColumnBody::ColumnBodyText(ColumnBodyText {
                        pos: tokens[0].pos(),
                        tokens,
                    })));
                }
                ast::Token::Number(num) => {
                    *tabular_body = tabular_body_peek.clone();
                    self.skip_rest_of_column_body_else_error(tabular_body, is_top_level)?;
                    return Ok(Some(ColumnBody::LiteralNumber(num.clone())));
                }
                t => todo!("token={:?}", t),
            }
            unreachable!()
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
        let column_definitions = &column_definitions[1..column_definitions.len() - 1];
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
            columns.len() != column_definitions.len(),
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
                (ColumnBody::InstBit(v), None) => v.clone().into(),
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
            let merged_def = MergedColumnDefinition {
                column_range: column_range.clone(),
                bit_range,
            };
            for c in &column_definitions[column_range.clone()] {
                c.merged_def.set(merged_def.clone()).ok().unwrap();
            }
            index = column_range.end;
        }
        Ok(())
    }
    fn parse_instruction_form_rows(
        &self,
        row_iterator: &mut PeekableTableRowIterator,
        column_definitions: &[ColumnDefinition],
    ) -> Result<Vec<InstructionForm>> {
        dbg!(row_iterator.collect::<Vec<_>>());
        todo!()
    }
    fn parse_tabular_env<'env>(&self, tabular_env: &'env ast::Environment) -> Result<()> {
        let mut tabular_body = tabular_env.body.iter();
        let mut first_token = tabular_body.next();
        match first_token.and_then(|t| Some(&*t.char_tokens()?.content)) {
            Some(pos) if matches!(pos, "t" | "b" | "c") => {
                first_token = None;
            }
            _ => {}
        }
        let mut column_definitions = self.parse_tabular_column_definitions(unwrap_or!(
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
        let mut row_iterator =
            TableRowIterator::new(self, tabular_body, &tabular_env.end, &column_definitions)
                .peekable();
        self.parse_blank_row(&mut row_iterator)?;
        self.parse_instruction_bit_ranges_row(&mut row_iterator, &column_definitions)?;
        dbg!(&column_definitions);
        let instruction_forms =
            self.parse_instruction_form_rows(&mut row_iterator, &column_definitions)?;
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

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub name: ast::CharTokens,
    pub bit_ranges: Option<Vec<FieldBitRange>>,
}

#[derive(Debug, Clone)]
pub struct ColumnBodyText {
    pub pos: ast::Pos,
    pub tokens: Vec<ast::Token>,
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
    ColumnBodyBoldText(ColumnBodyBoldText),
    LiteralNumber(ast::Number),
}

impl GetPos for ColumnBody {
    fn pos(&self) -> ast::Pos {
        match self {
            ColumnBody::InstBit(v) => v.pos,
            ColumnBody::InstBitRange(v) => v.end_bit_pos, // end is first in text
            ColumnBody::FieldDef(v) => v.name.pos(),
            ColumnBody::ColumnBodyText(v) => v.pos,
            ColumnBody::ColumnBodyBoldText(v) => v.pos,
            ColumnBody::LiteralNumber(v) => v.pos(),
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
pub struct InstructionForm {
    pub pos: ast::Pos,
    // TODO: finish
}

pub fn parse(file_name: &str, input: &str) -> Result<InstructionSet> {
    ast::Pos::call_with_input_context(InputContext { file_name, input }, || {
        let parser = Parser::new(file_name, input);
        let document = crate::tex::parse(input)
            .map_err(|e| parser.err_line_col(e.location, format!("expected: {}", e.expected)))?;
        parser.parse_instruction_set(&document)
    })
}

#[cfg(test)]
mod test {

    #[test]
    fn test_parse_instr_table() {
        super::parse(
            "instr-table.tex",
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../riscv-isa-manual/src/instr-table.tex"
            )),
        )
        .unwrap();
    }
}
