use crate::tok::TokenId;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position {
    line: usize,
    column: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SourceSpan {
    pub begin: Position,
    pub end: Position,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TokenSpan {
    pub begin: TokenId,
    pub end: TokenId,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Position {
        Position { line, column }
    }

    /// Line begins at 1.
    pub fn line(self) -> usize {
        self.line
    }

    /// Column begins at 0.
    pub fn column(self) -> usize {
        self.column
    }
}

impl SourceSpan {
    pub fn new(begin: Position, end: Position) -> SourceSpan {
        SourceSpan { begin, end }
    }
}

impl TokenSpan {
    pub fn new(begin: TokenId, end: TokenId) -> TokenSpan {
        TokenSpan { begin, end }
    }
}
