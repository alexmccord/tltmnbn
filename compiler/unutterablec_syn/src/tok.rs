//! This module is intended to be glob imported as
//! ```
//! use unutterablec::syn::tok::*;
//! ```

use std::ops::{Deref, DerefMut};

use unutterablec_idx;
use unutterablec_idx::{Idx, IndexedVec};

use crate::offside::{Measured, OffsideTape};
use crate::scanunit::{Gate, Quotation};
use crate::span::Position;

unutterablec_idx::newindex!(pub TokenId);

#[derive(Debug)]
pub struct TokenVec {
    inner: IndexedVec<TokenId, Token>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    kind: TokenKind,
    pos: Position,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Unknown(Unknown),
    Kw(Keyword),
    Ident(Ident),
    Numeral(Numeral),
    ByteString(ByteString),
    Operator(Operator),
    Group(Group),
    Ws(Ws),
    Semicolon(Semicolon),
    Comma(Comma),
    Eof(Eof),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Unknown,
    Kw,
    Ident,
    Numeral,
    ByteString,
    ByteChar,
    Operator,
    Paren(Parity),
    Brace(Parity),
    Bracket(Parity),
    Space,
    Tab,
    Newline,
    Semicolon,
    Comma,
    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TokenBuf {
    str: String,
    is_whitespace_insignificant: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Unknown(TokenBuf);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Keyword {
    // Module system
    Module,
    Import,
    Export,
    Public,
    Open,
    Hiding,
    Renaming,

    // Product and sum types
    Record,
    Data,
    Deriving,

    // Trait stuff
    Trait,
    Impl,

    // Syntactic delimiters
    Where,

    // Expressions
    Let,
    In,
    Do,
    If,
    Then,
    Else,
    Function,
    Match,
    With,
    Forall,
    Exists,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ident(pub String);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Numeral(pub String);

// Lexer lexes this into a raw representation of a string literal as it exists
// in the source code. It is not the responsibility of the lexer to parse it out
// to give it the actual semantic meaning.
//
// In fact, we want to prevent this from happening at the compiler level
// entirely. This should be pushed down to the language level. It does mean that
// string literals have no principal types, but we already have that problem in
// other cases anyway.
//
// Be aware of the following:
// 1. this includes the quotation marks in all cases.
// 2. in the case of ByteChar, the length of the string is still arbitrary.
// 3. using `len()` returns the number of bytes of the source code, not the span.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ByteString {
    ByteString(TokenBuf),
    ByteChar(TokenBuf),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Operator(pub String);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Parity {
    Open,
    Closed,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Group {
    Paren(Parity),
    Brace(Parity),
    Bracket(Parity),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Ws {
    Space { count: usize },
    Tab { count: usize },
    Newline { count: usize },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Semicolon;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Comma;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Eof;

impl TokenVec {
    pub fn new() -> TokenVec {
        TokenVec {
            inner: IndexedVec::new(),
        }
    }

    pub fn get_pos(&self, id: TokenId) -> (Position, Position) {
        let begin = self.inner[id].pos;
        let end = self.inner.get(id.increment(1)).map_or(begin, |tok| tok.pos);
        (begin, end)
    }
}

impl Deref for TokenVec {
    type Target = IndexedVec<TokenId, Token>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for TokenVec {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl FromIterator<Token> for TokenVec {
    fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
        TokenVec {
            inner: IndexedVec::from_iter(iter),
        }
    }
}

impl Token {
    pub(crate) fn new(kind: TokenKind, pos: Position) -> Token {
        Token { kind, pos }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn ty(&self) -> TokenType {
        self.kind.ty()
    }

    pub fn is(&self, ty: TokenType) -> bool {
        self.kind.is(ty)
    }
}

impl TokenKind {
    pub fn ty(&self) -> TokenType {
        match self {
            TokenKind::Unknown(u) => u.token_type(),
            TokenKind::Kw(kw) => kw.token_type(),
            TokenKind::Ident(i) => i.token_type(),
            TokenKind::Numeral(n) => n.token_type(),
            TokenKind::ByteString(bs) => bs.token_type(),
            TokenKind::Operator(op) => op.token_type(),
            TokenKind::Group(g) => g.token_type(),
            TokenKind::Ws(ws) => ws.token_type(),
            TokenKind::Semicolon(s) => s.token_type(),
            TokenKind::Comma(c) => c.token_type(),
            TokenKind::Eof(eof) => eof.token_type(),
        }
    }

    pub fn is(&self, ty: TokenType) -> bool {
        self.ty() == ty
    }
}

impl TokenBuf {
    pub fn new(str: String, is_whitespace_insignificant: bool) -> TokenBuf {
        TokenBuf {
            str,
            is_whitespace_insignificant,
        }
    }

    pub fn as_str(&self) -> &str {
        &self.str
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl Unknown {
    pub fn new(str: String, is_whitespace_insignificant: bool) -> Unknown {
        Unknown(TokenBuf::new(str, is_whitespace_insignificant))
    }

    pub fn token_type(&self) -> TokenType {
        TokenType::Unknown
    }

    pub fn is_whitespace_insignificant(&self) -> bool {
        self.0.is_whitespace_insignificant
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl Keyword {
    pub fn token_type(&self) -> TokenType {
        TokenType::Kw
    }

    pub fn find(s: impl AsRef<str>) -> Option<Keyword> {
        match s.as_ref() {
            "module" => Some(Keyword::Module),
            "import" => Some(Keyword::Import),
            "export" => Some(Keyword::Export),
            "public" => Some(Keyword::Public),
            "open" => Some(Keyword::Open),
            "hiding" => Some(Keyword::Hiding),
            "renaming" => Some(Keyword::Renaming),
            "record" => Some(Keyword::Record),
            "data" => Some(Keyword::Data),
            "trait" => Some(Keyword::Trait),
            "impl" => Some(Keyword::Impl),
            "deriving" => Some(Keyword::Deriving),
            "where" => Some(Keyword::Where),
            "let" => Some(Keyword::Let),
            "in" => Some(Keyword::In),
            "do" => Some(Keyword::Do),
            "if" => Some(Keyword::If),
            "then" => Some(Keyword::Then),
            "else" => Some(Keyword::Else),
            "function" => Some(Keyword::Function),
            "match" => Some(Keyword::Match),
            "with" => Some(Keyword::With),
            "forall" => Some(Keyword::Forall),
            "exists" => Some(Keyword::Exists),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Keyword::Module => "module",
            Keyword::Import => "import",
            Keyword::Export => "export",
            Keyword::Public => "public",
            Keyword::Open => "open",
            Keyword::Hiding => "hiding",
            Keyword::Renaming => "renaming",
            Keyword::Record => "record",
            Keyword::Data => "data",
            Keyword::Deriving => "deriving",
            Keyword::Trait => "trait",
            Keyword::Impl => "impl",
            Keyword::Where => "where",
            Keyword::Let => "let",
            Keyword::In => "in",
            Keyword::Do => "do",
            Keyword::If => "if",
            Keyword::Then => "then",
            Keyword::Else => "else",
            Keyword::Function => "function",
            Keyword::Match => "match",
            Keyword::With => "with",
            Keyword::Forall => "forall",
            Keyword::Exists => "exists",
        }
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl Ident {
    pub fn token_type(&self) -> TokenType {
        TokenType::Ident
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl Numeral {
    pub fn token_type(&self) -> TokenType {
        TokenType::Numeral
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl ByteString {
    pub fn new_bytestring(str: impl Into<String>, is_whitespace_insignificant: bool) -> ByteString {
        ByteString::ByteString(TokenBuf::new(str.into(), is_whitespace_insignificant))
    }

    pub fn new_bytechar(str: impl Into<String>, is_whitespace_insignificant: bool) -> ByteString {
        ByteString::ByteChar(TokenBuf::new(str.into(), is_whitespace_insignificant))
    }

    pub fn token_type(&self) -> TokenType {
        match self {
            ByteString::ByteString(..) => TokenType::ByteString,
            ByteString::ByteChar(..) => TokenType::ByteChar,
        }
    }

    pub fn from_quot(
        quot: Quotation,
        str: impl Into<String>,
        is_whitespace_insignificant: bool,
    ) -> ByteString {
        match quot {
            Quotation::Single => ByteString::new_bytechar(str, is_whitespace_insignificant),
            Quotation::Double => ByteString::new_bytestring(str, is_whitespace_insignificant),
        }
    }

    pub fn is_whitespace_insignificant(&self) -> bool {
        match self {
            ByteString::ByteString(token_buf) => token_buf.is_whitespace_insignificant,
            ByteString::ByteChar(token_buf) => token_buf.is_whitespace_insignificant,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            ByteString::ByteString(token_buf) => token_buf.as_str(),
            ByteString::ByteChar(token_buf) => token_buf.as_str(),
        }
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl Operator {
    pub fn token_type(&self) -> TokenType {
        TokenType::Operator
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl Parity {
    pub fn get_dual(self) -> Parity {
        match self {
            Parity::Open => Parity::Closed,
            Parity::Closed => Parity::Open,
        }
    }
}

impl Group {
    pub fn token_type(&self) -> TokenType {
        match self {
            Group::Paren(parity) => TokenType::Paren(*parity),
            Group::Brace(parity) => TokenType::Brace(*parity),
            Group::Bracket(parity) => TokenType::Bracket(*parity),
        }
    }

    pub fn get_dual(self) -> Group {
        match self {
            Group::Paren(p) => Group::Paren(p.get_dual()),
            Group::Brace(p) => Group::Brace(p.get_dual()),
            Group::Bracket(p) => Group::Bracket(p.get_dual()),
        }
    }

    fn as_str(&self) -> &str {
        match self {
            Group::Paren(Parity::Open) => "(",
            Group::Paren(Parity::Closed) => ")",
            Group::Brace(Parity::Open) => "{",
            Group::Brace(Parity::Closed) => "}",
            Group::Bracket(Parity::Open) => "[",
            Group::Bracket(Parity::Closed) => "]",
        }
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl Ws {
    pub fn token_type(&self) -> TokenType {
        match self {
            Ws::Space { .. } => TokenType::Space,
            Ws::Tab { .. } => TokenType::Tab,
            Ws::Newline { .. } => TokenType::Newline,
        }
    }

    pub fn to_string(&self) -> String {
        let str = match self {
            Ws::Space { .. } => " ",
            Ws::Tab { .. } => "\t",
            Ws::Newline { .. } => "\n",
        };

        str.repeat(self.len())
    }

    pub fn len(&self) -> usize {
        match self {
            Ws::Space { count } => *count,
            Ws::Tab { count } => *count,
            Ws::Newline { count } => *count,
        }
    }
}

impl Semicolon {
    pub fn token_type(&self) -> TokenType {
        TokenType::Semicolon
    }

    pub fn as_str(&self) -> &str {
        ";"
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl Comma {
    pub fn token_type(&self) -> TokenType {
        TokenType::Comma
    }

    pub fn as_str(&self) -> &str {
        ","
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl Eof {
    pub fn token_type(&self) -> TokenType {
        TokenType::Eof
    }

    pub fn as_str(&self) -> &str {
        "\0"
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

impl OffsideTape for Token {
    fn measure(&self) -> Option<Measured> {
        self.kind().measure()
    }
}

impl OffsideTape for TokenKind {
    fn measure(&self) -> Option<Measured> {
        match self {
            TokenKind::Unknown(u) => u.measure(),
            TokenKind::Kw(kw) => kw.measure(),
            TokenKind::Ident(ident) => ident.measure(),
            TokenKind::Numeral(num) => num.measure(),
            TokenKind::ByteString(byte_string) => byte_string.measure(),
            TokenKind::Operator(op) => op.measure(),
            TokenKind::Group(g) => g.measure(),
            TokenKind::Ws(ws) => ws.measure(),
            TokenKind::Semicolon(semi) => semi.measure(),
            TokenKind::Comma(comma) => comma.measure(),
            TokenKind::Eof(eof) => eof.measure(),
        }
    }
}

impl OffsideTape for TokenBuf {
    fn measure(&self) -> Option<Measured> {
        if self.is_whitespace_insignificant {
            return None;
        }

        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for Unknown {
    fn measure(&self) -> Option<Measured> {
        self.0.measure()
    }
}

impl OffsideTape for Keyword {
    fn measure(&self) -> Option<Measured> {
        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for Ident {
    fn measure(&self) -> Option<Measured> {
        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for Numeral {
    fn measure(&self) -> Option<Measured> {
        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for ByteString {
    fn measure(&self) -> Option<Measured> {
        match self {
            ByteString::ByteString(token_buf) => token_buf.measure(),
            ByteString::ByteChar(token_buf) => token_buf.measure(),
        }
    }
}

impl OffsideTape for Operator {
    fn measure(&self) -> Option<Measured> {
        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for Group {
    fn measure(&self) -> Option<Measured> {
        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for Ws {
    fn measure(&self) -> Option<Measured> {
        match self {
            Ws::Space { count } => Some(Measured::Monospace(*count as u16)),
            Ws::Tab { count } => Some(Measured::Elastic(*count as u16)),
            Ws::Newline { .. } => Some(Measured::Retract),
        }
    }
}

impl OffsideTape for Semicolon {
    fn measure(&self) -> Option<Measured> {
        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for Comma {
    fn measure(&self) -> Option<Measured> {
        Some(Measured::Monospace(self.len() as u16))
    }
}

impl OffsideTape for Eof {
    fn measure(&self) -> Option<Measured> {
        Some(Measured::Retract)
    }
}

impl From<Gate> for Parity {
    fn from(value: Gate) -> Self {
        match value {
            Gate::Open => Parity::Open,
            Gate::Closed => Parity::Closed,
        }
    }
}

impl From<Unknown> for TokenKind {
    fn from(value: Unknown) -> Self {
        TokenKind::Unknown(value)
    }
}

impl From<Keyword> for TokenKind {
    fn from(value: Keyword) -> Self {
        TokenKind::Kw(value)
    }
}

impl From<Ident> for TokenKind {
    fn from(value: Ident) -> Self {
        TokenKind::Ident(value)
    }
}

impl From<Numeral> for TokenKind {
    fn from(value: Numeral) -> Self {
        TokenKind::Numeral(value)
    }
}

impl From<ByteString> for TokenKind {
    fn from(value: ByteString) -> Self {
        TokenKind::ByteString(value)
    }
}

impl From<Operator> for TokenKind {
    fn from(value: Operator) -> Self {
        TokenKind::Operator(value)
    }
}

impl From<Group> for TokenKind {
    fn from(value: Group) -> Self {
        TokenKind::Group(value)
    }
}

impl From<Ws> for TokenKind {
    fn from(value: Ws) -> Self {
        TokenKind::Ws(value)
    }
}

impl From<Semicolon> for TokenKind {
    fn from(value: Semicolon) -> Self {
        TokenKind::Semicolon(value)
    }
}

impl From<Comma> for TokenKind {
    fn from(value: Comma) -> Self {
        TokenKind::Comma(value)
    }
}

impl From<Eof> for TokenKind {
    fn from(value: Eof) -> Self {
        TokenKind::Eof(value)
    }
}

impl PartialEq<TokenKind> for Unknown {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::Unknown(u) if self == u)
    }
}

impl PartialEq<Unknown> for TokenKind {
    fn eq(&self, other: &Unknown) -> bool {
        other == self
    }
}

impl PartialEq<TokenKind> for Keyword {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::Kw(kw) if self == kw)
    }
}

impl PartialEq<Keyword> for TokenKind {
    fn eq(&self, other: &Keyword) -> bool {
        other == self
    }
}

impl PartialEq<TokenKind> for Ident {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::Ident(ident) if self == ident)
    }
}

impl PartialEq<Ident> for TokenKind {
    fn eq(&self, other: &Ident) -> bool {
        other == self
    }
}

impl PartialEq<TokenKind> for Numeral {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::Numeral(numeral) if self == numeral)
    }
}

impl PartialEq<Numeral> for TokenKind {
    fn eq(&self, other: &Numeral) -> bool {
        other == self
    }
}

impl PartialEq<TokenKind> for ByteString {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::ByteString(bytestring) if self == bytestring)
    }
}

impl PartialEq<ByteString> for TokenKind {
    fn eq(&self, other: &ByteString) -> bool {
        other == self
    }
}

impl PartialEq<TokenKind> for Operator {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::Operator(operator) if self == operator)
    }
}

impl PartialEq<Operator> for TokenKind {
    fn eq(&self, other: &Operator) -> bool {
        other == self
    }
}

impl PartialEq<TokenKind> for Group {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::Group(group) if self == group)
    }
}

impl PartialEq<Group> for TokenKind {
    fn eq(&self, other: &Group) -> bool {
        other == self
    }
}

impl PartialEq<TokenKind> for Ws {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::Ws(ws) if self == ws)
    }
}

impl PartialEq<Ws> for TokenKind {
    fn eq(&self, other: &Ws) -> bool {
        other == self
    }
}

impl PartialEq<TokenKind> for Semicolon {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::Semicolon(semi) if self == semi)
    }
}

impl PartialEq<Semicolon> for TokenKind {
    fn eq(&self, other: &Semicolon) -> bool {
        other == self
    }
}

impl PartialEq<TokenKind> for Comma {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::Comma(comma) if self == comma)
    }
}

impl PartialEq<Comma> for TokenKind {
    fn eq(&self, other: &Comma) -> bool {
        other == self
    }
}

impl PartialEq<TokenKind> for Eof {
    fn eq(&self, other: &TokenKind) -> bool {
        matches!(other, TokenKind::Eof(eof) if self == eof)
    }
}

impl PartialEq<Eof> for TokenKind {
    fn eq(&self, other: &Eof) -> bool {
        other == self
    }
}
