use std::str::Chars;

use crate::scanunit::*;

#[derive(Debug)]
pub struct Cursor {
    input: String,
    byte_offset: usize,
}

impl Cursor {
    pub fn new(input: impl Into<String>) -> Cursor {
        Cursor {
            input: input.into(),
            byte_offset: 0,
        }
    }

    pub fn end(&self) -> bool {
        self.byte_offset > self.input.len()
    }

    pub fn byte_offset(&self) -> usize {
        self.byte_offset
    }

    pub fn slice(&self, i: usize, j: usize) -> &str {
        &self.input[i..j]
    }

    fn chars(&self) -> Chars {
        self.input
            .split_at(self.byte_offset.min(self.input.len()))
            .1
            .chars()
    }

    pub fn skip_mut(&mut self, n: usize) {
        self.byte_offset += n;
    }

    pub fn peek(&self, peek_offset: usize) -> ScanUnit {
        // The guts of the parser. Smelly as hell, in the sewers it belongs.
        let mut chars = self.chars().skip(peek_offset);

        match chars.next() {
            None => ScanUnit::Eof,
            Some('\r') => match chars.next() {
                Some('\n') => ScanUnit::Newline(Newline::CRLF),
                _ => ScanUnit::Newline(Newline::CR),
            },
            Some('\n') => ScanUnit::Newline(Newline::LF),
            Some(c @ ('a'..='z' | 'A'..='Z')) => ScanUnit::Alpha(Alpha(c)),
            Some(
                c @ ('~' | '&' | '|' | '^' | '%' | '*' | '-' | '+' | '/' | '\\' | '=' | ':' | '!'
                | '?' | '.' | '<' | '>'),
            ) => ScanUnit::Operator(Operator(c)),
            Some(c) => {
                if let Some(d) = Digit::from_char(c) {
                    ScanUnit::Digit(d)
                } else if let Some(d) = Delimiter::from_char(c) {
                    ScanUnit::Delimiter(d)
                } else if let Some(q) = Quotation::from_char(c) {
                    ScanUnit::Quot(q)
                } else if let Some(s) = Space::from_char(c) {
                    ScanUnit::Space(s)
                } else {
                    ScanUnit::Unrecognized(Unrecognized(c))
                }
            }
        }
    }

    pub fn get(&self) -> ScanUnit {
        self.peek(0)
    }
}

impl Iterator for Cursor {
    type Item = ScanUnit;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.get();
        self.byte_offset += c.byte_len();
        match c {
            ScanUnit::Eof => None,
            c => Some(c),
        }
    }
}
