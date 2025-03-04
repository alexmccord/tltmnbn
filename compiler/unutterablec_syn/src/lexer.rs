use crate::cursor::Cursor;

use crate::scanunit::Operator as ScannedOp;
use crate::scanunit::*;

use crate::span::Position;
use crate::tok::Operator as OpTok;
use crate::tok::*;

pub fn tokenize(input: impl Into<String>) -> TokenVec {
    TokenVec::from_iter(Lexer::new(input))
}

#[derive(Debug)]
pub struct Lexer {
    cursor: Cursor,
    current_pos: Position,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Scan {
    Ok(Cmd),
    Err(Cmd),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Cmd {
    Append,
    Terminate,
}

fn map_tok<T, F>(res: Result<T, TokenKind>, f: F) -> TokenKind
where
    F: FnOnce(T) -> TokenKind,
{
    res.map_or_else(|tok| tok, |v| f(v))
}

impl Lexer {
    pub fn new(input: impl Into<String>) -> Lexer {
        Lexer {
            cursor: Cursor::new(input),
            current_pos: Position::new(1, 0),
        }
    }

    fn current_position(&self) -> Position {
        self.current_pos
    }

    fn advance_cursor(&mut self) {
        self.current_pos = match self.cursor.next() {
            Some(ScanUnit::Newline(_)) => Position::new(self.current_pos.line() + 1, 0),
            Some(_) => Position::new(self.current_pos.line(), self.current_pos.column() + 1),
            None => return,
        }
    }

    fn take_while<F>(&mut self, f: F) -> (bool, Result<(usize, usize), TokenKind>)
    where
        F: Fn(ScanUnit) -> Scan,
    {
        let ((), has_nonascii, result) = self.take_while_with((), |(), su| ((), f(su)));
        (has_nonascii, result)
    }

    fn take_while_with<T, F>(
        &mut self,
        mut state: T,
        f: F,
    ) -> (T, bool, Result<(usize, usize), TokenKind>)
    where
        F: Fn(T, ScanUnit) -> (T, Scan),
    {
        let i = self.cursor.byte_offset();
        let mut ok = true;
        let mut has_nonascii = false;

        // We call `self.advance_cursor()` here and now without the loop because
        // `self.next()` already checked what ScanUnit we have so we can skip that
        // redundant computation. But we have to do this after we get our `i`,
        // otherwise slicing will be off by one byte at the start.
        self.advance_cursor();

        loop {
            let su = self.cursor.get();
            let (s, res) = f(state, su);

            has_nonascii |= !su.is_ascii();
            state = s;

            let command = match res {
                Scan::Ok(cmd) => cmd,
                Scan::Err(cmd) => {
                    ok = false;
                    cmd
                }
            };

            let result = match command {
                Cmd::Append => {
                    self.advance_cursor();
                    continue;
                }
                Cmd::Terminate => {
                    let j = self.cursor.byte_offset();

                    if ok {
                        Ok((i, j))
                    } else {
                        let str = self.cursor.slice(i, j).to_string();
                        Err(TokenKind::Unknown(Unknown::new(str, has_nonascii)))
                    }
                }
            };

            return (state, has_nonascii, result);
        }
    }

    fn scan<F>(&mut self, f: F) -> (bool, Result<String, TokenKind>)
    where
        F: Fn(ScanUnit) -> Scan,
    {
        let ((), has_nonascii, result) = self.scan_with((), |(), su| ((), f(su)));
        (has_nonascii, result)
    }

    fn scan_with<T, F>(&mut self, state: T, f: F) -> (T, bool, Result<String, TokenKind>)
    where
        F: Fn(T, ScanUnit) -> (T, Scan),
    {
        let (state, has_nonascii, result) = self.take_while_with(state, f);

        match result {
            Ok((i, j)) => (state, has_nonascii, Ok(self.cursor.slice(i, j).to_string())),
            Err(tok) => (state, has_nonascii, Err(tok)),
        }
    }

    fn scan_unknown(&mut self) -> TokenKind {
        // We don't really _need_ for scan to return a Result
        // in this case, but... why duplicate logic? /shrug.
        let (has_nonascii, res) = self.scan(|c| match c {
            ScanUnit::Unrecognized(_) => Scan::Err(Cmd::Append),
            _ => Scan::Err(Cmd::Terminate),
        });

        map_tok(res, |str| {
            TokenKind::Unknown(Unknown::new(str, has_nonascii))
        })
    }

    fn scan_identifier(&mut self) -> TokenKind {
        let (_, res) = self.scan(|su| match su {
            ScanUnit::Alpha(_) => Scan::Ok(Cmd::Append),
            ScanUnit::Digit(_) => Scan::Ok(Cmd::Append),
            ScanUnit::Quot(_) => Scan::Ok(Cmd::Append), // TODO: enforce it to be postfix?
            ScanUnit::Operator(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Delimiter(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Space(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Newline(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Unrecognized(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Eof => Scan::Ok(Cmd::Terminate),
        });

        map_tok(res, |str| match Keyword::find(&str) {
            Some(kw) => TokenKind::Kw(kw),
            None => TokenKind::Ident(Ident(str)),
        })
    }

    fn scan_numeral(&mut self) -> TokenKind {
        let (_, res) = self.scan(|su| match su {
            ScanUnit::Alpha(_) => Scan::Err(Cmd::Append),
            ScanUnit::Digit(_) => Scan::Ok(Cmd::Append),
            ScanUnit::Quot(_) => Scan::Err(Cmd::Terminate),
            ScanUnit::Operator(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Delimiter(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Space(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Newline(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Unrecognized(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Eof => Scan::Ok(Cmd::Terminate),
        });

        map_tok(res, |str| TokenKind::Numeral(Numeral(str)))
    }

    fn scan_bytestring(&mut self, quot: Quotation) -> TokenKind {
        enum MachineState {
            // Used to have `Escaped` to escape `\n` but decided to remove it. I
            // realized it causes too many problems than it solves. Like, how
            // should it interact with the layout parsing? Does it include the
            // whitespace used for indentation? So, yeah, out it goes.
            //
            // This is used right now for escaping quotation marks and double
            // backslashes.
            //
            // 1. `\u{xxxx}`
            // 2. `\t`
            // 3. `\r`
            // 4. `\n`
            // 5. `\s`
            Escaped,
            Next,
            Finished,
        }

        use MachineState::*;

        let (s, is_nonascii, res) = self.scan_with((Next, true), |(ms, ok), su| match ms {
            Finished => ((Finished, ok), Scan::Ok(Cmd::Terminate)),
            Next => match su {
                ScanUnit::Quot(q) if q == quot => ((Finished, ok), Scan::Ok(Cmd::Append)),
                ScanUnit::Operator(ScannedOp('\\')) => ((Escaped, ok), Scan::Ok(Cmd::Append)),
                ScanUnit::Space(Space::Tab) => ((Next, false), Scan::Ok(Cmd::Append)),
                ScanUnit::Newline(_) => ((Next, ok), Scan::Err(Cmd::Terminate)),
                ScanUnit::Eof => ((Finished, ok), Scan::Err(Cmd::Terminate)),
                _ => ((Next, ok), Scan::Ok(Cmd::Append)),
            },
            Escaped => match su {
                ScanUnit::Alpha(Alpha('r')) => ((Next, ok), Scan::Ok(Cmd::Append)),
                ScanUnit::Alpha(Alpha('n')) => ((Next, ok), Scan::Ok(Cmd::Append)),
                ScanUnit::Alpha(Alpha('t')) => ((Next, ok), Scan::Ok(Cmd::Append)),
                ScanUnit::Alpha(Alpha('s')) => ((Next, ok), Scan::Ok(Cmd::Append)),
                ScanUnit::Alpha(_) => ((Next, ok), Scan::Err(Cmd::Append)),
                ScanUnit::Digit(_) => ((Next, ok), Scan::Err(Cmd::Append)),
                ScanUnit::Quot(_) => ((Next, ok), Scan::Ok(Cmd::Append)),
                ScanUnit::Operator(ScannedOp('\\')) => ((Next, ok), Scan::Ok(Cmd::Append)),
                ScanUnit::Operator(_) => ((Next, ok), Scan::Err(Cmd::Append)),
                ScanUnit::Delimiter(_) => ((Next, ok), Scan::Err(Cmd::Append)),
                ScanUnit::Space(Space::Space) => ((Next, ok), Scan::Err(Cmd::Append)),
                ScanUnit::Space(Space::Tab) => ((Next, false), Scan::Err(Cmd::Append)),
                ScanUnit::Newline(_) => ((Next, ok), Scan::Err(Cmd::Terminate)),
                ScanUnit::Unrecognized(_) => ((Next, ok), Scan::Err(Cmd::Append)),
                ScanUnit::Eof => ((Finished, ok), Scan::Err(Cmd::Terminate)),
            },
        });

        map_tok(res, |str| {
            TokenKind::ByteString(ByteString::from_quot(quot, str, !s.1 || is_nonascii))
        })
    }

    fn scan_operator(&mut self) -> TokenKind {
        let (_, res) = self.scan(|su| match su {
            ScanUnit::Alpha(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Digit(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Quot(_) => Scan::Err(Cmd::Terminate), // `<='` seems like a weird thing to support...
            ScanUnit::Operator(_) => Scan::Ok(Cmd::Append),
            ScanUnit::Delimiter(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Space(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Newline(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Unrecognized(_) => Scan::Err(Cmd::Terminate),
            ScanUnit::Eof => Scan::Ok(Cmd::Terminate),
        });

        map_tok(res, |str| TokenKind::Operator(OpTok(str)))
    }

    fn scan_delimiter(&mut self, delim: Delimiter) -> TokenKind {
        self.advance_cursor();

        match delim {
            Delimiter::Paren(g) => TokenKind::Group(Group::Paren(g.into())),
            Delimiter::Brace(g) => TokenKind::Group(Group::Brace(g.into())),
            Delimiter::Bracket(g) => TokenKind::Group(Group::Bracket(g.into())),
            Delimiter::Semicolon => TokenKind::Semicolon(Semicolon),
            Delimiter::Comma => TokenKind::Comma(Comma),
        }
    }

    fn scan_whitespaces(&mut self, space: Space) -> TokenKind {
        // We cluster whitespace tokens into a single one for efficiency.
        // Importantly, we'd only do this for as long as the whitespace is the
        // same as the one we started out with.
        let (_, res) = self.take_while(|su| match su {
            ScanUnit::Alpha(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Digit(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Quot(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Operator(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Delimiter(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Space(s) if s == space => Scan::Ok(Cmd::Append),
            ScanUnit::Space(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Newline(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Unrecognized(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Eof => Scan::Ok(Cmd::Terminate),
        });

        map_tok(res, |(i, j)| {
            TokenKind::Ws(match space {
                Space::Space => Ws::Space { count: j - i },
                Space::Tab => Ws::Tab { count: j - i },
            })
        })
    }

    fn scan_newlines(&mut self) -> TokenKind {
        let (_, res) = self.take_while(|su| match su {
            ScanUnit::Alpha(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Digit(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Quot(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Operator(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Delimiter(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Space(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Newline(_) => Scan::Ok(Cmd::Append),
            ScanUnit::Unrecognized(_) => Scan::Ok(Cmd::Terminate),
            ScanUnit::Eof => Scan::Ok(Cmd::Terminate),
        });

        map_tok(res, |(i, j)| TokenKind::Ws(Ws::Newline { count: j - i }))
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor.end() {
            return None;
        }

        let pos = self.current_position();

        // We don't call `next` or `advance_cursor` here yet because
        // we have to know the current index `i` to get a substring.
        // Advancing it now means the classic off by one error.
        let kind = match self.cursor.get() {
            ScanUnit::Unrecognized(_) => self.scan_unknown(),
            ScanUnit::Alpha(_) => self.scan_identifier(),
            ScanUnit::Digit(_) => self.scan_numeral(),
            ScanUnit::Quot(q) => self.scan_bytestring(q),
            ScanUnit::Operator(_) => self.scan_operator(),
            ScanUnit::Delimiter(delim) => self.scan_delimiter(delim),
            ScanUnit::Space(s) => self.scan_whitespaces(s),
            ScanUnit::Newline(_) => self.scan_newlines(),
            ScanUnit::Eof => {
                self.advance_cursor();
                TokenKind::Eof(Eof)
            }
        };

        Some(Token::new(kind, pos))
    }
}
