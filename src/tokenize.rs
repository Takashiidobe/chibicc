use crate::ErrorReporting;
use crate::Token;
use crate::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub struct Lexer<'a> {
    src: &'a [u8],
    index: usize,
}

impl<'a> ErrorReporting for Lexer<'a> {
    fn src(&self) -> &[u8] {
        self.src
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a [u8]) -> Self {
        Self { src, index: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.src.len()
    }

    fn peek(&self) -> Option<u8> {
        if self.is_at_end() {
            None
        } else {
            Some(self.src[self.index])
        }
    }

    fn peek_next(&self) -> Option<u8> {
        if self.index + 1 >= self.src.len() {
            None
        } else {
            Some(self.src[self.index + 1])
        }
    }

    fn number(&mut self) -> i32 {
        let mut buf = vec![];

        while !self.is_at_end() && self.src[self.index].is_ascii_digit() {
            buf.push(self.src[self.index]);
            self.index += 1;
        }

        String::from_utf8_lossy(&buf).parse().unwrap()
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut toks = Vec::new();
        let buf = self.src;

        while self.index < buf.len() {
            let c = self.peek().unwrap();

            if c.is_ascii_whitespace() {
                self.index += 1;
            } else if c.is_ascii_digit() {
                let start = self.index;
                let val = self.number();
                let end = self.index;
                if start == end {
                    self.error_at(self.index, "expected number")
                }
                toks.push(Token {
                    offset: start,
                    length: end - start,
                    kind: TokenKind::Num { val },
                });
            } else if matches!(
                c,
                b'!' | b'=' | b'>' | b'<' | b'(' | b')' | b'+' | b'-' | b'*' | b'/' | b';'
            ) {
                match (self.peek(), self.peek_next()) {
                    (Some(b'!') | Some(b'=') | Some(b'>') | Some(b'<'), Some(b'=')) => {
                        toks.push(Token {
                            offset: self.index,
                            length: 2,
                            kind: TokenKind::Punct,
                        });
                        self.index += 2;
                    }
                    (
                        Some(b'<') | Some(b'>') | Some(b'(') | Some(b')') | Some(b'+') | Some(b'-')
                        | Some(b'*') | Some(b'/') | Some(b';'),
                        _,
                    ) => {
                        toks.push(Token {
                            offset: self.index,
                            length: 1,
                            kind: TokenKind::Punct,
                        });
                        self.index += 1;
                    }
                    _ => {}
                }
            } else {
                dbg!(String::from_utf8_lossy(&self.src[self.index..]));
                self.error_at(self.index, "invalid token")
            }
        }

        toks.push(Token {
            offset: self.index,
            length: 0,
            kind: TokenKind::Eof,
        });
        toks
    }
}