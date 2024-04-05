use std::collections::HashMap;

use crate::ErrorReporting;
use crate::Keyword;
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

    fn change_keyword(&mut self, token: &mut Token) {
        let keyword_map = HashMap::from([
            ("return", Keyword::Return),
            ("if", Keyword::If),
            ("else", Keyword::Else),
            ("for", Keyword::For),
            ("while", Keyword::While),
        ]);

        if let TokenKind::Ident { ref name } = token.kind {
            if let Some(keyword) = keyword_map.get(name.as_str()) {
                token.kind = TokenKind::Keyword {
                    keyword: keyword.clone(),
                }
            }
        }
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
                b'!' | b'='
                    | b'>'
                    | b'<'
                    | b'('
                    | b')'
                    | b'+'
                    | b'-'
                    | b'*'
                    | b'/'
                    | b';'
                    | b'{'
                    | b'}'
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
                        | Some(b'*') | Some(b'/') | Some(b';') | Some(b'=') | Some(b'{')
                        | Some(b'}'),
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
            } else if c.is_ascii_alphabetic() || c == b'_' {
                let start = self.index;
                let mut var = vec![c];
                self.index += 1;
                while self.peek().is_some_and(|c| {
                    c.is_ascii_alphabetic() || c.is_ascii_alphanumeric() || c == b'_'
                }) {
                    var.push(self.peek().unwrap());
                    self.index += 1;
                }

                let keyword_or_ident = String::from_utf8_lossy(&var).to_string();
                let mut token = Token {
                    offset: start,
                    length: var.len(),
                    kind: TokenKind::Ident {
                        name: keyword_or_ident,
                    },
                };

                self.change_keyword(&mut token);

                toks.push(token);
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
