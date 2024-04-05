use std::collections::HashMap;

use crate::ErrorReporting;
use crate::Node;
use crate::NodeKind;
use crate::Token;
use crate::TokenKind;
use crate::P;

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    src: &'a [u8],
    toks: &'a [Token],
    tok_index: usize,
    vars: HashMap<String, usize>,
}

impl<'a> ErrorReporting for Parser<'a> {
    fn src(&self) -> &[u8] {
        self.src
    }
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a [u8], toks: &'a [Token]) -> Self {
        if toks.is_empty() {
            panic!("Empty token array")
        }
        Self {
            src,
            toks,
            tok_index: 0,
            vars: HashMap::default(),
        }
    }

    pub fn parse(&mut self) -> (Vec<Node>, HashMap<String, usize>) {
        let mut statements = vec![];
        while !self.is_done() {
            statements.push(self.stmt());
        }
        (statements, self.vars.clone())
    }

    // expr = assign
    fn expr(&mut self) -> Node {
        self.assign()
    }

    // stmt = "return expr ";"
    //      | expr-stmt
    fn stmt(&mut self) -> Node {
        if self.r#match("return") {
            self.advance();
            let node = self.expr();
            self.skip(";");

            return Node {
                kind: NodeKind::Return { lhs: P::new(node) },
            };
        }
        self.expr_stmt()
    }

    // expr-stmt = expr ";"
    fn expr_stmt(&mut self) -> Node {
        let node = self.expr();
        self.skip(";");
        node
    }

    // assign = equality ("=" assign)?
    fn assign(&mut self) -> Node {
        let mut node = self.equality();

        if self.r#match("=") {
            self.advance();
            node = Node {
                kind: NodeKind::Assign {
                    lhs: P::new(node),
                    rhs: P::new(self.assign()),
                },
            };
        }

        node
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Node {
        let mut node = self.relational();

        loop {
            if self.r#match("==") {
                self.advance();
                node = Node {
                    kind: NodeKind::Eq {
                        lhs: P::new(node),
                        rhs: P::new(self.relational()),
                    },
                };
            } else if self.r#match("!=") {
                self.advance();
                node = Node {
                    kind: NodeKind::Ne {
                        lhs: P::new(node),
                        rhs: P::new(self.relational()),
                    },
                };
            } else {
                break;
            }
        }

        node
    }

    // relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> Node {
        let mut node = self.add();

        loop {
            if self.r#match("<") {
                self.advance();
                node = Node {
                    kind: NodeKind::Lt {
                        lhs: P::new(node),
                        rhs: P::new(self.add()),
                    },
                };
            } else if self.r#match("<=") {
                self.advance();
                node = Node {
                    kind: NodeKind::Le {
                        lhs: P::new(node),
                        rhs: P::new(self.add()),
                    },
                };
            } else if self.r#match(">") {
                self.advance();
                node = Node {
                    kind: NodeKind::Lt {
                        lhs: P::new(self.add()),
                        rhs: P::new(node),
                    },
                };
            } else if self.r#match(">=") {
                self.advance();
                node = Node {
                    kind: NodeKind::Le {
                        lhs: P::new(self.add()),
                        rhs: P::new(node),
                    },
                };
            } else {
                break;
            }
        }

        node
    }

    // add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Node {
        let mut node = self.mul();

        while let TokenKind::Punct = self.peek().kind {
            if self.r#match("+") {
                self.advance();
                node = Node {
                    kind: NodeKind::Add {
                        lhs: P::new(node),
                        rhs: P::new(self.mul()),
                    },
                }
            } else if self.r#match("-") {
                self.advance();
                node = Node {
                    kind: NodeKind::Sub {
                        lhs: P::new(node),
                        rhs: P::new(self.mul()),
                    },
                }
            } else {
                break;
            }
        }

        node
    }

    // mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> Node {
        let mut node = self.unary();

        while let TokenKind::Punct = self.peek().kind {
            if self.r#match("*") {
                self.advance();
                node = Node {
                    kind: NodeKind::Mul {
                        lhs: P::new(node),
                        rhs: P::new(self.unary()),
                    },
                }
            } else if self.r#match("/") {
                self.advance();
                node = Node {
                    kind: NodeKind::Div {
                        lhs: P::new(node),
                        rhs: P::new(self.unary()),
                    },
                }
            } else {
                break;
            }
        }

        node
    }

    // unary = ("+" | "-") unary
    //       | primary
    fn unary(&mut self) -> Node {
        if self.r#match("+") {
            self.advance();
            return self.unary();
        }

        if self.r#match("-") {
            self.advance();
            return Node {
                kind: NodeKind::Neg {
                    expr: P::new(self.unary()),
                },
            };
        }

        self.primary()
    }

    // primary = "(" expr ")" | ident | num
    fn primary(&mut self) -> Node {
        let c = self.peek().clone();
        match c.kind {
            TokenKind::Num { val } => {
                self.advance();
                return Node {
                    kind: NodeKind::Num { val },
                };
            }
            TokenKind::Punct => {
                if self.r#match("(") {
                    self.advance();
                    let node = self.expr();
                    self.skip(")");
                    return node;
                }
            }
            TokenKind::Ident { ref name } => {
                if !self.vars.contains_key(name) {
                    let offset = (self.vars.len() + 1) * 8;
                    self.vars.insert(name.to_string(), offset);
                }

                let node = Node {
                    kind: NodeKind::Var {
                        name: name.to_string(),
                    },
                };
                self.advance();
                return node;
            }
            _ => {}
        };
        self.error_tok(&c, "expected an expression");
    }

    fn peek(&self) -> &Token {
        &self.toks[self.tok_index]
    }

    fn advance(&mut self) {
        if self.tok_index >= self.toks.len() {
            panic!("Unexpected end of file");
        }
        self.tok_index += 1;
    }

    fn r#match(&self, s: &str) -> bool {
        let tok = self.peek();
        self.src[tok.offset..(tok.offset + tok.length)].eq(s.as_bytes())
    }

    fn skip(&mut self, s: &str) {
        if !self.r#match(s) {
            self.error_tok(self.peek(), &format!("Expected {}", s));
        }
        self.advance();
    }

    pub fn ensure_done(&self) {
        match self.peek().kind {
            TokenKind::Eof => {}
            _ => self.error_tok(self.peek(), "extra token"),
        }
    }

    fn is_done(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }
}
