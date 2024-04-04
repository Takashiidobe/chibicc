use std::env;

#[derive(Debug)]
pub enum TokenKind {
    Punct,
    Num { val: i32 },
    Eof,
}

#[derive(Debug)]
pub struct Token {
    offset: usize,
    length: usize,
    kind: TokenKind,
}

type P<A> = Box<A>;

pub enum NodeKind {
    // Primary
    Num { val: i32 },

    // Binary
    Add { lhs: P<Node>, rhs: P<Node> },
    Sub { lhs: P<Node>, rhs: P<Node> },
    Mul { lhs: P<Node>, rhs: P<Node> },
    Div { lhs: P<Node>, rhs: P<Node> },

    // Unary
    Neg { expr: P<Node> },
}

pub struct Node {
    kind: NodeKind,
}

trait ErrorReporting {
    fn src(&self) -> &[u8];

    fn error_at(&self, offset: usize, msg: &str) -> ! {
        eprintln!("{}", String::from_utf8_lossy(self.src()));
        eprint!("{: <1$}", "", offset);
        eprintln!("^ {}", msg);
        panic!();
    }

    fn error_tok(&self, tok: &Token, msg: &str) -> ! {
        self.error_at(tok.offset, msg);
    }
}

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

    fn number(&mut self) -> i32 {
        let mut buf = vec![];

        while !self.is_at_end() && self.src[self.index].is_ascii_digit() {
            buf.push(self.src[self.index]);
            self.index += 1;
        }

        String::from_utf8_lossy(&buf).parse().unwrap()
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut toks = Vec::new();
        let buf = self.src;

        while self.index < buf.len() {
            let c = buf[self.index];

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
            } else if ispunct(c) {
                toks.push(Token {
                    offset: self.index,
                    length: 1,
                    kind: TokenKind::Punct,
                });
                self.index += 1;
            } else {
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

fn ispunct(c: u8) -> bool {
    matches!(c, b'+' | b'-' | b'*' | b'/' | b'(' | b')')
}

pub struct Parser<'a> {
    src: &'a [u8],
    toks: &'a [Token],
    tok_index: usize,
}

impl<'a> ErrorReporting for Parser<'a> {
    fn src(&self) -> &[u8] {
        self.src
    }
}

impl<'a> Parser<'a> {
    fn new(src: &'a [u8], toks: &'a [Token]) -> Self {
        if toks.is_empty() {
            panic!("Empty token array")
        }
        Self {
            src,
            toks,
            tok_index: 0,
        }
    }

    fn expr(&mut self) -> Node {
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

    fn primary(&mut self) -> Node {
        match self.peek().kind {
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
            _ => {}
        };
        self.error_tok(self.peek(), "expected an expression")
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

    fn ensure_done(&self) {
        match self.peek().kind {
            TokenKind::Eof => {}
            _ => self.error_tok(self.peek(), "extra token"),
        }
    }
}

pub struct Codegen<'a> {
    src: &'a [u8],
    depth: i64,
}

impl<'a> ErrorReporting for Codegen<'a> {
    fn src(&self) -> &[u8] {
        self.src
    }
}

impl<'a> Codegen<'a> {
    fn new(src: &'a [u8]) -> Self {
        Self { src, depth: 0 }
    }

    fn program(&mut self, node: &Node) {
        println!("  .globl main");
        println!("main:");
        self.expr(node);
        println!("  ret");
    }

    fn push(&mut self) {
        println!("  push %rax");
        self.depth += 1;
    }

    fn pop(&mut self, arg: &str) {
        println!("  pop {}", arg);
        self.depth -= 1;
    }

    fn expr(&mut self, node: &Node) {
        match node.kind {
            NodeKind::Num { val } => println!("  mov ${}, %rax", val),
            NodeKind::Add { ref lhs, ref rhs } => {
                self.expr(rhs.as_ref());
                self.push();
                self.expr(lhs.as_ref());
                self.pop("%rdi");
                println!("  add %rdi, %rax");
            }
            NodeKind::Sub { ref lhs, ref rhs } => {
                self.expr(rhs.as_ref());
                self.push();
                self.expr(lhs.as_ref());
                self.pop("%rdi");
                println!("  sub %rdi, %rax");
            }
            NodeKind::Mul { ref lhs, ref rhs } => {
                self.expr(rhs.as_ref());
                self.push();
                self.expr(lhs.as_ref());
                self.pop("%rdi");
                println!("  imul %rdi, %rax");
            }
            NodeKind::Div { ref lhs, ref rhs } => {
                self.expr(rhs.as_ref());
                self.push();
                self.expr(lhs.as_ref());
                self.pop("%rdi");
                println!("  cqo");
                println!("  idiv %rdi, %rax");
            }
            NodeKind::Neg { ref expr } => {
                self.expr(expr);
                println!("  neg %rax");
            }
        };
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments", args[0]);
    }

    let src = args[1].as_bytes();

    let mut lexer = Lexer::new(src);

    let toks = lexer.tokenize();

    let mut parser = Parser::new(src, &toks);

    let node = parser.expr();
    parser.ensure_done();

    let mut codegen = Codegen::new(src);
    codegen.program(&node);

    assert!(codegen.depth == 0);
}
