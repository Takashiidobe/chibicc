#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Punct,
    Num { val: i32 },
    Ident { name: u8 },
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    offset: usize,
    length: usize,
    kind: TokenKind,
}

type P<A> = Box<A>;

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    // Primary
    Num { val: i32 },

    // Binary
    Add { lhs: P<Node>, rhs: P<Node> },
    Sub { lhs: P<Node>, rhs: P<Node> },
    Mul { lhs: P<Node>, rhs: P<Node> },
    Div { lhs: P<Node>, rhs: P<Node> },
    Eq { lhs: P<Node>, rhs: P<Node> },
    Ne { lhs: P<Node>, rhs: P<Node> },
    Lt { lhs: P<Node>, rhs: P<Node> },
    Le { lhs: P<Node>, rhs: P<Node> },

    // Unary
    Neg { expr: P<Node> },

    // Statement
    ExprStmt { lhs: P<Node>, next: Option<P<Node>> },

    // Variables
    Assign { lhs: P<Node>, rhs: P<Node> },
    Var { name: u8 },
}

#[derive(Debug, Clone, PartialEq)]
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

pub mod codegen;
pub mod parse;
pub mod tokenize;
