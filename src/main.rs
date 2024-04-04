#[derive(Default, Debug, Clone, PartialEq)]
pub enum TokenKind {
    Punct,
    Num {
        val: i32,
    },
    #[default]
    Eof,
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    loc: usize,
    len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, loc: usize, len: usize) -> Self {
        Self { kind, loc, len }
    }

    pub fn get_number(&self) -> i32 {
        match self.kind {
            TokenKind::Punct | TokenKind::Eof => panic!("expected a number"),
            TokenKind::Num { val } => val,
        }
    }
}

pub struct Compiler {
    program: String,
    chars: Vec<char>,
    index: usize,
}

impl Compiler {
    pub fn new(source: &str) -> Self {
        Self {
            program: source.to_string(),
            chars: source.chars().collect(),
            index: 0,
        }
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.chars.len()
    }

    fn curr(&self) -> Option<char> {
        if !self.is_at_end() {
            Some(self.chars[self.index])
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while self.curr().is_some_and(|c| c.is_ascii_whitespace()) {
            self.index += 1;
        }
    }

    fn number(&mut self) -> i32 {
        let mut curr_num = String::default();
        while let Some(c) = self.curr() {
            if !c.is_ascii_digit() {
                break;
            }
            curr_num.push(c);
            self.index += 1;
        }
        curr_num.parse().unwrap()
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        while !self.is_at_end() {
            let c = self.curr();
            if c.is_some_and(|c| c.is_ascii_whitespace()) {
                self.skip_whitespace();
                continue;
            }
            if c.is_some_and(|c| c.is_ascii_digit()) {
                let start = self.index;
                let val = self.number();
                let end = self.index;

                let token = Token::new(TokenKind::Num { val }, start, end - start);

                tokens.push(token);
                continue;
            }
            if c == Some('+') || c == Some('-') {
                let token = Token::new(TokenKind::Punct, self.index, 1);
                self.index += 1;
                tokens.push(token);
                continue;
            }

            panic!("invalid token");
        }

        let eof_token = Token::new(TokenKind::Eof, self.index, 1);
        tokens.push(eof_token);

        tokens
    }

    pub fn compile(&mut self) {
        let tokens = self.tokenize();

        println!("  .globl main");
        println!("main:");

        if let Some(tok) = tokens.first() {
            println!("  mov ${}, %rax", tok.get_number());
        }

        let mut i = 1;

        while i < tokens.len() {
            let token = &tokens[i];
            if token.kind == TokenKind::Eof {
                break;
            }

            if self.equal(token, "+") {
                i += 1;
                println!("  add ${}, %rax", tokens[i].get_number());
                i += 1;
            } else if self.equal(token, "-") {
                i += 1;
                println!("  sub ${}, %rax", tokens[i].get_number());
                i += 1;
            } else {
                self.error_tok(token, "invalid token");
            }
        }

        println!("  ret");
    }

    fn equal(&self, tok: &Token, s: &str) -> bool {
        self.chars[tok.loc..(tok.loc + tok.len)] == s.chars().collect::<Vec<_>>()
    }

    fn error_at(&self, offset: usize, msg: &str) -> ! {
        eprintln!("{}", self.program);
        eprint!("{: <1$}", "", offset);
        eprintln!("^ {}", msg);
        panic!();
    }

    fn error_tok(&self, tok: &Token, msg: &str) -> ! {
        self.error_at(tok.loc, msg);
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args.len());
        panic!();
    }

    let mut compiler = Compiler::new(&args[1]);
    compiler.compile()
}
