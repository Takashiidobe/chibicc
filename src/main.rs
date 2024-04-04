use std::process::exit;

#[derive(Default, Debug, Clone, PartialEq)]
enum TokenKind {
    Punct,
    Num,
    #[default]
    Eof,
}

#[derive(Default, Debug, Clone, PartialEq)]
struct Token {
    kind: TokenKind,
    val: Option<i32>,
    loc: usize,
    len: usize,
    raw: String,
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, end: usize) -> Self {
        Self {
            kind,
            loc: start,
            len: end - start,
            val: None,
            raw: String::default(),
        }
    }

    pub fn equal(&self, op: char) -> bool {
        self.raw == op.to_string()
    }

    pub fn get_number(&self) -> i32 {
        if self.kind != TokenKind::Num {
            eprintln!("expected a number");
            exit(1);
        }
        // safe cause it is a number
        self.val.unwrap()
    }
}

struct Scanner {
    chars: Vec<char>,
    index: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
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

                let mut token = Token::new(TokenKind::Num, start, end);
                token.val = Some(val);
                token.len = end - start;

                tokens.push(token);
                continue;
            }
            if c == Some('+') || c == Some('-') {
                let mut token = Token::new(TokenKind::Punct, self.index, self.index + 1);
                token.raw = c.unwrap().to_string();
                self.index += 1;
                tokens.push(token);
                continue;
            }

            eprintln!("invalid token");
            exit(1);
        }

        let eof_token = Token::new(TokenKind::Eof, self.index, self.index);
        tokens.push(eof_token);

        tokens
    }
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args.len());
        exit(1);
    }

    let mut scanner = Scanner::new(&args[1]);
    let tokens = scanner.tokenize();

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

        if token.equal('+') {
            i += 1;
            println!("  add ${}, %rax", tokens[i].get_number());
            i += 1;
        } else if token.equal('-') {
            i += 1;
            println!("  sub ${}, %rax", tokens[i].get_number());
            i += 1;
        } else {
            eprintln!("Invalid token: {:?}", token);
            exit(1);
        }
    }

    println!("  ret");
}
