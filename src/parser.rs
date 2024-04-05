use std::cell::RefCell;
use std::rc::Rc;

use crate::errors::ErrorReporting;
use crate::lexer::{Token, TokenKind};

pub type P<A> = Box<A>;
pub type SP<A> = Rc<RefCell<A>>;
pub type AsciiStr = Vec<u8>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Ptr(P<Type>),
    Unit,
    Fn(P<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node<Kind> {
    pub kind: Kind,
    pub offset: usize,
    pub r#type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarData {
    pub name: AsciiStr,
    pub r#type: Type,
    pub stack_offset: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Num(i64),
    Var(SP<VarData>),

    Addr(P<ExprNode>),
    Deref(P<ExprNode>),

    Funcall(AsciiStr, Vec<ExprNode>),

    Add(P<ExprNode>, P<ExprNode>),
    Sub(P<ExprNode>, P<ExprNode>),
    Mul(P<ExprNode>, P<ExprNode>),
    Div(P<ExprNode>, P<ExprNode>),
    Neg(P<ExprNode>),

    Eq(P<ExprNode>, P<ExprNode>),
    Ne(P<ExprNode>, P<ExprNode>),
    Lt(P<ExprNode>, P<ExprNode>),
    Le(P<ExprNode>, P<ExprNode>),

    Assign(P<ExprNode>, P<ExprNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Expr(ExprNode),
    Return(ExprNode),
    Block(Vec<StmtNode>),
    If(P<ExprNode>, P<StmtNode>, Option<P<StmtNode>>),
    For(
        Option<P<StmtNode>>,
        Option<P<ExprNode>>,
        Option<P<ExprNode>>,
        P<StmtNode>,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Function(AsciiStr, Vec<SP<VarData>>, StmtNode, i64),
}

pub type ExprNode = Node<ExprKind>;
pub type StmtNode = Node<StmtKind>;
pub type DeclNode = Node<DeclKind>;
pub type SourceUnit = Vec<DeclNode>;

#[derive(Debug, Clone, PartialEq)]
pub struct Parser<'a> {
    src: &'a [u8],
    toks: &'a [Token],
    tok_index: usize,
    vars: Vec<SP<VarData>>,
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
            vars: Vec::new(),
        }
    }

    // source_unit = stmt+
    pub fn source_unit(&mut self) -> SourceUnit {
        let mut fns = vec![];
        loop {
            match self.peek().kind {
                TokenKind::Eof => break,
                _ => fns.push(self.function()),
            }
        }
        fns
    }

    pub fn function(&mut self) -> DeclNode {
        let offset = self.peek().offset;

        let r#type = self.declspec();
        let (r#type, name) = self.declarator(&r#type);

        self.vars.clear();
        let body = self.compound_stmt();
        let mut locals = self.vars.clone();
        locals.reverse();
        DeclNode {
            kind: DeclKind::Function(name, locals, body, -1),
            offset,
            r#type,
        }
    }

    // stmt = "return" expr ";"
    //      | "if" "(" expr ")" stmt ("else" stmt)?
    //      | "for" "( expr-stmt ";" expr? ";" expr? ")" stmt
    //      | "while" "(" expr ")" stmt
    //      | "{" compound-stmt
    //      | expr-stmt
    fn stmt(&mut self) -> StmtNode {
        if self.r#match("return") {
            let offset = self.advance().offset;
            let expr = self.expr();
            self.skip(";");
            return StmtNode {
                kind: StmtKind::Return(expr),
                offset,
                r#type: Type::Unit,
            };
        }

        if self.r#match("if") {
            let offset = self.advance().offset;
            self.skip("(");
            let cond = P::new(self.expr());
            self.skip(")");
            let then_stmt = P::new(self.stmt());
            let mut else_stmt = None;
            if self.r#match("else") {
                self.advance();
                else_stmt = Some(P::new(self.stmt()));
            }
            return StmtNode {
                kind: StmtKind::If(cond, then_stmt, else_stmt),
                offset,
                r#type: Type::Unit,
            };
        }

        if self.r#match("for") {
            let offset = self.advance().offset;
            self.skip("(");
            let init = Some(P::new(self.expr_stmt()));

            let mut cond = None;
            if !self.r#match(";") {
                cond = Some(P::new(self.expr()));
            }
            self.skip(";");

            let mut inc = None;
            if !self.r#match(")") {
                inc = Some(P::new(self.expr()));
            }
            self.skip(")");

            let body = P::new(self.stmt());

            return StmtNode {
                kind: StmtKind::For(init, cond, inc, body),
                offset,
                r#type: Type::Unit,
            };
        }

        if self.r#match("while") {
            let offset = self.advance().offset;
            self.skip("(");
            let cond = Some(P::new(self.expr()));
            self.skip(")");
            let body = P::new(self.stmt());
            return StmtNode {
                kind: StmtKind::For(None, cond, None, body),
                offset,
                r#type: Type::Unit,
            };
        }

        if self.r#match("{") {
            return self.compound_stmt();
        }

        self.expr_stmt()
    }

    // compound_stmt = "{" (declaration | stmt)* "}
    fn compound_stmt(&mut self) -> StmtNode {
        let offset = self.skip("{").offset;
        let mut stmts = Vec::new();
        while !self.r#match("}") {
            if self.r#match("int") {
                self.declaration(&mut stmts);
            } else {
                stmts.push(self.stmt());
            }
        }
        self.advance();
        StmtNode {
            kind: StmtKind::Block(stmts),
            offset,
            r#type: Type::Unit,
        }
    }

    // declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(&mut self, stmts: &mut Vec<StmtNode>) {
        let base_type = self.declspec();

        let mut count = 0;
        while !self.r#match(";") {
            if count > 0 {
                self.skip(",");
            }
            count += 1;

            let offset = self.peek().offset;
            let (r#type, name) = self.declarator(&base_type);
            let var_data = Rc::new(RefCell::new(VarData {
                name,
                r#type: r#type.clone(),
                stack_offset: -1,
            }));
            self.vars.push(var_data.clone());

            if !self.r#match("=") {
                continue;
            }

            self.advance();
            let lhs = ExprNode {
                kind: ExprKind::Var(var_data),
                offset,
                r#type,
            };
            let rhs = self.assign();
            let rhs_type = rhs.r#type.clone();

            stmts.push(StmtNode {
                kind: StmtKind::Expr(ExprNode {
                    kind: ExprKind::Assign(P::new(lhs), P::new(rhs)),
                    offset,
                    r#type: rhs_type,
                }),
                offset,
                r#type: Type::Unit,
            });
        }
    }

    // declspec = "int"
    fn declspec(&mut self) -> Type {
        self.skip("int");
        Type::Int
    }

    // declarator = "*"* ident type-suffix
    fn declarator(&mut self, base_type: &Type) -> (Type, AsciiStr) {
        let mut r#type = base_type.clone();
        while self.r#match("*") {
            self.advance();
            r#type = Type::Ptr(P::new(r#type));
        }

        match self.peek().kind {
            TokenKind::Ident => {
                let name = self.tok_source(self.peek()).to_owned();
                self.advance();
                (self.type_suffix(r#type), name)
            }
            _ => self.error_tok(self.peek(), "expected a variable name"),
        }
    }
    fn type_suffix(&mut self, r#type: Type) -> Type {
        if self.r#match("(") {
            self.advance();
            self.skip(")");
            return Type::Fn(P::new(r#type));
        }
        r#type
    }

    // expr-stmt = expr? ";"
    fn expr_stmt(&mut self) -> StmtNode {
        if self.r#match(";") {
            let offset = self.advance().offset;
            return StmtNode {
                kind: StmtKind::Block(Vec::new()),
                offset,
                r#type: Type::Unit,
            };
        }

        let expr = self.expr();
        let offset = expr.offset;
        self.skip(";");
        StmtNode {
            kind: StmtKind::Expr(expr),
            offset,
            r#type: Type::Unit,
        }
    }

    // expr = assign
    fn expr(&mut self) -> ExprNode {
        self.assign()
    }

    // assign = equality ("=" assign)?
    fn assign(&mut self) -> ExprNode {
        let mut node = self.equality();
        if self.r#match("=") {
            let offset = self.advance().offset;
            let r#type = node.r#type.clone();
            node = ExprNode {
                kind: ExprKind::Assign(P::new(node), P::new(self.assign())),
                offset,
                r#type,
            };
        }
        node
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> ExprNode {
        let mut node = self.relational();

        loop {
            if self.r#match("==") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Eq(P::new(node), P::new(self.relational())),
                    offset,
                    r#type: Type::Int,
                };
            } else if self.r#match("!=") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Ne(P::new(node), P::new(self.relational())),
                    offset,
                    r#type: Type::Int,
                };
            } else {
                break;
            }
        }

        node
    }

    // relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> ExprNode {
        let mut node = self.add();

        loop {
            if self.r#match("<") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Lt(P::new(node), P::new(self.add())),
                    offset,
                    r#type: Type::Int,
                };
            } else if self.r#match("<=") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Le(P::new(node), P::new(self.add())),
                    offset,
                    r#type: Type::Int,
                };
            } else if self.r#match(">") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Lt(P::new(self.add()), P::new(node)),
                    offset,
                    r#type: Type::Int,
                };
            } else if self.r#match(">=") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Le(P::new(self.add()), P::new(node)),
                    offset,
                    r#type: Type::Int,
                };
            } else {
                break;
            }
        }

        node
    }

    // add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> ExprNode {
        let mut node = self.mul();

        loop {
            if self.r#match("+") {
                let offset = self.advance().offset;
                let rhs = P::new(self.mul());
                node = self.add_overload(P::new(node), rhs, offset);
            } else if self.r#match("-") {
                let offset = self.advance().offset;
                let rhs = P::new(self.mul());
                node = self.sub_overload(P::new(node), rhs, offset);
            } else {
                break;
            }
        }

        node
    }

    // mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> ExprNode {
        let mut node = self.unary();

        loop {
            if self.r#match("*") {
                let offset = self.advance().offset;
                let r#type = node.r#type.clone();
                node = ExprNode {
                    kind: ExprKind::Mul(P::new(node), P::new(self.unary())),
                    offset,
                    r#type,
                };
            } else if self.r#match("/") {
                let offset = self.advance().offset;
                let r#type = node.r#type.clone();
                node = ExprNode {
                    kind: ExprKind::Div(P::new(node), P::new(self.unary())),
                    offset,
                    r#type,
                };
            } else {
                break;
            }
        }

        node
    }

    // unary = ("+" | "-" | "*" | "&") unary
    //       | primary
    fn unary(&mut self) -> ExprNode {
        if self.r#match("+") {
            self.advance();
            return self.unary();
        }

        if self.r#match("-") {
            let offset = self.advance().offset;
            let node = P::new(self.unary());
            let r#type = node.r#type.clone();

            return ExprNode {
                kind: ExprKind::Neg(node),
                offset,
                r#type,
            };
        }

        if self.r#match("&") {
            let offset = self.advance().offset;
            let node = P::new(self.unary());
            let r#type = Type::Ptr(P::new(node.r#type.clone()));

            return ExprNode {
                kind: ExprKind::Addr(node),
                offset,
                r#type,
            };
        }

        if self.r#match("*") {
            let offset = self.advance().offset;

            let node = P::new(self.unary());

            let r#type = if let Type::Ptr(ref base) = node.r#type {
                *base.clone()
            } else {
                Type::Int
            };

            return ExprNode {
                kind: ExprKind::Deref(node),
                offset,
                r#type,
            };
        }

        self.primary()
    }

    // primary = "(" expr ")" | ident args? | num
    // args = "(" ")"
    fn primary(&mut self) -> ExprNode {
        match self.peek().kind {
            TokenKind::Num(val) => {
                let offset = self.advance().offset;
                return ExprNode {
                    kind: ExprKind::Num(val),
                    offset,
                    r#type: Type::Int,
                };
            }
            TokenKind::Ident => {
                if self.match_ahead(1, "(") {
                    return self.funcall();
                }

                let tok = self.peek();
                let offset = tok.offset;
                let name = self.tok_source(tok).to_owned();
                self.advance();

                let var_data = self.vars.iter().find(|v| v.borrow().name == name);
                if let Some(var_data) = var_data {
                    let expr = ExprNode {
                        kind: ExprKind::Var(var_data.clone()),
                        offset,
                        r#type: Type::Int,
                    };
                    return expr;
                }
                self.error_at(offset, "undefined variable");
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
        self.error_tok(self.peek(), "expected an expression");
    }

    // funcall = ident "(" (assign ("," assign)*? ")"
    fn funcall(&mut self) -> ExprNode {
        let tok = self.peek();
        let offset = tok.offset;
        let fn_name = self.tok_source(tok).to_owned();
        self.advance();

        let mut args = vec![];
        self.skip("(");
        while !self.r#match(")") {
            if !args.is_empty() {
                self.skip(",");
            }
            args.push(self.assign());
        }
        self.skip(")");

        ExprNode {
            kind: ExprKind::Funcall(fn_name, args),
            offset,
            r#type: Type::Int,
        }
    }

    fn peek(&self) -> &Token {
        &self.toks[self.tok_index]
    }

    fn peek_n(&self, n: usize) -> &Token {
        &self.toks[self.tok_index + n]
    }

    fn advance(&mut self) -> &Token {
        if self.tok_index >= self.toks.len() {
            panic!("Unexpected end of file");
        }
        let tok = &self.toks[self.tok_index];
        self.tok_index += 1;
        tok
    }

    fn tok_source(&self, tok: &Token) -> &[u8] {
        &self.src[tok.offset..(tok.offset + tok.length)]
    }

    fn r#match(&self, s: &str) -> bool {
        self.tok_source(self.peek()).eq(s.as_bytes())
    }

    fn match_ahead(&self, n: usize, s: &str) -> bool {
        self.tok_source(self.peek_n(n)).eq(s.as_bytes())
    }

    fn skip(&mut self, s: &str) -> &Token {
        if !self.r#match(s) {
            self.error_tok(self.peek(), &format!("Expected {}", s));
        }
        self.advance()
    }

    fn is_done(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    pub fn ensure_done(&self) {
        if !self.is_done() {
            self.error_tok(self.peek(), "extra token")
        }
    }

    fn add_overload(&self, lhs: P<ExprNode>, rhs: P<ExprNode>, offset: usize) -> ExprNode {
        let mut lhs = lhs;
        let mut rhs = rhs;

        if let Type::Int = lhs.r#type {
            if let Type::Ptr(_) = rhs.r#type {
                std::mem::swap(&mut lhs, &mut rhs);
            }
        }

        match (&lhs.r#type, &rhs.r#type) {
            (Type::Int, Type::Int) => ExprNode {
                kind: ExprKind::Add(lhs, rhs),
                offset,
                r#type: Type::Int,
            },
            (Type::Ptr(_), Type::Int) => {
                let rhs = P::new(ExprNode {
                    kind: ExprKind::Mul(
                        P::new(ExprNode {
                            kind: ExprKind::Num(8),
                            offset,
                            r#type: Type::Int,
                        }),
                        rhs,
                    ),
                    offset,
                    r#type: Type::Int,
                });
                let r#type = lhs.r#type.clone();
                ExprNode {
                    kind: ExprKind::Add(lhs, rhs),
                    offset,
                    r#type,
                }
            }
            _ => self.error_at(offset, "invalid operands"),
        }
    }

    fn sub_overload(&self, lhs: P<ExprNode>, rhs: P<ExprNode>, offset: usize) -> ExprNode {
        match (&lhs.r#type, &rhs.r#type) {
            (Type::Int, Type::Int) => ExprNode {
                kind: ExprKind::Sub(lhs, rhs),
                offset,
                r#type: Type::Int,
            },
            (Type::Ptr(_), Type::Int) => {
                let rhs = P::new(ExprNode {
                    kind: ExprKind::Mul(synth_num(8, offset), rhs),
                    offset,
                    r#type: Type::Int,
                });
                let r#type = lhs.r#type.clone();
                ExprNode {
                    kind: ExprKind::Sub(lhs, rhs),
                    offset,
                    r#type,
                }
            }
            (Type::Ptr(_), Type::Ptr(_)) => {
                let node = P::new(ExprNode {
                    kind: ExprKind::Sub(lhs, rhs),
                    offset,
                    r#type: Type::Int,
                });
                ExprNode {
                    kind: ExprKind::Div(node, synth_num(8, offset)),
                    offset,
                    r#type: Type::Int,
                }
            }
            _ => self.error_at(offset, "invalid operands"),
        }
    }
}

fn synth_num(v: i64, offset: usize) -> P<ExprNode> {
    P::new(ExprNode {
        kind: ExprKind::Num(v),
        offset,
        r#type: Type::Int,
    })
}
