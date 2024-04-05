use std::cell::RefCell;
use std::rc::Rc;

use crate::errors::ErrorReporting;
use crate::lexer::{Token, TokenKind};

pub type P<A> = Box<A>;
pub type SP<A> = Rc<RefCell<A>>;
pub type AsciiStr = Vec<u8>;

#[derive(Debug, Clone)]
pub enum TypeKind {
    Int,
    Ptr(P<Type>),
    Fn(P<Type>, Vec<P<Type>>),
    Array(P<Type>, usize),
    Unit,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub size: usize,
}

impl Type {
    fn int() -> Type {
        Type {
            kind: TypeKind::Int,
            size: 8,
        }
    }
    fn unit() -> Type {
        Type {
            kind: TypeKind::Unit,
            size: 0,
        }
    }
    fn ptr(base: P<Type>) -> Type {
        Type {
            kind: TypeKind::Ptr(base),
            size: 8,
        }
    }
    fn func(ret: P<Type>, params: Vec<P<Type>>) -> Type {
        Type {
            kind: TypeKind::Fn(ret, params),
            size: 0,
        }
    }
    fn array(base: P<Type>, len: usize) -> Type {
        let base_size = base.size;
        Type {
            kind: TypeKind::Array(base, len),
            size: base_size * len,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node<Kind> {
    pub kind: Kind,
    pub offset: usize,
    pub r#type: Type,
}

#[derive(Debug, Clone)]
pub struct VarData {
    pub name: AsciiStr,
    pub r#type: Type,
    pub stack_offset: i64,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Num(i64),
    Var(SP<VarData>),

    Addr(P<ExprNode>),
    Deref(P<ExprNode>),

    Funcall(AsciiStr, Vec<P<ExprNode>>),

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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum TopDeclKind {
    Function {
        name: AsciiStr,
        params: Vec<SP<VarData>>,
        locals: Vec<SP<VarData>>,
        body: StmtNode,
        stack_size: i64,
    },
}

pub type ExprNode = Node<ExprKind>;
pub type StmtNode = Node<StmtKind>;
pub type TopDeclNode = Node<TopDeclKind>;
pub type SourceUnit = Vec<TopDeclNode>;

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
        let mut fns = Vec::new();
        loop {
            match self.peek().kind {
                TokenKind::Eof => break,
                _ => fns.push(self.function()),
            }
        }
        fns
    }

    pub fn function(&mut self) -> TopDeclNode {
        self.vars.clear();

        let offset = self.peek().offset;
        let r#type = self.declspec();
        let (r#type, name) = self.declarator(&r#type);

        let params = self.vars.clone();

        let body = self.compound_stmt();
        // Reverse them to keep the locals layout in line with chibicc
        let locals: Vec<SP<VarData>> = self.vars.clone().into_iter().rev().collect();
        TopDeclNode {
            kind: TopDeclKind::Function {
                name,
                params,
                locals,
                body,
                stack_size: -1,
            },
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
        if self.peek_is("return") {
            let offset = self.advance().offset;
            let expr = self.expr();
            self.skip(";");
            return StmtNode {
                kind: StmtKind::Return(expr),
                offset,
                r#type: Type::unit(),
            };
        }

        if self.peek_is("if") {
            let offset = self.advance().offset;
            self.skip("(");
            let cond = P::new(self.expr());
            self.skip(")");
            let then_stmt = P::new(self.stmt());
            let mut else_stmt = None;
            if self.peek_is("else") {
                self.advance();
                else_stmt = Some(P::new(self.stmt()));
            }
            return StmtNode {
                kind: StmtKind::If(cond, then_stmt, else_stmt),
                offset,
                r#type: Type::unit(),
            };
        }

        if self.peek_is("for") {
            let offset = self.advance().offset;
            self.skip("(");
            let init = Some(P::new(self.expr_stmt()));

            let mut cond = None;
            if !self.peek_is(";") {
                cond = Some(P::new(self.expr()));
            }
            self.skip(";");

            let mut inc = None;
            if !self.peek_is(")") {
                inc = Some(P::new(self.expr()));
            }
            self.skip(")");

            let body = P::new(self.stmt());

            return StmtNode {
                kind: StmtKind::For(init, cond, inc, body),
                offset,
                r#type: Type::unit(),
            };
        }

        if self.peek_is("while") {
            let offset = self.advance().offset;
            self.skip("(");
            let cond = Some(P::new(self.expr()));
            self.skip(")");
            let body = P::new(self.stmt());
            return StmtNode {
                kind: StmtKind::For(None, cond, None, body),
                offset,
                r#type: Type::unit(),
            };
        }

        if self.peek_is("{") {
            return self.compound_stmt();
        }

        self.expr_stmt()
    }

    // compound_stmt = "{" (declaration | stmt)* "}
    fn compound_stmt(&mut self) -> StmtNode {
        let offset = self.skip("{").offset;
        let mut stmts = Vec::new();
        while !self.peek_is("}") {
            if self.peek_is("int") {
                self.declaration(&mut stmts);
            } else {
                stmts.push(self.stmt());
            }
        }
        self.advance();
        StmtNode {
            kind: StmtKind::Block(stmts),
            offset,
            r#type: Type::unit(),
        }
    }

    // declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(&mut self, stmts: &mut Vec<StmtNode>) {
        let base_ty = self.declspec();

        let mut count = 0;
        while !self.peek_is(";") {
            if count > 0 {
                self.skip(",");
            }
            count += 1;

            let offset = self.peek().offset;
            let (r#type, name) = self.declarator(&base_ty);
            let var_data = Rc::new(RefCell::new(VarData {
                name,
                r#type: r#type.clone(),
                stack_offset: -1,
            }));
            self.vars.push(var_data.clone());

            if !self.peek_is("=") {
                continue;
            }

            self.advance();
            let lhs = ExprNode {
                kind: ExprKind::Var(var_data),
                offset,
                r#type: r#type.clone(),
            };
            let rhs = self.assign();
            stmts.push(StmtNode {
                kind: StmtKind::Expr(ExprNode {
                    kind: ExprKind::Assign(P::new(lhs), P::new(rhs)),
                    offset,
                    r#type,
                }),
                offset,
                r#type: Type::unit(),
            });
        }
    }

    // declspec = "int"
    fn declspec(&mut self) -> Type {
        self.skip("int");
        Type::int()
    }

    // declarator = "*"* ident type-suffix
    fn declarator(&mut self, base_type: &Type) -> (Type, AsciiStr) {
        let mut r#type = base_type.clone();
        while self.peek_is("*") {
            self.advance();
            r#type = Type::ptr(P::new(r#type));
        }

        let decl = match self.peek().kind {
            TokenKind::Ident => {
                let name = self.tok_source(self.peek()).to_owned();
                self.advance();
                (self.type_suffix(r#type), name)
            }
            _ => self.error_tok(self.peek(), "expected a variable name"),
        };

        println!("# DECL {}: {:?}", String::from_utf8_lossy(&decl.1), decl.0);
        decl
    }

    // type-suffix = "(" func-params
    //             | "[" num "]"
    //             | ε
    fn type_suffix(&mut self, r#type: Type) -> Type {
        if self.peek_is("(") {
            return self.func_params(r#type);
        }

        if self.peek_is("[") {
            self.advance();
            let len = self.get_number();
            self.skip("]");
            return Type::array(P::new(r#type), len.try_into().unwrap());
        }
        r#type
    }

    // func-params = (param ("," param)*)? ")"
    // param       = declspec declarator
    fn func_params(&mut self, r#type: Type) -> Type {
        let mut params = Vec::new();
        self.advance();
        while !self.peek_is(")") {
            if params.len() > 0 {
                self.skip(",");
            }
            let base_ty = self.declspec();
            let (r#type, name) = self.declarator(&base_ty);
            params.push(P::new(r#type.clone()));
            self.vars.push(Rc::new(RefCell::new(VarData {
                name,
                r#type,
                stack_offset: -1,
            })));
        }
        self.skip(")");
        return Type::func(P::new(r#type), params);
    }

    // expr-stmt = expr? ";"
    fn expr_stmt(&mut self) -> StmtNode {
        if self.peek_is(";") {
            let offset = self.advance().offset;
            return StmtNode {
                kind: StmtKind::Block(Vec::new()),
                offset,
                r#type: Type::unit(),
            };
        }

        let expr = self.expr();
        let offset = expr.offset;
        self.skip(";");
        StmtNode {
            kind: StmtKind::Expr(expr),
            offset,
            r#type: Type::unit(),
        }
    }

    // expr = assign
    fn expr(&mut self) -> ExprNode {
        self.assign()
    }

    // assign = equality ("=" assign)?
    fn assign(&mut self) -> ExprNode {
        let mut node = self.equality();
        if self.peek_is("=") {
            let offset = self.advance().offset;
            let rhs = P::new(self.assign());
            let r#type = node.r#type.clone();
            if let TypeKind::Array(_, _) = r#type.kind {
                self.error_at(node.offset, "not an lvalue");
            }
            node = ExprNode {
                kind: ExprKind::Assign(P::new(node), rhs),
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
            if self.peek_is("==") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Eq(P::new(node), P::new(self.relational())),
                    offset,
                    r#type: Type::int(),
                };
            } else if self.peek_is("!=") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Ne(P::new(node), P::new(self.relational())),
                    offset,
                    r#type: Type::int(),
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
            if self.peek_is("<") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Lt(P::new(node), P::new(self.add())),
                    offset,
                    r#type: Type::int(),
                };
            } else if self.peek_is("<=") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Le(P::new(node), P::new(self.add())),
                    offset,
                    r#type: Type::int(),
                };
            } else if self.peek_is(">") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Lt(P::new(self.add()), P::new(node)),
                    offset,
                    r#type: Type::int(),
                };
            } else if self.peek_is(">=") {
                let offset = self.advance().offset;
                node = ExprNode {
                    kind: ExprKind::Le(P::new(self.add()), P::new(node)),
                    offset,
                    r#type: Type::int(),
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
            if self.peek_is("+") {
                let offset = self.advance().offset;
                let rhs = P::new(self.mul());
                node = self.add_overload(P::new(node), rhs, offset);
            } else if self.peek_is("-") {
                let offset = self.advance().offset;
                let rhs = P::new(self.mul());
                node = self.sub_overload(P::new(node), rhs, offset);
            } else {
                break;
            }
        }

        node
    }

    fn add_overload(&self, lhs: P<ExprNode>, rhs: P<ExprNode>, offset: usize) -> ExprNode {
        let mut lhs = lhs;
        let mut rhs = rhs;

        if let TypeKind::Int = lhs.r#type.kind {
            if let TypeKind::Ptr(_) = rhs.r#type.kind {
                let tmp = lhs;
                lhs = rhs;
                rhs = tmp;
            }
        }

        match (&lhs.r#type.kind, &rhs.r#type.kind) {
            (TypeKind::Int, TypeKind::Int) => ExprNode {
                kind: ExprKind::Add(lhs, rhs),
                offset,
                r#type: Type::int(),
            },
            (TypeKind::Ptr(_), TypeKind::Int) | (TypeKind::Array(_, _), TypeKind::Int) => {
                let rhs = P::new(ExprNode {
                    kind: ExprKind::Mul(
                        P::new(ExprNode {
                            kind: ExprKind::Num(8),
                            offset,
                            r#type: Type::int(),
                        }),
                        rhs,
                    ),
                    offset,
                    r#type: Type::int(),
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
        match (&lhs.r#type.kind, &rhs.r#type.kind) {
            (TypeKind::Int, TypeKind::Int) => ExprNode {
                kind: ExprKind::Sub(lhs, rhs),
                offset,
                r#type: Type::int(),
            },
            (TypeKind::Ptr(_), TypeKind::Int) | (TypeKind::Array(_, _), TypeKind::Int) => {
                let rhs = P::new(ExprNode {
                    kind: ExprKind::Mul(synth_num(8, offset), rhs),
                    offset,
                    r#type: Type::int(),
                });
                let r#type = lhs.r#type.clone();
                ExprNode {
                    kind: ExprKind::Sub(lhs, rhs),
                    offset,
                    r#type,
                }
            }
            // TODO better way than combinatorial explosion?
            (TypeKind::Ptr(_), TypeKind::Ptr(_))
            | (TypeKind::Array(_, _), TypeKind::Ptr(_))
            | (TypeKind::Ptr(_), TypeKind::Array(_, _))
            | (TypeKind::Array(_, _), TypeKind::Array(_, _)) => {
                let node = P::new(ExprNode {
                    kind: ExprKind::Sub(lhs, rhs),
                    offset,
                    r#type: Type::int(),
                });
                ExprNode {
                    kind: ExprKind::Div(node, synth_num(8, offset)),
                    offset,
                    r#type: Type::int(),
                }
            }
            _ => self.error_at(offset, "invalid operands"),
        }
    }

    // mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self) -> ExprNode {
        let mut node = self.unary();

        loop {
            if self.peek_is("*") {
                let offset = self.advance().offset;
                let r#type = node.r#type.clone();
                node = ExprNode {
                    kind: ExprKind::Mul(P::new(node), P::new(self.unary())),
                    offset,
                    r#type,
                };
            } else if self.peek_is("/") {
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
        if self.peek_is("+") {
            self.advance();
            return self.unary();
        }

        if self.peek_is("-") {
            let offset = self.advance().offset;
            let node = P::new(self.unary());
            let r#type = node.r#type.clone();
            return ExprNode {
                kind: ExprKind::Neg(node),
                offset,
                r#type,
            };
        }

        if self.peek_is("&") {
            let offset = self.advance().offset;
            let node = P::new(self.unary());
            let r#type = match &node.r#type.kind {
                TypeKind::Array(base_type, _) => Type::ptr(P::new(*base_type.clone())),
                _ => Type::ptr(P::new(node.r#type.clone())),
            };
            return ExprNode {
                kind: ExprKind::Addr(node),
                offset,
                r#type,
            };
        }

        if self.peek_is("*") {
            let offset = self.advance().offset;
            let node = P::new(self.unary());
            let r#type = match &node.r#type.kind {
                TypeKind::Ptr(base) => *base.clone(),
                TypeKind::Array(base, _) => *base.clone(),
                _ => {
                    println!("{:?}", node);
                    self.error_at(offset, "invalid pointer dereference")
                }
            };
            return ExprNode {
                kind: ExprKind::Deref(node),
                offset,
                r#type,
            };
        }

        self.primary()
    }

    // primary = "(" expr ")" | funcall | num
    fn primary(&mut self) -> ExprNode {
        match self.peek().kind {
            TokenKind::Num(val) => {
                let offset = self.advance().offset;
                return ExprNode {
                    kind: ExprKind::Num(val),
                    offset,
                    r#type: Type::int(),
                };
            }
            TokenKind::Ident => {
                if self.la_is(1, "(") {
                    return self.funcall();
                }

                let tok = self.peek();
                let offset = tok.offset;
                let name = self.tok_source(tok).to_owned();
                self.advance();

                let var_data = self.vars.iter().find(|v| v.borrow().name == name);
                if let Some(var_data) = var_data {
                    let r#type = var_data.borrow_mut().r#type.clone();
                    let expr = ExprNode {
                        kind: ExprKind::Var(var_data.clone()),
                        offset,
                        r#type,
                    };
                    return expr;
                } else {
                    self.error_at(offset, "undefined variable");
                }
            }
            TokenKind::Punct => {
                if self.peek_is("(") {
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

    // funcall = ident "(" (assign ("," assign)*)? ")"
    fn funcall(&mut self) -> ExprNode {
        let tok = self.peek();
        let offset = tok.offset;
        let fn_name = self.tok_source(tok).to_owned();
        self.advance();

        let mut args = Vec::new();
        self.skip("(");
        while !self.peek_is(")") {
            if args.len() > 0 {
                self.skip(",");
            }
            args.push(P::new(self.assign()));
        }
        self.skip(")");

        ExprNode {
            kind: ExprKind::Funcall(fn_name, args),
            offset,
            r#type: Type::int(),
        }
    }

    fn peek(&self) -> &Token {
        &self.toks[self.tok_index]
    }
    fn la(&self, n: usize) -> &Token {
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

    fn get_number(&mut self) -> i64 {
        if let TokenKind::Num(val) = self.peek().kind {
            self.advance();
            return val;
        }
        self.error_tok(self.peek(), "expected a number");
    }

    fn tok_source(&self, tok: &Token) -> &[u8] {
        &self.src[tok.offset..(tok.offset + tok.length)]
    }

    fn peek_is(&self, s: &str) -> bool {
        self.tok_source(self.peek()).eq(s.as_bytes())
    }

    fn la_is(&self, n: usize, s: &str) -> bool {
        self.tok_source(self.la(n)).eq(s.as_bytes())
    }

    fn skip(&mut self, s: &str) -> &Token {
        if !self.peek_is(s) {
            self.error_tok(self.peek(), &format!("Expected {}", s));
        }
        self.advance()
    }

    fn is_done(&self) -> bool {
        match self.peek().kind {
            TokenKind::Eof => true,
            _ => false,
        }
    }

    pub fn ensure_done(&self) {
        if !self.is_done() {
            self.error_tok(self.peek(), "extra token")
        }
    }
}

fn synth_num(v: i64, offset: usize) -> P<ExprNode> {
    P::new(ExprNode {
        kind: ExprKind::Num(v),
        offset,
        r#type: Type::int(),
    })
}
