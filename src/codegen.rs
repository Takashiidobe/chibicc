use crate::ErrorReporting;
use crate::Node;
use crate::NodeKind;

pub struct Codegen<'a> {
    pub src: &'a [u8],
    pub depth: i64,
}

impl<'a> ErrorReporting for Codegen<'a> {
    fn src(&self) -> &[u8] {
        self.src
    }
}

impl<'a> Codegen<'a> {
    pub fn new(src: &'a [u8]) -> Self {
        Self { src, depth: 0 }
    }

    pub fn program(&mut self, nodes: &[Node]) {
        println!("  .globl main");
        println!("main:");

        // Prologue
        println!("  push %rbp");
        println!("  mov %rsp, %rbp");
        println!("  sub $208, %rsp");
        println!();

        for node in nodes {
            self.stmt(node);
        }

        // Epilogue
        println!();
        println!("  mov %rbp, %rsp");
        println!("  pop %rbp");

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

    fn stmt(&mut self, node: &Node) {
        self.expr(node);
    }

    fn addr(&mut self, node: &Node) {
        match node.kind {
            NodeKind::Var { name } => {
                let offset: i32 = ((name - b'a' + 1) * 8).into();
                println!("  lea {}(%rbp), %rax", -offset);
            }
            _ => self.error_at(0, "not an lvalue"),
        }
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
            NodeKind::Eq { ref lhs, ref rhs } => {
                self.expr(rhs.as_ref());
                self.push();
                self.expr(lhs.as_ref());
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  sete %al");
                println!("  movzb %al, %rax");
            }
            NodeKind::Ne { ref lhs, ref rhs } => {
                self.expr(rhs.as_ref());
                self.push();
                self.expr(lhs.as_ref());
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  setne %al");
                println!("  movzb %al, %rax");
            }
            NodeKind::Le { ref lhs, ref rhs } => {
                self.expr(rhs.as_ref());
                self.push();
                self.expr(lhs.as_ref());
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  setle %al");
                println!("  movzb %al, %rax");
            }
            NodeKind::Lt { ref lhs, ref rhs } => {
                self.expr(rhs.as_ref());
                self.push();
                self.expr(lhs.as_ref());
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  setl %al");
                println!("  movzb %al, %rax");
            }
            NodeKind::ExprStmt { .. } => {}
            NodeKind::Var { .. } => {
                self.addr(node);
                println!("  mov (%rax), %rax");
            }
            NodeKind::Assign { ref lhs, ref rhs } => {
                self.addr(lhs);
                self.push();
                self.expr(rhs);
                self.pop("%rdi");
                println!("  mov %rax, (%rdi)");
            }
        };
    }
}
