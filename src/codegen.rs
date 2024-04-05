use std::collections::HashMap;

use crate::ErrorReporting;
use crate::Node;
use crate::NodeKind;

#[derive(Debug, Clone, PartialEq)]
pub struct Codegen<'a> {
    pub src: &'a [u8],
    pub depth: i64,
    pub vars: HashMap<String, usize>,
}

impl<'a> ErrorReporting for Codegen<'a> {
    fn src(&self) -> &[u8] {
        self.src
    }
}

impl<'a> Codegen<'a> {
    pub fn new(src: &'a [u8], vars: HashMap<String, usize>) -> Self {
        Self {
            src,
            depth: 0,
            vars,
        }
    }

    pub fn program(&mut self, node: &Node) {
        println!("  .globl main");
        println!("main:");

        // Prologue
        println!("  push %rbp");
        println!("  mov %rsp, %rbp");
        println!("  sub ${}, %rsp", self.vars.len() * 8);
        println!();

        self.stmt(node);

        // Epilogue
        println!();
        println!(".L.return:");
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
            NodeKind::Var { ref name } => {
                // can't fail, otherwise its a reference error.
                let offset = *self.vars.get(name).unwrap();
                println!("  lea -{}(%rbp), %rax", offset);
            }
            _ => self.error_at(0, "not an lvalue"),
        }
    }

    fn expr(&mut self, node: &Node) {
        match node.kind {
            NodeKind::Num { val } => println!("  mov ${}, %rax", val),
            NodeKind::Add { ref lhs, ref rhs } => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  add %rdi, %rax");
            }
            NodeKind::Sub { ref lhs, ref rhs } => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  sub %rdi, %rax");
            }
            NodeKind::Mul { ref lhs, ref rhs } => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  imul %rdi, %rax");
            }
            NodeKind::Div { ref lhs, ref rhs } => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  cqo");
                println!("  idiv %rdi, %rax");
            }
            NodeKind::Neg { ref expr } => {
                self.expr(expr);
                println!("  neg %rax");
            }
            NodeKind::Eq { ref lhs, ref rhs } => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  sete %al");
                println!("  movzb %al, %rax");
            }
            NodeKind::Ne { ref lhs, ref rhs } => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  setne %al");
                println!("  movzb %al, %rax");
            }
            NodeKind::Le { ref lhs, ref rhs } => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  setle %al");
                println!("  movzb %al, %rax");
            }
            NodeKind::Lt { ref lhs, ref rhs } => {
                self.expr(rhs);
                self.push();
                self.expr(lhs);
                self.pop("%rdi");
                println!("  cmp %rdi, %rax");
                println!("  setl %al");
                println!("  movzb %al, %rax");
            }
            NodeKind::ExprStmt { ref lhs, .. } => {
                self.expr(lhs);
            }
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
            NodeKind::Return { ref lhs } => {
                self.expr(lhs);
                println!("  jmp .L.return");
            }
            NodeKind::Block { ref body } => {
                for node in body {
                    self.stmt(node);
                }
            }
        };
    }
}
