#[macro_use]
extern crate lazy_static;

use std::env;

use crate::{codegen::Codegen, lexer::Lexer, parser::Parser};

pub mod codegen;
pub mod errors;
pub mod lexer;
pub mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments", args[0]);
    }

    let src = args[1].as_bytes();

    let lexer = Lexer::new(src);

    let toks = lexer.tokenize();

    let mut parser = Parser::new(src, &toks);

    let source_unit = parser.source_unit();
    parser.ensure_done();

    let mut codegen = Codegen::new(src, source_unit);
    codegen.program();
    codegen.sanity_checks();
}
