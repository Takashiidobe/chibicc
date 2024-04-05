use chibicc::codegen::Codegen;
use chibicc::parse::Parser;
use chibicc::tokenize::Lexer;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("{}: invalid number of arguments", args[0]);
    }

    let src = args[1].as_bytes();

    let mut lexer = Lexer::new(src);

    let toks = lexer.tokenize();

    let mut parser = Parser::new(src, &toks);
    let (nodes, vars) = parser.parse();
    parser.ensure_done();

    let mut codegen = Codegen::new(src, vars);
    codegen.program(&nodes);

    assert!(codegen.depth == 0);
}
