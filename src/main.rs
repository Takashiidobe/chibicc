use std::process::exit;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args.len());
        exit(1);
    }
    let exit_code: u32 = args[1].parse().unwrap_or(0);
    println!("  .globl main");
    println!("main:");
    println!("  mov ${}, %rax", exit_code);
    println!("  ret");
}
