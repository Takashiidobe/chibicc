use std::process::exit;

#[derive(Debug, Clone, PartialEq)]
enum Op {
    Add,
    Sub,
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args.len());
        exit(1);
    }
    println!("  .globl main");
    println!("main:");
    let mut curr_num = String::default();
    let mut stack = vec![];
    let mut nums = vec![];
    for c in args[1].chars() {
        match c {
            '0'..='9' => curr_num.push(c),
            '+' => {
                nums.push(curr_num.parse::<i32>().unwrap());
                curr_num = String::default();
                stack.push(Op::Add);
            }
            '-' => {
                nums.push(curr_num.parse::<i32>().unwrap());
                curr_num = String::default();
                stack.push(Op::Sub);
            }
            _ => {
                eprintln!("unexpected character: {}", c);
                exit(1);
            }
        }
    }

    if !curr_num.is_empty() {
        nums.push(curr_num.parse::<i32>().unwrap());
    }

    assert!(nums.is_empty() || nums.len() - 1 == stack.len());

    println!("  mov ${}, %rax", nums[0]);

    for (num, op) in nums.into_iter().skip(1).zip(stack.into_iter()) {
        match op {
            Op::Add => println!("  add ${}, %rax", num),
            Op::Sub => println!("  sub ${}, %rax", num),
        }
    }

    println!("  ret");
}
