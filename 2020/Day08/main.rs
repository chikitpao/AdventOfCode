//  Aoc 2020, Day 8: Handheld Halting
//  Author: Chi-Kit Pao
//
//  Outputs:
//  Question 1: Immediately before any instruction is executed a second time, what value is in the accumulator?
//  Answer: 2034
//  Question 2: What is the value of the accumulator after the program terminates?
//  Answer: 672
//
//  Time usage shown via command "time".
//  real	0m0,048s
//  user	0m0,037s
//  sys	0m0,012s
//

use std::collections::HashSet;
use std::fs;

#[derive(PartialEq)]
enum Op {
    Acc,
    Jmp,
    Nop,
}

#[derive(PartialEq)]
enum RetCode {
    Continue,
    Loop,
    End,
}

struct Instr {
    op: Op,
    arg: i32,
}

struct Console
{
    instructions : Vec<Instr>,
    accumulator : i32,
    ip : i32,   // instruction pointer
    history : HashSet<i32>,
}

fn execute(console: &mut Console) -> RetCode {
    if console.history.contains(&console.ip) {
        return RetCode::Loop;
    }

    let ip : usize = console.ip.try_into().unwrap();
    if console.ip < 0 || ip >= console.instructions.len() {
        return RetCode::End;
    }
    console.history.insert(console.ip);

    if console.instructions[ip].op == Op::Acc {
        console.accumulator += console.instructions[ip].arg;
        console.ip += 1;
    } else if console.instructions[ip].op == Op::Jmp {
        console.ip += console.instructions[ip].arg;
    } else if console.instructions[ip].op == Op::Nop {
        console.ip += 1;
    } else {
        panic!("Unknown Op value during execution!");
    }
    return RetCode::Continue;
}

fn main() {
    // Read file
    let file_path = "input.txt";
    let contents = fs::read_to_string(file_path)
        .expect("Cannot read file.");

    let mut console = Console{instructions: Vec::new(), accumulator: 0,
         ip: 0, history: HashSet::new()};

    // Parse file
    for line in contents.lines() {
        let parts: Vec<&str> = line.split(' ').collect();
        let op : Op;
        if parts[0] == "acc" {
            op = Op::Acc;
        } else if parts[0] == "jmp" {
            op = Op::Jmp;
        } else if parts[0] == "nop" {
            op = Op::Nop;
        } else {
             panic!("Unknow op value {}!", parts[0]);
        }

        console.instructions.push(Instr{op: op, arg: parts[1].parse().unwrap()});
    }

    // Run program for part 1
    loop {
        if execute(&mut console) != RetCode::Continue {
            break;
        }
    }
    println!("Question 1: Immediately before any instruction is executed a second time, what value is in the accumulator?");
    println!("Answer: {}", console.accumulator);

    // Run program for part 2.
    // Change executed jmp or nop instruction until program can terminate.
    let mut codepath : Vec<usize> = Vec::new();
    for v in &console.history {
        if console.instructions[*v as usize].op != Op::Acc {
            codepath.push(*v as usize)
        }
    }
    codepath.sort();
    for i in 0..(codepath.len()) {
        // Reset console
        console.accumulator = 0;
        console.ip = 0;
        console.history.clear();

        // flip opcode
        if console.instructions[codepath[i]].op == Op::Jmp {
            console.instructions[codepath[i]].op = Op::Nop;
        } else {
            console.instructions[codepath[i]].op = Op::Jmp;
        }

        let mut retcode : RetCode;
        loop {
            retcode = execute(&mut console);
            if retcode != RetCode::Continue {
                break;
            }
        }
        if retcode == RetCode::End {
            break;
        }
        if retcode == RetCode::Loop {
            // flip opcode back
            if console.instructions[codepath[i]].op == Op::Jmp {
                console.instructions[codepath[i]].op = Op::Nop;
            } else {
                console.instructions[codepath[i]].op = Op::Jmp;
            }
        }
    }

    println!("Question 2: What is the value of the accumulator after the program terminates?");
    println!("Answer: {}", console.accumulator);
}
