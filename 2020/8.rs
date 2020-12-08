use std::collections::HashSet;
use std::fs;

#[derive(Copy, Clone)]
enum Op {
    Acc, Jmp, Nop
}
use Op::*;

#[derive(Eq, PartialEq)]
enum ProgramResult {
    Terminated,
    Loop,
}

fn main() {
    let contents = fs::read_to_string("8input").unwrap();
    let instructions: Vec<(Op, isize)> = contents.lines().map(|line| {
        let mut words = line.split(' ');
        let op_str = words.next().unwrap();
        let val = words.next().unwrap();

        let op = match op_str {
            "acc" => Acc,
            "jmp" => Jmp,
            "nop" => Nop,
            _ => panic!("Unknown opcode {}", op_str),
        };

        (op, val.parse().unwrap())
    }).collect();

    println!("Part 1 {}", run_program(&instructions, None).0);

    for (i, (op, _)) in instructions.iter().enumerate() {
        let substitute = match op {
            Acc => continue,
            Jmp => (i, Nop),
            Nop => (i, Jmp),
        };

        let (acc, res) = run_program(&instructions, Some(substitute));
        if res == ProgramResult::Terminated {
            println!("Part 2 {}", acc);
            break;
        }
    }
}

fn run_program(
    instructions: &[(Op, isize)],
    substitute: Option<(usize, Op)>,
) -> (isize, ProgramResult) {
    let mut pc = 0;
    let mut acc = 0;
    let mut visited_locations = HashSet::new();
    while !visited_locations.contains(&pc) && pc < instructions.len() {
        visited_locations.insert(pc);
        let (op, val) = instructions[pc];

        let op = substitute.map(|(addr, sub_op)| {
            if addr == pc { sub_op } else { op }
        }).unwrap_or(op);

        match op {
            Acc => {
                acc += val;
                pc += 1;
            }
            Jmp => {
                if val >= 0 {
                    pc += val as usize;
                } else {
                    pc -= (-val) as usize;
                }
            }
            Nop => pc += 1,
        }
    }

    let result = if pc >= instructions.len() {
        ProgramResult::Terminated
    } else {
        ProgramResult::Loop
    };

    (acc, result)
}
