use std::collections::HashSet;
use std::fs;

#[derive(Copy, Clone)]
enum Op {
    Acc, Jmp, Nop
}
use Op::*;

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

    let mut pc = 0;
    let mut acc = 0;
    let mut visited_locations = HashSet::new();
    while !visited_locations.contains(&pc) {
        visited_locations.insert(pc);
        let (op, val) = instructions[pc];
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

    println!("Part 1 {}", acc);
}
