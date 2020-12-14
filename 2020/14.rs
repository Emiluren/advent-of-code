use std::collections::HashMap;
use std::fs;

#[derive(Debug)]
enum Instruction {
    Mask(u64, u64),
    Mem(usize, u64),
}
use Instruction::*;

fn main() {
    let contents = fs::read_to_string("14input").unwrap();
    let instructions: Vec<_> = contents.lines().map(|l| {
        let mut sides = l.split(" = ");
        let (instr, data) = (sides.next().unwrap(), sides.next().unwrap());

        if instr == "mask" {
            parse_mask(data)
        } else {
            assert!(instr.starts_with("mem["));
            let addr = instr
                .chars()
                .skip(4)
                .take_while(|c| c.is_digit(10))
                .collect::<String>()
                .parse()
                .unwrap();
            Mem(addr, data.parse().unwrap())
        }
    }).collect();

    let mut one_mask = 0;
    let mut zero_mask = !0;
    let mut memory = HashMap::new();
    for instr in &instructions {
        match instr {
            Mask(o, z) => {
                one_mask = *o;
                zero_mask = *z;
            }
            Mem(addr, val) => {
                memory.insert(addr, (val | one_mask) & zero_mask);
            }
        }
    }

    println!("Part 1: {}", memory.values().sum::<u64>());
}

fn parse_mask(mask_str: &str) -> Instruction {
    let mut one_mask = 0;
    let mut zero_mask = !0;

    assert_eq!(mask_str.chars().count(), 36);

    for (i, c) in mask_str.chars().enumerate() {
        let byte_index = 35 - i;
        let shifted_one = 1 << byte_index;

        match c {
            'X' => {}
            '1' => {
                one_mask |= shifted_one;
            }
            '0' => {
                zero_mask ^= shifted_one;
            }
            _ => {
                panic!("Unknown character in mask {}", c);
            }
        }
    }

    // println!("{:064b}", one_mask);
    // println!("{:064b}", zero_mask);

    Mask(one_mask, zero_mask)
}
