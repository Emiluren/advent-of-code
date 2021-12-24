use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug, PartialEq, Eq)]
enum Source { Reg(char), Val(i64) }

#[derive(Debug, PartialEq, Eq)]
enum Instruction {
    Inp(char), Add(char, Source), Mul(char, Source), Div(char, Source), Mod(char, Source), Eql(char, Source)
}
use Instruction::*;

fn parse_dest(s: &str) -> Option<Source> {
    match s {
        "w" => Some(Source::Reg('w')),
        "x" => Some(Source::Reg('x')),
        "y" => Some(Source::Reg('y')),
        "z" => Some(Source::Reg('z')),
        _ => s.parse().ok().map(|v| Source::Val(v)),
    }
}

fn digits(mut n: i64) -> [i64; 14] {
    let mut digs = [0; 14];
    for i in 0..14 {
        digs[i] = n % 10;
        n /= 10;
    }
    digs
}

fn main() -> std::io::Result<()> {
    let mut file = File::open("24input")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let input: Vec<_> = contents.lines().map(|line| {
        let words = line.split(' ').collect::<Vec<_>>();
        let a = words[1].chars().nth(0).unwrap();
        let b = words.get(2).and_then(|b| parse_dest(b));
        match words[0] {
            "inp" => Inp(a),
            "add" => Add(a, b.unwrap()),
            "mul" => Mul(a, b.unwrap()),
            "div" => Div(a, b.unwrap()),
            "mod" => Mod(a, b.unwrap()),
            "eql" => Eql(a, b.unwrap()),
            _ => panic!("Unknown instruction {}", words[0]),
        }
    }).collect();

    let test_nums = (11111111111111..99999999999999)
        .map(digits).filter(|n| !n.iter().any(|d| *d == 0)).rev();
    for n in test_nums {
        let mut regs = HashMap::new();
        regs.insert('w', 0);
        regs.insert('x', 0);
        regs.insert('y', 0);
        regs.insert('z', 0);

        let mut inbuf = n.iter();

        macro_rules! getb {
            ($b:expr) => {
                match $b {
                    Source::Reg(r) => regs[r],
                    Source::Val(v) => *v,
                }
            }
        }

        for instr in &input {
            match instr {
                Inp(a) => {
                    regs.insert(*a, *inbuf.next().unwrap());
                }
                Add(a, b) => {
                    regs.insert(*a, regs[a] + getb!(b));
                }
                Mul(a, b) => {
                    regs.insert(*a, regs[a] * getb!(b));
                }
                Div(a, b) => {
                    regs.insert(*a, regs[a] / getb!(b));
                }
                Mod(a, b) => {
                    regs.insert(*a, regs[a] % getb!(b));
                }
                Eql(a, b) => {
                    regs.insert(*a, if regs[a] == getb!(b) { 1 } else { 0 });
                }
            }
        }

        if regs[&'z'] == 0 {
            println!("Part 1: {}", n.iter().enumerate().map(|(i, d)| 10_i64.pow(i as u32) * d).sum::<i64>());
            break;
        }
    }

    Ok(())
}
