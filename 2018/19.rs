#[derive(Clone, Copy)]
enum Instructions {
    Addr(usize, usize, usize),
    Addi(usize, usize, usize),
    Mulr(usize, usize, usize),
    Muli(usize, usize, usize),
    Setr(usize, usize),
    Seti(usize, usize),
    Gtrr(usize, usize, usize),
    Eqrr(usize, usize, usize)
}

fn run_program(mut regs: [usize; 6]) -> usize {
    while regs[2] < PROGRAM.len() {
        let pc = regs[2];
        let instruction = PROGRAM[pc];
        match instruction {
            Instructions::Addr(a, b, c) => regs[c] = regs[a] + regs[b],
            Instructions::Addi(a, b, c) => regs[c] = regs[a] + b,
            Instructions::Mulr(a, b, c) => regs[c] = regs[a] * regs[b],
            Instructions::Muli(a, b, c) => regs[c] = regs[a] * b,
            Instructions::Setr(a, c) => regs[c] = regs[a],
            Instructions::Seti(a, c) => regs[c] = a,
            Instructions::Gtrr(a, b, c) => regs[c] = if regs[a] > regs[b] { 1 } else { 0 },
            Instructions::Eqrr(a, b, c) => regs[c] = if regs[a] == regs[b] { 1 } else { 0 },
        }
        regs[2] += 1;
    }
    regs[0]
}

fn main() {
    let mut regs = [0, 0, 0, 0, 0, 0];
    println!("Part 1: {}", run_program(regs));

    regs = [1, 0, 0, 0, 0, 0];
    println!("Part 2: {}", run_program(regs));
}

const PROGRAM: [Instructions; 36] = [
    Instructions::Addi(2, 16, 2),
    Instructions::Seti(1, 1),
    Instructions::Seti(1, 3),
    Instructions::Mulr(1, 3, 5),
    Instructions::Eqrr(5, 4, 5),
    Instructions::Addr(5, 2, 2),
    Instructions::Addi(2, 1, 2),
    Instructions::Addr(1, 0, 0),
    Instructions::Addi(3, 1, 3),
    Instructions::Gtrr(3, 4, 5),
    Instructions::Addr(2, 5, 2),
    Instructions::Seti(2, 2),
    Instructions::Addi(1, 1, 1),
    Instructions::Gtrr(1, 4, 5),
    Instructions::Addr(5, 2, 2),
    Instructions::Seti(1, 2),
    Instructions::Mulr(2, 2, 2),
    Instructions::Addi(4, 2, 4),
    Instructions::Mulr(4, 4, 4),
    Instructions::Mulr(2, 4, 4),
    Instructions::Muli(4, 11, 4),
    Instructions::Addi(5, 6, 5),
    Instructions::Mulr(5, 2, 5),
    Instructions::Addi(5, 19, 5),
    Instructions::Addr(4, 5, 4),
    Instructions::Addr(2, 0, 2),
    Instructions::Seti(0, 2),
    Instructions::Setr(2, 5),
    Instructions::Mulr(5, 2, 5),
    Instructions::Addr(2, 5, 5),
    Instructions::Mulr(2, 5, 5),
    Instructions::Muli(5, 14, 5),
    Instructions::Mulr(5, 2, 5),
    Instructions::Addr(4, 5, 4),
    Instructions::Seti(0, 0),
    Instructions::Seti(0, 2),
];
