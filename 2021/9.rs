use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("9input").unwrap();
    let mut reader = io::BufReader::new(file).lines();
    let mut input = Vec::new();

    while let Some(Ok(line)) = reader.next() {
        let mut row = Vec::new();
        for c in line.chars() {
            row.push(c.to_string().parse::<u8>().unwrap());
        }
        input.push(row);
    }

    let mut risk_sum = 0;
    for (r, row) in input.iter().enumerate() {
        for (c, n) in row.iter().enumerate() {
            if neighbors(&input, r, c).iter().all(|m| *n < *m) {
                risk_sum += *n as u32 + 1;
            }
        }
    }
    println!("Part 1: {}", risk_sum);
}

fn neighbors(input: &[Vec<u8>], r: usize, c: usize) -> Vec<u8> {
    let mut res = Vec::new();
    if r > 0 {
        res.push(input[r-1][c]);
    }
    if c > 0 {
        res.push(input[r][c-1]);
    }
    if r < input.len() - 1 {
        res.push(input[r+1][c]);
    }
    if c < input[r].len() - 1 {
        res.push(input[r][c+1]);
    }
    res
}
