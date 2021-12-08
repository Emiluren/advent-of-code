use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("8input").unwrap();
    let mut input: Vec<(Vec<Vec<char>>, Vec<Vec<char>>)> = Vec::new();
    let mut reader = io::BufReader::new(file).lines();
    while let Some(Ok(line)) = reader.next() {
        let mut split_line = line.split('|');
        let signal_patterns_str = split_line.next().unwrap();
        let output_value_str = split_line.next().unwrap();
        input.push((
            signal_patterns_str.split(' ').map(|s| s.chars().collect()).collect(),
            output_value_str.split(' ').map(|s| s.chars().collect()).collect(),
        ));
    }

    let mut count_1478 = 0;
    for (_, output_values) in input {
        for v in output_values {
            let l = v.len();
            if l == 2 || l == 4 || l == 3 || l == 7 {
                count_1478 += 1;
            }
        }
    }
    println!("Part 1: {}", count_1478);
}
