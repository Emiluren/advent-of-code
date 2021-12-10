use std::collections::{BinaryHeap, HashMap};
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::open("10input")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let input: Vec<_> = contents.lines().collect();

    let closing: HashMap<char, char> = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')].into_iter().collect();
    let error_points: HashMap<char, _> = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)].into_iter().collect();
    let corr_points: HashMap<char, _> = [(')', 1), (']', 2), ('}', 3), ('>', 4)].into_iter().collect();

    let mut error_total = 0;
    let mut corr_scores = BinaryHeap::new();
    for line in input {
        let mut stack = Vec::new();
        let mut valid = true;
        for c in line.chars() {
            if closing.values().any(|c2| *c2 == c) {
                if stack.len() == 0 || closing[&stack.pop().unwrap()] != c {
                    error_total += error_points[&c];
                    valid = false;
                    break;
                }
            } else {
                stack.push(c);
            }
        }
        if valid {
            let score = stack.iter().rev().fold(0_u64, |acc, c| {
                acc*5 + corr_points[&closing[c]]
            });
            corr_scores.push(score);
        }
    }
    println!("Part 1: {}", error_total);

    let s = corr_scores.into_sorted_vec();
    println!("Part 2: {}", s[s.len() / 2]);
    Ok(())
}
