use std::fs::File;
use std::io::{self, BufRead};

fn is_symbol(c: char) -> bool {
    !c.is_numeric() && c != '.'
}

fn main() {
    let file = File::open("3input").unwrap();
    let mut map = vec![];
    let mut numbers = vec![];
    for line in io::BufReader::new(file).lines() {
        let line_str = line.unwrap();

        let mut line_numbers: Vec<(usize, String)> = vec![];
        let mut chars = line_str.chars().enumerate();
        let mut num_str = String::new();
        while let Some((col, ch)) = chars.next() {
            if ch.is_numeric() {
                num_str.push(ch);
            } else if !num_str.is_empty() {
                line_numbers.push((col-1, num_str));
                num_str = String::new();
            }
        }
        if !num_str.is_empty() {
            line_numbers.push((line_str.len()-1, num_str));
        }

        let map_row: Vec<char> = line_str.chars().collect();
        map.push(map_row);
        numbers.push(line_numbers);
    }

    let width = map[0].len();
    let height = map.len();
    let mut part_num_sum = 0;

    for (row, line_numbers) in numbers.iter().enumerate() {
        'check: for (col, num_str) in line_numbers.iter().cloned() {
            let num: u32 = num_str.parse().unwrap();
            if col < width - 1 {
                if is_symbol(map[row][col+1]) {
                    part_num_sum += num;
                    continue;
                }
                if row > 0 && is_symbol(map[row-1][col+1]) {
                    part_num_sum += num;
                    continue;
                }
                if row < height - 1 && is_symbol(map[row+1][col+1]) {
                    part_num_sum += num;
                    continue;
                }
            }
            for i in 0..num_str.len() {
                if row > 0 && is_symbol(map[row-1][col-i]) {
                    part_num_sum += num;
                    continue 'check;
                }
                if row < height - 1 && is_symbol(map[row+1][col-i]) {
                    part_num_sum += num;
                    continue 'check;
                }
            }
            if col > num_str.len() {
                if row > 0 && is_symbol(map[row-1][col - num_str.len()]) {
                    part_num_sum += num;
                    continue;
                }
                if is_symbol(map[row][col-num_str.len()]) {
                    part_num_sum += num;
                    continue;
                }
                if row < height - 1 && is_symbol(map[row+1][col - num_str.len()]) {
                    part_num_sum += num;
                }
            }
        }
    }
    println!("Part 1: {}", part_num_sum);
}