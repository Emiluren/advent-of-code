use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn is_symbol(c: char) -> bool {
    !c.is_numeric() && c != '.'
}

fn main() {
    let file = File::open("3input").unwrap();
    let mut map = vec![];
    let mut numbers = vec![];
    let mut gears = HashMap::new();
    for (row, line) in io::BufReader::new(file).lines().enumerate() {
        let line_str = line.unwrap();

        let mut line_numbers: Vec<(usize, String)> = vec![];
        let mut chars = line_str.chars().enumerate();
        let mut num_str = String::new();
        while let Some((col, ch)) = chars.next() {
            if ch.is_numeric() {
                num_str.push(ch);
            } else {
                if !num_str.is_empty() {
                    line_numbers.push((col-1, num_str));
                    num_str = String::new();
                }
                if ch == '*' {
                    gears.insert((row, col), vec![]);
                }
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
        for (col, num_str) in line_numbers.iter().cloned() {
            let num: u32 = num_str.parse().unwrap();
            let mut added = false;
            let mut check_sym = |r: usize, c: usize| {
                let ch = map[r][c];
                if is_symbol(ch) {
                    if !added {
                        part_num_sum += num;
                        added = true;
                    }
                    if ch == '*' {
                        gears.get_mut(&(r, c)).unwrap().push(num);
                    }
                }
            };
            if col < width - 1 {
                check_sym(row, col+1);
                if row > 0 { check_sym(row-1, col+1); }
                if row < height - 1 { check_sym(row+1, col+1); }
            }
            for i in 0..num_str.len() {
                if row > 0 { check_sym(row-1, col-i); }
                if row < height - 1 { check_sym(row+1, col-i); }
            }
            if col > num_str.len() {
                if row > 0 { check_sym(row-1, col - num_str.len()); }
                check_sym(row, col-num_str.len());
                if row < height - 1 { check_sym(row+1, col - num_str.len()); }
            }
        }
    }
    println!("Part 1: {}", part_num_sum);

    let mut gear_sum = 0;
    for gear in gears.values() {
        if gear.len() == 2 {
            gear_sum += gear.iter().product::<u32>();
        }
    }
    println!("Part 2: {}", gear_sum);
}