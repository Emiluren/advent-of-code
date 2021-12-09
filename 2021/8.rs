use std::collections::{BTreeSet, BTreeMap};
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("8input").unwrap();
    let mut input: Vec<(Vec<BTreeSet<char>>, Vec<BTreeSet<char>>)> = Vec::new();
    let mut reader = io::BufReader::new(file).lines();
    while let Some(Ok(line)) = reader.next() {
        let mut split_line = line.split(" | ");
        let signal_patterns_str = split_line.next().unwrap();
        let output_value_str = split_line.next().unwrap();
        input.push((
            signal_patterns_str.split(' ').map(|s| s.chars().collect()).collect(),
            output_value_str.split(' ').map(|s| s.chars().collect()).collect(),
        ));
    }

    let dig_seg_str = [
        "abcefg",
        "cf",
        "acdeg",
        "acdfg",
        "bcdf",
        "abdfg",
        "abdefg",
        "acf",
        "abcdefg",
        "abcdfg",
    ];
    let mut count_to_dig = vec![Vec::new(); 8];
    for (i, ds) in dig_seg_str.iter().enumerate() {
        count_to_dig[ds.chars().count()].push(i);
    }

    // part 2 solution was stolen from /u/4HbQ
    let mut count_1478 = 0;
    let mut sum = 0;
    for (signal_patterns, output_values) in input {
        let length_map: BTreeMap<usize, _> = signal_patterns.into_iter().map(|s| (s.len(), s.clone())).collect();
        for (i, pat) in output_values.iter().enumerate() {
            let l = pat.len();
            if count_to_dig[l].len() == 1 {
                count_1478 += 1;
            }
            let inters_4 = pat.intersection(&length_map[&4]).count();
            let inters_1 = pat.intersection(&length_map[&2]).count();
            let n = match (l, inters_4, inters_1) {
                (2,_,_) => 1,
                (3,_,_) => 7,
                (4,_,_) => 4,
                (7,_,_) => 8,
                (5,2,_) => 2,
                (5,3,1) => 5,
                (5,3,2) => 3,
                (6,4,_) => 9,
                (6,3,1) => 6,
                (6,3,2) => 0,
                _ => panic!("something's wrong"),
            };
            sum += 10_usize.pow(3-i as u32) * n;
        }
    }
    println!("Part 1: {}", count_1478);
    println!("Part 2: {}", sum);
}
