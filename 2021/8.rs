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
    let mut dig_segs = vec![BTreeSet::new(); 10];
    let mut count_to_dig = vec![Vec::new(); 8];
    let mut segs_to_dig = BTreeMap::new();
    for (i, ds) in dig_seg_str.iter().enumerate() {
        let segs: BTreeSet<char> = ds.chars().collect();
        count_to_dig[segs.len()].push(i);
        segs_to_dig.insert(segs.clone(), i);
        dig_segs.push(segs);
    }

    let mut count_1478 = 0;
    let mut sum = 0;
    for (signal_patterns, output_values) in input {
        for pat in &output_values {
            if count_to_dig[pat.len()].len() == 1 {
                count_1478 += 1;
            }
        }

        let map = find_mapping(&dig_segs, &signal_patterns, BTreeMap::new()).unwrap();
        for (i, pat) in output_values.iter().enumerate() {
            let translated = pat.iter().map(|c| map[c]).collect();
            sum += 10_usize.pow(3-i as u32) * segs_to_dig[&translated];
        }
    }
    println!("Part 1: {}", count_1478);
    println!("Part 2: {}", sum);
}

const SEGS: &'static str = "abcdefg";

fn find_mapping(
    dig_segs: &[BTreeSet<char>],
    signal_patterns: &[BTreeSet<char>],
    current_map: BTreeMap<char, char>,
) -> Option<BTreeMap<char, char>> {
    if !valid_map(dig_segs, signal_patterns, &current_map) {
        return None;
    }
    if SEGS.chars().all(|c| current_map.contains_key(&c)) {
        return Some(current_map);
    }

    for c in SEGS.chars() {
        if !current_map.contains_key(&c) {
            let taken: BTreeSet<char> = current_map.values().cloned().collect();
            for c2 in SEGS.chars().filter(|c2| !taken.contains(c2)) {
                let mut map = current_map.clone();
                map.insert(c, c2);
                let new_map = find_mapping(dig_segs, signal_patterns, map);
                if new_map.is_some() {
                    return new_map;
                }
            }
        }
    }
    None
}

fn valid_map(
    dig_segs: &[BTreeSet<char>],
    signal_patterns: &[BTreeSet<char>],
    current_map: &BTreeMap<char, char>,
) -> bool {
    for pat in signal_patterns {
        let possibilities: Vec<_> = dig_segs.iter().filter(|ds| ds.len() == pat.len()).collect();
        let mut found_pos = false;
        for pos in possibilities {
            if pat.iter().all(|c| !current_map.contains_key(&c) || pos.contains(&current_map[c])) {
                found_pos = true;
                break;
            }
        }
        if !found_pos {
            return false;
        }
    }
    return true;
}

fn print_alts(dig_rep: &[Vec<BTreeSet<char>>]) {
    for (i, alts) in dig_rep.iter().enumerate() {
        print!("{} ", i);
        for (j, alt) in alts.iter().enumerate() {
            for c in alt {
                print!("{}", c);
            }
            if j != alts.len() - 1 {
                print!(" ");
            }
        }
        println!();
    }
}
