use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::open("14input")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut lines = contents.lines();

    let first_line = lines.next().unwrap();
    let last_c = first_line.chars().last().unwrap();
    lines.next().unwrap();

    let mut rules = HashMap::new();
    for line in lines {
        let split_line: Vec<_> = line.split(" -> ").collect();
        let pattern: Vec<_> = split_line[0].chars().collect();
        rules.insert(
            (pattern[0], pattern[1]),
            split_line[1].chars().next().unwrap()
        );
    }

    let mut state = HashMap::new();
    for (c1, c2) in first_line.chars().zip(first_line.chars().skip(1)) {
        *state.entry((c1, c2)).or_insert(0) += 1;
    }

    for _ in 0..10 {
        state = run_iteration(&state, &rules);
    }
    println!("Part 1: {}", state_score(&state, last_c));

    for _ in 10..40 {
        state = run_iteration(&state, &rules);
    }
    println!("Part 2: {}", state_score(&state, last_c));

    Ok(())
}

fn run_iteration(old_state: &HashMap<(char, char), usize>, rules: &HashMap<(char, char), char>) -> HashMap<(char, char), usize> {
    let mut state = old_state.clone();
    for ((c1, c2), count) in old_state {
        let (c1, c2) = (*c1, *c2);
        if let Some(insert) = rules.get(&(c1, c2)) {
            *state.get_mut(&(c1, c2)).unwrap() -= count;
            *state.entry((c1, *insert)).or_insert(0) += count;
            *state.entry((*insert, c2)).or_insert(0) += count;
        }
    }
    state
}

fn state_score(state: &HashMap<(char, char), usize>, last_c: char) -> usize {
    let mut char_count = HashMap::new();
    for ((c1, _), count) in state {
        *char_count.entry(c1).or_insert(0) += count;
    }
    *char_count.entry(&last_c).or_insert(0) += 1;
    char_count.values().max().unwrap() - char_count.values().min().unwrap()
}
