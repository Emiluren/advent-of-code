use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::open("14input")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let mut lines = contents.lines();

    let mut state: Vec<_> = lines.next().unwrap().chars().collect();
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

    for ((c1, c2), insert) in &rules {
        println!("{}{} -> {}", c1, c2, insert);
    }

    for _ in 0..10 {
        //println!("i = {}", i);
        //dbg!(state.iter().collect::<String>());
        state = run_iteration(&state, &rules);
    }

    let mut char_count = HashMap::new();
    for c in state {
        *char_count.entry(c).or_insert(0) += 1;
    }
    dbg!(&char_count);
    let mut count_vec: Vec<_> = char_count.values().copied().collect();
    count_vec.sort();

    let l = count_vec.len();
    println!("Part 1: {}", count_vec[l-1] - count_vec[0]);

    Ok(())
}

fn run_iteration(state: &[char], rules: &HashMap<(char, char), char>) -> Vec<char> {
    let mut new_state = Vec::new();
    for (c1, c2) in state.iter().copied().take(state.len()-1).zip(state.iter().copied().skip(1)) {
        new_state.push(c1);
        if let Some(insert) = rules.get(&(c1, c2)) {
            new_state.push(*insert);
        }
    }
    new_state.push(state[state.len()-1]);
    new_state
}
