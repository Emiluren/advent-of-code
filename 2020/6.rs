use std::collections::HashSet;
use std::fs;

fn main() {
    let contents = fs::read_to_string("6input").unwrap();
    let groups: Vec<Vec<&str>> = contents.split("\n\n").map(|s| s.lines().collect()).collect();

    let mut any_count = 0;
    let mut all_count = 0;
    for group in groups {
        let mut any_answered = HashSet::new();
        let mut all_answered: HashSet<_> = ('a'..='z').collect();

        for person in group {
            let mut i_answered = HashSet::new();
            for c in person.chars() {
                any_answered.insert(c);
                i_answered.insert(c);
            }

            all_answered.retain(|c| i_answered.contains(c));
        }
        any_count += any_answered.len();
        all_count += all_answered.len();
    }

    println!("Part 1: {}", any_count);
    println!("Part 2: {}", all_count);
}
