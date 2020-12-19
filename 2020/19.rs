use std::collections::HashSet;

#[derive(Clone, Debug)]
enum Rule {
    Letter(char),
    Combination(Vec<Vec<usize>>),
}
use Rule::*;

fn main() {
    let input = std::fs::read_to_string("19input").unwrap();
    let mut lines = input.lines();

    let mut rules = vec![Combination(Vec::new()); 134];
    while let Some(line) = lines.next() {
        if line == "" {
            break;
        }

        let index: usize = line.chars()
            .take_while(char::is_ascii_digit)
            .collect::<String>()
            .parse()
            .unwrap();

        let mut chars = line.chars().peekable();
        while let Some(c) = chars.next() {
            if !c.is_ascii_digit() {
                break;
            }
        }

        assert_eq!(chars.next(), Some(' '));

        rules[index] = if chars.peek() == Some(&'"') {
            chars.next().unwrap();
            Letter(chars.next().unwrap())
        } else {
            let rest: String = chars.collect();
            Combination(
                rest.split('|')
                    .map(|alt| alt.trim().split(' ').map(|n| n.parse().unwrap()).collect())
                    .collect()
            )
        };
    }

    let rules0: HashSet<String> = expand_rule(0, &rules).into_iter().collect();
    let count = lines.filter(|l| rules0.contains(*l)).count();
    println!("Part 1: {}", count);
}

fn expand_rule(rule: usize, rules: &[Rule]) -> Vec<String> {
    match rules[rule].clone() {
        Letter(c) => {
            vec![c.to_string()]
        }
        Combination(combs) => {
            let mut strings = Vec::new();
            for alt in combs {
                let mut expanded_alts = vec![String::new()];
                for sub_rule in alt {
                    let sub_alts = expand_rule(sub_rule, rules);
                    let old_len = expanded_alts.len();
                    if sub_alts.len() > 1 {
                        let old = expanded_alts.clone();
                        for _ in 1..sub_alts.len() {
                            expanded_alts.append(&mut old.clone());
                        }
                    }
                    for (i, ex_sub) in sub_alts.into_iter().enumerate() {
                        for j in 0..old_len {
                            expanded_alts[i * old_len + j] += &ex_sub;
                        }
                    }
                }
                strings.append(&mut expanded_alts);
            }
            strings
        }
    }
}
