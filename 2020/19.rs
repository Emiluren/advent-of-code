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

    let mut expanded = vec![Vec::new(); 134];
    expand_rule(0, &rules, &mut expanded);

    let messages: Vec<&str> = lines.collect();

    let expanded_0: HashSet<String> = expanded[0].iter().cloned().collect();
    let count = messages.iter().filter(|l| expanded_0.contains(**l)).count();
    println!("Part 1: {}", count);

    let expanded_42: HashSet<String> = expanded[42].iter().cloned().collect();
    let expanded_31: HashSet<String> = expanded[31].iter().cloned().collect();

    //dbg!(&expanded_42, &expanded_31);

    let mut count2 = 0;
    for msg in messages {
        // All alternatives for rule 42 and 31 are 8 characters long
        if msg.len() % 8 != 0 {
            continue;
        }

        let chunks: Vec<_> = msg.chars()
            .collect::<Vec<_>>()
            .chunks_exact(8)
            .map(|c| c.iter().collect::<String>())
            .collect();

        'checker: for split in 1..(chunks.len() - 1) {
            let part_for_11 = chunks.len() - split;
            if part_for_11 % 2 != 0 {
                continue;
            }

            for i in 0..(split + part_for_11/2) {
                if !expanded_42.contains(&chunks[i]) {
                    continue 'checker;
                }
            }

            for i in (split + part_for_11/2)..chunks.len() {
                if !expanded_31.contains(&chunks[i]) {
                    continue 'checker;
                }
            }

            count2 += 1;
            break;
        }
    }

    println!("Part 2: {}", count2);
}

fn expand_rule(rule: usize, rules: &[Rule], result: &mut [Vec<String>]) {
    if result[rule].len() > 0 {
        return;
    }
    match rules[rule].clone() {
        Letter(c) => {
            result[rule] = vec![c.to_string()];
        }
        Combination(combs) => {
            let mut strings = Vec::new();
            for alt in combs {
                let mut expanded_alts = vec![String::new()];
                for sub_rule in alt {
                    expand_rule(sub_rule, rules, result);
                    let sub_alts = result[sub_rule].clone();
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
            result[rule] = strings;
        }
    }
}

// fn match_rule(usize: rule, msg: &str, rules: Vec<Rule>) -> Vec<usize> {
//     if msg.len() == 0 {
//         return false;
//     }

//     match rules[rule] {
//         Letter(c) => {
//             if msg.matches(c) {
//                 vec![1]
//             } else {
//                 Vec::new()
//             }
//         }
//         Combination(combs) => {
//             let mut possibilities = Vec::new();
//             'alt_loop: for alt in combs {
//                 let mut starting_index = 0;
//                 for sub_rule in alt {
//                     if let ()
//                 }
//             }
//         }
//     }
// }
