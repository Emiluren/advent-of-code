use std::fs;
use itertools::Itertools;

fn main() {
    let contents = fs::read_to_string("2input").unwrap();

    let mut valid_passwords1 = 0;
    let mut valid_passwords2 = 0;
    for line in contents.lines() {
        let mut characters = line.chars().peekable();
        let lower_str: String = characters.by_ref().peeking_take_while(|c| c.is_digit(10)).collect();
        let lower: usize = lower_str.parse().unwrap();

        assert_eq!(characters.next(), Some('-'));

        let upper_str: String = characters.by_ref().peeking_take_while(|c| c.is_digit(10)).collect();
        let upper = upper_str.parse().unwrap();

        assert_eq!(characters.next(), Some(' '));

        let limit_char = characters.next().unwrap();

        assert_eq!(characters.next(), Some(':'));
        assert_eq!(characters.next(), Some(' '));

        // Part 1
        let characters2 = characters.clone();
        let char_count = characters.filter(|c| *c == limit_char).count();

        if char_count >= lower && char_count <= upper {
            valid_passwords1 += 1;
        }

        // Part 2
        let mut characters_from_c1 = characters2.skip(lower - 1).peekable();
        let c1 = *characters_from_c1.peek().unwrap() == limit_char;
        let c2 = characters_from_c1.skip(upper - lower).next().unwrap() == limit_char;

        if (c1 && !c2) || (!c1 && c2) {
            valid_passwords2 += 1;
        }
    }

    println!("Part 1: {}", valid_passwords1);
    println!("Part 2: {}", valid_passwords2);
}
