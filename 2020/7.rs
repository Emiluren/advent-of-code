use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    let contents = fs::read_to_string("7input").unwrap();

    // parse input
    let mut rules = HashMap::new();
    for line in contents.lines() {
        let mut word_iter = line.split(' ');
        let shade = word_iter.next().unwrap();
        let color = word_iter.next().unwrap();

        let mut contains = Vec::new();
        while let Some(num_string) = word_iter.next() {
            let s = word_iter.next().unwrap();
            let c = word_iter.next().unwrap();
            contains.push((num_string.parse::<usize>().unwrap(), (s, c)));
        }

        rules.insert((shade, color), contains);
    }

    // turn rules "inside out"
    let mut can_be_contained_by = HashMap::new();
    for (container_bag_type, contains) in &rules {
        for (_, inner_bag_type) in contains {
            let containers = can_be_contained_by.entry(inner_bag_type).or_insert(Vec::new());
            containers.push(*container_bag_type);
        }
    }

    let shiny_gold = ("shiny", "gold");

    // solve part 1
    let mut can_contain_gold_bag = HashSet::new();
    let mut contain_stack = vec![shiny_gold];
    let empty = Vec::new();
    while let Some(bag_type) = contain_stack.pop() {
        for container in can_be_contained_by.get(&bag_type).unwrap_or(&empty) {
            if !can_contain_gold_bag.contains(container) {
                can_contain_gold_bag.insert(container);
                contain_stack.push(*container);
            }
        }
    }
    println!("Part 1: {}", can_contain_gold_bag.len());

    println!("Part 2: {}", count_bags_inside(shiny_gold, &rules));
}

type Bag<'a> = (&'a str, &'a str);

fn count_bags_inside<'a>(
    bag: Bag<'a>,
    rules: &HashMap<Bag<'a>, Vec<(usize, Bag<'a>)>>
) -> usize {
    let mut total = 0;
    if let Some(bags_inside) = rules.get(&bag) {
        for (amount, inside_bag) in bags_inside {
            total += amount * (1 + count_bags_inside(*inside_bag, rules));
        }
    }
    total
}
