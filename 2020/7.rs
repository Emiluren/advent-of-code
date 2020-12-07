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
            let containers = can_be_contained_by.entry(*inner_bag_type).or_insert(Vec::new());
            containers.push(*container_bag_type);
        }
    }

    let shiny_gold = ("shiny", "gold");

    let can_contain_gold_bag = container_bag_types(shiny_gold, &can_be_contained_by);
    println!("Part 1: {}", can_contain_gold_bag.len());

    println!("Part 2: {}", count_bags_inside(shiny_gold, &rules));
}

type Bag<'a> = (&'a str, &'a str);

fn container_bag_types<'a>(
    bag: Bag<'a>,
    can_be_contained_by: &'a HashMap<Bag<'a>, Vec<Bag<'a>>>,
) -> HashSet<&'a Bag<'a>> {
    if let Some(containers) = can_be_contained_by.get(&bag) {
        return containers
            .iter()
            .flat_map(|c| container_bag_types(*c, can_be_contained_by))
            .chain(containers.iter())
            .collect();
    }
    return HashSet::new();
}

fn count_bags_inside<'a>(
    bag: Bag<'a>,
    rules: &HashMap<Bag<'a>, Vec<(usize, Bag<'a>)>>,
) -> usize {
    let mut total = 0;
    if let Some(bags_inside) = rules.get(&bag) {
        for (amount, inside_bag) in bags_inside {
            total += amount * (1 + count_bags_inside(*inside_bag, rules));
        }
    }
    total
}
