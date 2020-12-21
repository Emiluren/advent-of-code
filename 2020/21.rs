use std::collections::{HashMap, HashSet};

fn main() {
    let input = std::fs::read_to_string("21input").unwrap();

    let mut foods = Vec::new();
    let mut allergen_locations = HashMap::new();
    let mut allergen_suspicions = HashMap::new();
    for (i, line) in input.lines().enumerate() {
        let paren_location = line.find('(').unwrap();
        let (ingredient_str, allergen_str) = line.split_at(paren_location);

        let ingredients: HashSet<_> = ingredient_str.trim_end().split(' ').collect();
        let allergens: HashSet<_> = allergen_str[10..(allergen_str.len() - 1)].split(", ").collect();

        for gen in &allergens {
            let locations = allergen_locations.entry(*gen).or_insert(Vec::new());
            locations.push(i);
        }

        for ingred in &ingredients {
            let suspicions = allergen_suspicions.entry(*ingred).or_insert(HashSet::new());
            for gen in &allergens {
                suspicions.insert(gen.clone());
            }
        }

        foods.push(ingredients);
    }

    let mut cannot_contain: HashMap<&str, HashSet<&str>> = HashMap::new();
    for (allergen, food_indices) in allergen_locations {
        let mut potential_allergens = foods[food_indices[0]].clone();
        let mut all_foods = foods[food_indices[0]].clone();
        for i in food_indices.into_iter().skip(1) {
            potential_allergens = potential_allergens.intersection(&foods[i]).cloned().collect();
            all_foods = all_foods.union(&foods[i]).cloned().collect();
        }

        cannot_contain.insert(
            allergen,
            all_foods.difference(&potential_allergens).cloned().collect(),
        );
    }

    let mut safe_ingredients = HashSet::new();
    for (ingred, allergens) in allergen_suspicions {
        if allergens.into_iter().all(|gen| { cannot_contain[gen].contains(ingred) }) {
            safe_ingredients.insert(ingred);
        }
    }

    let count: usize = foods.into_iter().map(|f| f.iter().filter(|i| safe_ingredients.contains(*i)).count()).sum();
    println!("Part 1: {}", count);
}
