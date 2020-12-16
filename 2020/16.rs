use text_io::try_scan;

fn parser(line: &str) -> Result<(String, [usize; 4]), Box<dyn std::error::Error>> {
    let field: String;
    let mut limits: [usize; 4] = [0; 4];
    try_scan!(line.bytes() => "{}: {}-{} or {}-{}", field, limits[0], limits[1], limits[2], limits[3]);

    Ok((field, limits))
}

fn in_limits(n: usize, limits: [usize; 4]) -> bool {
    let [min1, max1, min2, max2] = limits;
    let in_range1 = || min1 <= n && n <= max1;
    let in_range2 = || min2 <= n && n <= max2;
    in_range1() || in_range2()
}

fn main () {
    let contents = std::fs::read_to_string("16input").unwrap();
    let mut lines = contents.lines().peekable();

    let mut rules = Vec::new();
    while let Ok(rule) = parser(lines.next().unwrap()) {
        rules.push(rule);
    }

    assert_eq!(lines.next().unwrap(), "your ticket:");
    let my_numbers: Vec<usize> = lines.next().unwrap().split(',').map(|s| s.parse().unwrap()).collect();

    assert_eq!(lines.next().unwrap(), "");
    assert_eq!(lines.next().unwrap(), "nearby tickets:");

    let mut valid_tickets = Vec::new();
    let mut invalid_sum = 0;
    while let Some(line) = lines.next() {
        let numbers: Vec<_> = line.split(',').map(|s| s.parse().unwrap()).collect();
        let mut valid = true;

        'outer: for n in &numbers {
            for (_, limits) in rules.iter().cloned() {
                if in_limits(*n, limits) {
                    continue 'outer;
                }
            }

            invalid_sum += *n;
            valid = false;
        }

        if valid {
            valid_tickets.push(numbers);
        }
    }

    println!("Part 1: {}", invalid_sum);

    let mut rule_possibilities = Vec::new();
    for field_i in 0..valid_tickets[0].len() {
        let mut possible_rules = Vec::new();

        'rule_loop: for (rule_i, (_, limits)) in rules.iter().enumerate() {
            for ticket in &valid_tickets {
                if !in_limits(ticket[field_i], *limits) {
                    continue 'rule_loop;
                }
            }

            possible_rules.push(rule_i);
        }

        rule_possibilities.push((field_i, possible_rules));
    }
    rule_possibilities.sort_by_key(|(_, r)| r.len());

    let mut decided_rules = vec![None; rules.len()];
    for (field_i, possible_rules) in rule_possibilities {
        for rule_i in possible_rules {
            if decided_rules[rule_i] == None {
                decided_rules[rule_i] = Some(field_i);
            }
        }
    }

    // The first 6 fields are "departure *"
    let product: usize = (0..6).map(|i| my_numbers[decided_rules[i].unwrap()]).product();
    println!("Part 2: {}", product);
}
