use std::fs;

fn main() {
    let contents = fs::read_to_string("13input").unwrap();
    let earliest_possible: usize = contents.lines().nth(0).unwrap().parse().unwrap();
    let second_line = contents.lines().nth(1).unwrap();
    let buses: Vec<usize> = second_line.split(',').filter_map(|s| s.parse().ok()).collect();

    let earliest_after = buses.iter().map(|bus| {
        let time = (earliest_possible / bus) * bus;
        if time == earliest_possible {
            time
        } else {
            time + bus
        }
    });

    let (i, t) = earliest_after.enumerate().min_by_key(|(_, t)| *t).unwrap();
    println!("Part 1: {}", (t - earliest_possible) * buses[i]);

    let mut rules = second_line.split(',').enumerate().filter_map(|(offset, bus)| {
        if let Ok(b) = bus.parse() {
            Some((offset, b))
        } else {
            None
        }
    });

    let (mut time, mut bus) = rules.next().unwrap();
    for (offset, b) in rules {
        loop {
            time += bus;
            if (time + offset) % b == 0 {
                break;
            }
        }

        bus = lcm(bus, b);
    }

    println!("Part 2: {}", time);
}

fn lcm(a: usize, b: usize) -> usize {
    a / gcd(a, b) * b
}

fn gcd(mut a: usize, mut b: usize) -> usize {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}
