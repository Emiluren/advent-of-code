use std::fs;

fn main() {
    let contents = fs::read_to_string("13input").unwrap();
    let earliest_possible: usize = contents.lines().nth(0).unwrap().parse().unwrap();

    let buses: Vec<usize> = contents
        .lines()
        .nth(1)
        .unwrap()
        .split(',')
        .filter_map(|s| s.parse().ok())
        .collect();

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
}
