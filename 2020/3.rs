use std::fs;

fn main() {
    let contents = fs::read_to_string("3input").unwrap();
    let map: Vec<Vec<bool>> = contents.lines().map( |line| {
        line.chars().map(|c| c == '#').collect()
    }).collect();

    println!("Part 1: {}", trees_encountered(3, 1, &map));

    let slopes = [
        (1, 1),
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2),
    ];
    println!("Part 2: {}", slopes.iter().cloned().map(
        |(xs, ys)| trees_encountered(xs, ys, &map)
    ).product::<usize>());
}

fn trees_encountered(x_step: usize, y_step: usize, map: &Vec<Vec<bool>>) -> usize {
    let height = map.len();
    let width = map[0].len();

    let mut x = 0;
    let mut y = 0;

    let mut trees = 0;
    while y < height {
        if map[y][x] {
            trees += 1;
        }
        x = (x + x_step) % width;
        y += y_step;
    }

    trees
}
