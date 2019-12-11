use std::collections::HashSet;

fn main() {
    let input = [
        [1,1,1,0,0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,0],
        [0,1,1,1,1,0,1,1,1,1,1,0,0,1,1,1,1,0,1,0,1],
        [0,1,1,1,0,1,0,1,0,1,1,1,1,1,0,1,1,0,0,1,1],
        [1,1,0,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1],
        [1,1,1,0,0,0,1,0,1,1,1,1,0,1,0,1,0,1,1,1,1],
        [1,0,1,1,0,0,1,1,1,0,1,1,1,1,1,1,1,1,0,0,0],
        [1,0,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,0],
        [0,1,0,0,1,0,1,0,0,1,1,1,0,0,0,1,1,1,1,0,1],
        [1,1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,1,1],
        [1,0,1,0,0,0,0,0,0,1,0,0,0,0,1,0,1,0,1,0,0],
        [1,1,1,1,1,1,0,1,1,1,0,1,0,1,0,1,1,0,0,0,1],
        [1,1,1,1,0,1,0,0,0,1,0,1,1,1,1,1,1,1,0,1,0],
        [0,1,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,1],
        [1,1,0,1,1,0,1,1,0,1,1,1,1,1,0,1,1,0,1,0,1],
        [1,1,1,0,1,1,1,1,1,1,1,0,0,1,1,0,1,0,0,0,0],
        [1,1,1,0,1,1,0,1,1,0,0,1,1,0,1,1,1,1,1,0,1],
        [1,1,0,1,1,1,1,1,1,1,1,0,1,0,1,0,1,1,1,1,1],
        [0,1,1,0,0,0,0,1,1,0,0,1,1,1,0,1,0,0,0,1,0],
        [1,0,0,1,0,1,1,1,1,0,1,1,1,1,1,1,0,0,1,1,1],
        [0,0,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1],
        [0,0,1,1,0,0,0,1,1,1,0,0,1,1,1,1,1,1,1,1,1],
    ];

    let width = input[0].len();
    let height = input.len();

    let mut input_set = HashSet::new();
    for row in 0..height {
        for col in 0..width {
            if input[row][col] == 1 {
                input_set.insert((row as isize, col as isize));
            }
        }
    }

    let mut max_asteroids = 0;
    for (row, col) in &input_set {
        let mut num_asteroids = 0;
        for (r2, c2) in &input_set {
            if (row, col) == (r2, c2) {
                continue;
            }
            let dy = r2 - row;
            let dx = c2 - col;
            let gcd = gcd(dx.abs() as usize, dy.abs() as usize) as isize;

            let (dx, dy) = (dx / gcd, dy / gcd);

            let mut check_row = row + dy;
            let mut check_col = col + dx;

            while !input_set.contains(&(check_row, check_col)) {
                check_row += dy;
                check_col += dx;
            }

            if (check_row, check_col) == (*r2, *c2) {
                num_asteroids += 1;
            }
        }
        max_asteroids = max_asteroids.max(num_asteroids);
    }

    println!("Part 1: {}", max_asteroids);
}

fn gcd(x: usize, y: usize) -> usize {
    let mut x = x;
    let mut y = y;
    while y != 0 {
        let t = y;
        y = x % y;
        x = t;
    }
    x
}
