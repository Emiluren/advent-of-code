use std::collections::HashSet;
use std::collections::HashMap;

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
                let point = (row as isize, col as isize);
                input_set.insert(point);
            }
        }
    }

    let mut max_asteroids = 0;
    let mut best_pos = (0, 0);
    for (row, col) in &input_set {
        let mut num_asteroids = 0;
        for (r2, c2) in &input_set {
            if (row, col) == (r2, c2) {
                continue;
            }

            let (dx, dy) = dx_dy_for((*row, *col), (*r2, *c2));

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
        if num_asteroids > max_asteroids {
            max_asteroids = num_asteroids;
            best_pos = (*row, *col);
        }
    }

    println!("Part 1: {}", max_asteroids);

    let mut grouped_by_angle = HashMap::new();
    for p in &input_set {
        if *p == best_pos {
            continue;
        }
        let dx_dy = dx_dy_for(best_pos, *p);
        grouped_by_angle.entry(dx_dy).or_insert(Vec::new());
        grouped_by_angle.get_mut(&dx_dy).map(|v| v.push(*p));
    }

    for (_, v) in grouped_by_angle.iter_mut() {
        v.sort_by(|a, b| (b.0.abs() + b.1.abs()).cmp(&(a.0.abs() + a.1.abs())));
    }

    let offset_angle = |(dx, dy): (isize, isize)| {
        let a = (dx as f32).atan2(-dy as f32);
        if a >= 0. {
            a
        } else {
            2. * std::f32::consts::PI + a
        }
    };
    let mut angle_vec: Vec<_> = grouped_by_angle.keys().cloned().collect();
    angle_vec.sort_by(|a, b| offset_angle(*a).partial_cmp(&offset_angle(*b)).unwrap());

    let mut last_vaporized = None;
    let mut vapor_count = 0;

    'vaporloop: while vapor_count < 200 {
        for angle in &angle_vec {
            if let Some(v) = grouped_by_angle.get_mut(angle) {
                vapor_count += 1;
                last_vaporized = v.pop();

                if vapor_count == 200 {
                    break 'vaporloop;
                }
            }
        }
    }

    let (y, x) = last_vaporized.unwrap();
    println!("Part 2: {}", x * 100 + y);
}

fn dx_dy_for((r1, c1): (isize, isize), (r2, c2): (isize, isize)) -> (isize, isize) {
    let dy = r2 - r1;
    let dx = c2 - c1;
    let gcd = gcd(dx.abs() as usize, dy.abs() as usize) as isize;

    (dx / gcd, dy / gcd)
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
