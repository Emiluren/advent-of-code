const GRID_SIZE: usize = 50;

fn main() {
    let input: Vec<Vec<_>> = std::fs::read_to_string("17input")
        .unwrap()
        .lines()
        .map(|l| l.chars().collect())
        .collect();

    let mut grid = [[['.'; GRID_SIZE]; GRID_SIZE]; GRID_SIZE];
    for x in 0..input[0].len() {
        for y in 0..input.len() {
            grid[x+20][y+20][25] = input[y][x];
        }
    }
    let mut grid2 = grid.clone();

    for _ in 0..6 {
        for x in 0..GRID_SIZE {
            for y in 0..GRID_SIZE {
                for z in 0..GRID_SIZE {
                    let count = count_neighbours((x, y, z), &grid);

                    let cell = grid[x][y][z];
                    if cell == '.' && count == 3 {
                        grid2[x][y][z] = '#';
                    } else if cell == '#' && (count < 2 || count > 3) {
                        grid2[x][y][z] = '.';
                    } else {
                        grid2[x][y][z] = cell;
                    }
                }
            }
        }
        std::mem::swap(&mut grid, &mut grid2);
    }

    let alive_count = grid.iter().flat_map(
        |p| p.iter().flat_map(
            |l| l.iter().filter(|c| **c == '#')
        )
    ).count();
    println!("Part 1: {}", alive_count);
}

fn count_neighbours((x, y, z): (usize, usize, usize), grid: &[[[char; GRID_SIZE]; GRID_SIZE]]) -> u8 {
    let mut count = 0;
    for i in -1..=1 {
        for j in -1..=1 {
            for k in -1..=1 {
                let xi = x as isize + i;
                let yj = y as isize + j;
                let zk = z as isize + k;
                let in_bounds = |v: &isize| *v >= 0 && *v < GRID_SIZE as isize;
                if !(i == 0 && j == 0 && k == 0) && [xi, yj, zk].iter().all(in_bounds) {
                    if grid[xi as usize][yj as usize][zk as usize] == '#' {
                        count += 1;
                    }
                }
            }
        }
    }

    count
}
