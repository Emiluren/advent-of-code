use std::fs;
use std::mem;

fn main() {
    let contents = fs::read_to_string("11input").unwrap();
    let mut grid: Vec<Vec<char>> = contents.lines().map(|l| l.chars().collect()).collect();
    let mut grid2 = grid.clone();

    let height = grid.len() as isize;
    let width = grid[0].len() as isize;

    loop {
        let mut changed = false;

        for y in 0..height {
            for x in 0..width {
                let mut adjacent_count = 0;
                for i in -1..=1 {
                    for j in -1..=1 {
                        let yi = y + i;
                        let xj = x + j;
                        if !(i == 0 && j == 0) && yi >= 0 && yi < height && xj >= 0 && xj < width {
                            if grid[yi as usize][xj as usize] == '#' {
                                adjacent_count += 1;
                            }
                        }
                    }
                }

                let seat = grid[y as usize][x as usize];
                if seat == 'L' && adjacent_count == 0 {
                    grid2[y as usize][x as usize] = '#';
                    changed = true;
                } else if seat == '#' && adjacent_count >= 4 {
                    grid2[y as usize][x as usize] = 'L';
                    changed = true;
                } else {
                    grid2[y as usize][x as usize] = seat;
                }
            }
        }
        let grid_string: String = grid2.iter().map(
            |r| r.iter().chain(std::iter::once(&'\n')
        ).collect::<String>()).collect();
        println!("{}", grid_string);

        mem::swap(&mut grid, &mut grid2);

        if !changed {
            break;
        }
    }

    let occupied_count = grid.iter().flat_map(|r| r.iter().filter(|c| **c == '#')).count();
    println!("Part 1: {}", occupied_count);
}

