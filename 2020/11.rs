use std::fs;
use std::mem;

fn main() {
    let contents = fs::read_to_string("11input").unwrap();
    let original_grid: Vec<Vec<char>> = contents.lines().map(|l| l.chars().collect()).collect();
    let mut grid = original_grid.clone();
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

        mem::swap(&mut grid, &mut grid2);
        //print_grid(&grid);

        if !changed {
            break;
        }
    }

    print_grid(&grid);

    let occupied_count = grid.iter().flat_map(|r| r.iter().filter(|c| **c == '#')).count();
    println!("Part 1: {}", occupied_count);

    let mut grid = original_grid.clone();
    loop {
        let mut changed = false;

        for y in 0..height {
            for x in 0..width {
                let mut visible_count = 0;
                for i in -1..=1 {
                    for j in -1..=1 {
                        if i == 0 && j == 0 {
                            continue;
                        }
                        let mut yi = y + i;
                        let mut xj = x + j;
                        while yi >= 0 && yi < height && xj >= 0 && xj < width {
                            let seat = grid[yi as usize][xj as usize];
                            if seat == '#' {
                                visible_count += 1;
                                break;
                            } else if seat == 'L' {
                                break;
                            }
                            yi += i;
                            xj += j;
                        }
                    }
                }

                let seat = grid[y as usize][x as usize];
                if seat == 'L' && visible_count == 0 {
                    grid2[y as usize][x as usize] = '#';
                    changed = true;
                } else if seat == '#' && visible_count >= 5 {
                    grid2[y as usize][x as usize] = 'L';
                    changed = true;
                } else {
                    grid2[y as usize][x as usize] = seat;
                }
            }
        }

        mem::swap(&mut grid, &mut grid2);
        //print_grid(&grid);

        if !changed {
            break;
        }
    }

    print_grid(&grid);
    let occupied_count = grid.iter().flat_map(|r| r.iter().filter(|c| **c == '#')).count();
    println!("Part 2: {}", occupied_count);
}

fn print_grid(grid: &Vec<Vec<char>>) {
    for row in grid {
        for c in row {
            print!("{}", c);
        }
        println!();
    }
    println!();
}
