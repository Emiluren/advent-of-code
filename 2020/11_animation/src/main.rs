use std::borrow::Cow;
use std::fs;
use std::mem;

const FLOOR: u8 = 0;
const EMPTY: u8 = 1;
const OCCUPIED: u8 = 2;

fn main() {
    let contents = fs::read_to_string("../11input").unwrap();
    let original_grid: Vec<u8> = contents.chars().filter_map(|c| match c {
        '#' => Some(OCCUPIED),
        'L' => Some(EMPTY),
        '.' => Some(FLOOR),
        _ => None,
    }).collect();
    let mut grid = original_grid.clone();
    let mut grid2 = grid.clone();

    let height = contents.lines().count();
    let width = contents.lines().nth(0).unwrap().len();

    let color_map = &[
        0, 0, 0,
        0x80, 0x80, 0x80,
        0xFF, 0xFF, 0xFF
    ];

    let mut image = fs::File::create("part1.gif").unwrap();
    let mut encoder = gif::Encoder::new(&mut image, width as u16, height as u16, color_map).unwrap();
    let mut frame_data = original_grid.clone();
    loop {
        let mut changed = false;

        for y in 0..height {
            for x in 0..width {
                let mut adjacent_count = 0;
                for i in -1..=1 {
                    for j in -1..=1 {
                        let yi = y as isize + i;
                        let xj = x as isize + j;
                        if !(i == 0 && j == 0) && yi >= 0 && yi < height as isize && xj >= 0 && xj < width as isize {
                            if grid[yi as usize * width + xj as usize] == OCCUPIED {
                                adjacent_count += 1;
                            }
                        }
                    }
                }

                let index = y*width + x;
                let seat = grid[index];
                if seat == EMPTY && adjacent_count == 0 {
                    grid2[index] = OCCUPIED;
                    changed = true;
                } else if seat == OCCUPIED && adjacent_count >= 4 {
                    grid2[index] = EMPTY;
                    changed = true;
                } else {
                    grid2[index] = seat;
                    // Only output cells that have stabilized to prevent flashing
                    frame_data[index] = seat;
                }
            }
        }

        mem::swap(&mut grid, &mut grid2);

        let mut frame = gif::Frame::default();
        frame.width = width as u16;
        frame.height = height as u16;
        frame.buffer = Cow::Borrowed(&*frame_data);
        frame.delay = 30;
        encoder.write_frame(&frame).unwrap();

        if !changed {
            break;
        }
    }

    print_grid(&grid, width);
    let occupied_count1 = grid.iter().filter(|c| **c == OCCUPIED).count();
    println!("Part 1: {}", occupied_count1);

    let mut image = fs::File::create("part2.gif").unwrap();
    let mut encoder = gif::Encoder::new(&mut image, width as u16, height as u16, color_map).unwrap();
    let mut frame_data = original_grid.clone();

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
                        let mut yi = y as isize + i;
                        let mut xj = x as isize + j;
                        while yi >= 0 && yi < height as isize && xj >= 0 && xj < width as isize {
                            let seat = grid[yi as usize * width + xj as usize];
                            if seat == OCCUPIED {
                                visible_count += 1;
                                break;
                            } else if seat == EMPTY {
                                break;
                            }
                            yi += i;
                            xj += j;
                        }
                    }
                }

                let index = y*width + x;
                let seat = grid[index];
                if seat == EMPTY && visible_count == 0 {
                    grid2[index] = OCCUPIED;
                    changed = true;
                } else if seat == OCCUPIED && visible_count >= 5 {
                    grid2[index] = EMPTY;
                    changed = true;
                } else {
                    grid2[index] = seat;
                    // Only output cells that have stabilized to prevent flashing
                    frame_data[index] = seat;
                }
            }
        }

        mem::swap(&mut grid, &mut grid2);

        let mut frame = gif::Frame::default();
        frame.width = width as u16;
        frame.height = height as u16;
        frame.buffer = Cow::Borrowed(&*frame_data);
        frame.delay = 30;
        encoder.write_frame(&frame).unwrap();

        if !changed {
            break;
        }
    }

    print_grid(&grid, width);
    let occupied_count2 = grid.iter().filter(|c| **c == OCCUPIED).count();
    println!("Part 2: {}", occupied_count2);
}

fn print_grid(grid: &[u8], width: usize) {
    let height = grid.len() / width;
    for y in 0..height {
        for x in 0..height {
            let c = match grid[y*width + x] {
                0 => '.',
                1 => 'L',
                2 => '#',
                _ => panic!("Unknown tile type {}", grid[y*width + x]),
            };
            print!("{}", c);
        }
        println!();
    }
    println!();
}
