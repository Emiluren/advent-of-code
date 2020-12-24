#[derive(Copy, Clone)]
enum Direction {
    E, SE, SW, W, NW, NE
}
use Direction::*;

fn main() {
    let input = std::fs::read_to_string("24input").unwrap();

    let mut steps_lines = Vec::new();
    for line in input.lines() {
        let mut chars = line.chars();

        let mut steps = Vec::new();
        while let Some(c1) = chars.next() {
            let c2 = if c1 == 's' || c1 == 'n' {
                chars.next()
            } else {
                None
            };

            let dir = match (c1, c2) {
                ('e', None) => E,
                ('s', Some('e')) => SE,
                ('s', Some('w')) => SW,
                ('w', None) => W,
                ('n', Some('w')) => NW,
                ('n', Some('e')) => NE,
                _ => panic!("Unknown step direction {:?}", (c1, c2)),
            };
            steps.push(dir);
        }
        steps_lines.push(steps);
    }

    const WIDTH: usize = 300;
    const HEIGHT: usize = 300;

    type Tiles = Vec<[bool; WIDTH]>;
    let mut tiles: Tiles = vec![[false; WIDTH]; HEIGHT];
    for steps in &steps_lines {
        let mut row = 150;
        let mut col = 150;

        for step in steps {
            let (r, c) = offset_pos((row, col), *step);
            row = r;
            col = c;
        }

        tiles[row][col] = !tiles[row][col];
    }

    let black_count = |tiles: &Tiles| tiles.iter()
        .map(|row| row.iter().filter(|v| **v).count())
        .sum::<usize>();
    println!("Part 1: {}", black_count(&tiles));

    let mut tiles2 = tiles.clone();
    for _ in 0..100 {
        for r in 1..(WIDTH-1) {
            for c in 1..(HEIGHT-1) {
                let mut count = 0;
                for neighbor in &[E, SE, SW, W, NW, NE] {
                    let (r2, c2) = offset_pos((r, c), *neighbor);
                    if tiles[r2][c2] {
                        count += 1;
                    }
                }

                let val = tiles[r][c];
                if val && (count == 0 || count > 2) {
                    tiles2[r][c] = false;
                } else if !val && count == 2 {
                    tiles2[r][c] = true;
                } else {
                    tiles2[r][c] = val;
                }
            }
        }

        std::mem::swap(&mut tiles, &mut tiles2);
    }
    println!("Part 2: {}", black_count(&tiles));
}

fn offset_pos((mut row, mut col): (usize, usize), s: Direction) -> (usize, usize) {
    match s {
        E => col += 1,
        SE => row += 1,
        SW => {
            row += 1;
            col -= 1;
        }
        W => col -= 1,
        NW => row -= 1,
        NE => {
            row -= 1;
            col += 1;
        }
    };

    (row, col)
}
