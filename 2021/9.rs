use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let file = File::open("9input").unwrap();
    let mut reader = io::BufReader::new(file).lines();
    let mut input = Vec::new();

    while let Some(Ok(line)) = reader.next() {
        let mut row = Vec::new();
        for c in line.chars() {
            row.push(c.to_string().parse::<u8>().unwrap());
        }
        input.push(row);
    }

    let (h, w) = (input.len() - 1, input[0].len() - 1);

    let mut risk_sum = 0;
    let mut low_points = Vec::new();
    for (r, row) in input.iter().enumerate() {
        for (c, n) in row.iter().enumerate() {
            if neighbors(r, c, h, w).iter().copied().all(|(r2, c2)| *n < input[r2][c2]) {
                low_points.push((r, c));
                risk_sum += *n as u32 + 1;
            }
        }
    }
    println!("Part 1: {}", risk_sum);

    let mut basins: Vec<HashSet<(usize, usize)>> = Vec::new();
    for (r, c) in low_points {
        for b in &basins {
            if b.contains(&(r, c)) {
                continue;
            }
        }

        let mut frontier = Vec::new();
        let mut basin = HashSet::new();
        frontier.push((r, c));
        basin.insert((r, c));
        while let Some((r, c)) = frontier.pop() {
            for (r2, c2) in neighbors(r, c, h, w) {
                if input[r2][c2] < 9 && !basin.contains(&(r2, c2)) {
                    frontier.push((r2, c2));
                    basin.insert((r2, c2));
                }
            }
        }
        basins.push(basin);
    }
    basins.sort_by_key(|b| b.len());
    let l = basins.len();
    println!("Part 2: {}", basins[l-1].len() * basins[l-2].len() * basins[l-3].len());
}

fn neighbors(r: usize, c: usize, height: usize, width: usize) -> Vec<(usize, usize)> {
    let mut res = Vec::new();
    if r > 0 {
        res.push((r-1, c));
    }
    if c > 0 {
        res.push((r, c-1));
    }
    if r < height {
        res.push((r+1, c));
    }
    if c < width {
        res.push((r, c+1));
    }
    res
}
