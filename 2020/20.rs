#[derive(Debug, Clone)]
struct Tile {
    id: u16,
    content: Vec<Vec<char>>,
    sides: [Vec<char>; 4],
    neighbours: [Option<TileConnection>; 4],
}

#[derive(Copy, Clone, Debug)]
struct TileConnection {
    other: usize,
    flipped: bool,
}

impl Tile {
    fn rotate_clockwise(&mut self, steps: usize) {
        let size = self.content.len();
        let old_content = self.content.clone();
        for row in 0..size {
            for col in 0..size {
                match steps {
                    0 => {}
                    1 => {
                        self.content[row][col] = old_content[size-col-1][row];
                    },
                    2 => {
                        self.content[size-row-1][size-col-1] = old_content[row][col];
                    },
                    3 => {
                        self.content[size-col-1][row] = old_content[row][col];
                    },
                    _ => panic!("Invalid step amount {}", steps),
                }
            }
        }

        self.sides.rotate_right(steps);
        self.neighbours.rotate_right(steps);
    }

    fn flip(&mut self) {
        self.content.reverse();
        self.sides.reverse();
        self.sides.swap(0, 2);
        self.neighbours.swap(0, 2);
    }
}

fn main() {
    let input = std::fs::read_to_string("20input").unwrap();
    let mut lines = input.lines();

    let mut tiles = Vec::new();
    while let Some(id_line) = lines.next() {
        let id_string: String = id_line.chars().skip(5).take_while(|c| *c != ':').collect();
        let id = id_string.parse().unwrap();

        let mut content: Vec<Vec<char>> = Vec::new();
        for _ in 0..10 {
            content.push(lines.next().unwrap().chars().collect());
        }
        let sides = [
            content[0].iter().cloned().collect(),
            content.iter().map(|row| row[9]).collect(),
            content[9].iter().cloned().rev().collect(),
            content.iter().map(|row| row[0]).rev().collect(),
        ];
        tiles.push(Tile {
            id,
            content,
            sides,
            neighbours: [None; 4]
        });
        lines.next().unwrap();
    }

    for t1 in 0..tiles.len() {
        for t2 in (t1+1)..tiles.len() {
            for i in 0..4 {
                if tiles[t1].neighbours[i].is_some() {
                    continue;
                }

                for j in 0..4 {
                    let mut found_neighbour = false;
                    let mut flipped = false;

                    if tiles[t2].neighbours[j].is_some() {
                        continue;
                    }

                    if tiles[t1].sides[i] == tiles[t2].sides[j] {
                        found_neighbour = true;
                        flipped = true; // We should expect the other side to be reversed
                    } else {
                        let rev2: Vec<char> = tiles[t2].sides[j].iter().copied().rev().collect();
                        if tiles[t1].sides[i] == rev2 {
                            found_neighbour = true;
                        }
                    }

                    if found_neighbour {
                        tiles[t1].neighbours[i] = Some(TileConnection {
                            other: t2,
                            flipped
                        });
                        tiles[t2].neighbours[j] = Some(TileConnection {
                            other: t1,
                            flipped
                        });
                    }
                }
            }
        }
    }

    let corners: Vec<_> = (0..tiles.len()).filter(|i| {
        let neighbour_count = tiles[*i].neighbours.iter().filter(|n| n.is_some()).count();
        neighbour_count == 2
    }).collect();

    dbg!(&corners);

    let corner_product: u64 = corners.iter().map(|i| tiles[*i].id as u64).product();
    println!("Part 1: {}", corner_product);

    let mut layout = [[0; 12]; 12];
    layout[0][0] = corners[0];

    let right_dir = 1;
    let down_dir = 2;
    // rotated_right_dir actually happened to be 1 for my data so this code does nothing
    let rotated_right_dir = tiles[corners[0]].neighbours.iter()
        .enumerate()
        .filter_map(|(i, c)| c.map(|_| i))
        .nth(0)
        .unwrap();
    let steps = if rotated_right_dir > right_dir {
        4 - (rotated_right_dir - right_dir)
    } else {
        right_dir - rotated_right_dir
    };
    tiles[corners[0]].rotate_clockwise(steps);

    let mut flipped_above = false;
    for row in 0..12 {
        let mut flipped_left = false;

        // A lot of duplicated code here but at least it works
        if row > 0 {
            let above_tile = layout[row-1][0];
            let above_conn = tiles[above_tile].neighbours[down_dir].unwrap();
            let this_tile = above_conn.other;

            layout[row][0] = this_tile;
            if above_conn.flipped != flipped_above {
                tiles[this_tile].flip();
                flipped_above = true;
                flipped_left = true;
            } else {
                flipped_above = false;
            }

            let rotated_up_dir = tiles[this_tile].neighbours.iter()
                .enumerate()
                .find_map(|(i, conn_opt)| {
                    conn_opt.and_then(|conn| {
                        if conn.other == above_tile {
                            Some(i)
                        } else {
                            None
                        }
                    })
                }).unwrap();
            let rotated_down_dir = (rotated_up_dir + 2) % 4;

            let steps = if rotated_down_dir > down_dir {
                4 - (rotated_down_dir - down_dir)
            } else {
                down_dir - rotated_down_dir
            };

            tiles[this_tile].rotate_clockwise(steps);
        }

        for col in 1..12 {
            let left_tile = layout[row][col-1];
            let left_conn = tiles[left_tile].neighbours[right_dir].unwrap();
            let this_tile = left_conn.other;

            layout[row][col] = this_tile;
            if left_conn.flipped != flipped_left {
                tiles[this_tile].flip();
                flipped_left = true;
            } else {
                flipped_left = false;
            }

            let rotated_left_dir = tiles[this_tile].neighbours.iter()
                .enumerate()
                .find_map(|(i, conn_opt)| {
                    conn_opt.and_then(|conn| {
                        if conn.other == left_tile {
                            Some(i)
                        } else {
                            None
                        }
                    })
                }).unwrap();
            let rotated_right_dir = (rotated_left_dir + 2) % 4;

            let steps = if rotated_right_dir > right_dir {
                4 - (rotated_right_dir - right_dir)
            } else {
                right_dir - rotated_right_dir
            };

            tiles[this_tile].rotate_clockwise(steps);
        }
    }

    for i in 0..12 {
        print_tiles(&layout[i], &tiles);
        println!();
    }

    let sea_monster: Vec<Vec<char>> = vec![
        "                  # ".chars().collect(),
        "#    ##    ##    ###".chars().collect(),
        " #  #  #  #  #  #   ".chars().collect(),
    ];
}

fn print_tiles(row: &[usize], tiles: &[Tile]) {
    for t in row {
        print!("{:^11}", t);
    }
    println!();
    for i in 0..10 {
        for t in row {
            print!("{} ", tiles[*t].content[i].iter().collect::<String>());
        }
        println!();
    }

}
