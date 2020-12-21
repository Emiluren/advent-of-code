#[derive(Debug)]
struct Tile {
    id: u16,
    content: Vec<Vec<char>>,
    sides: [Vec<char>; 4],
    neighbours: [Option<TileConnection>; 4],
}

#[derive(Copy, Clone, Debug)]
struct TileConnection {
    other: usize,
    other_side: usize,
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
                        self.content[col][row] = old_content[row][col];
                    },
                    2 => {
                        self.content[size-row][size-col] = old_content[row][col];
                    },
                    3 => {
                        self.content[row][col] = old_content[col][row];
                    },
                    _ => panic!("Invalid step amount {}", steps),
                }
            }
        }

        self.sides.rotate_right(steps);
        self.neighbours.rotate_right(steps);
    }

    fn flip(&mut self) {
        for row in &mut self.content {
            row.reverse();
        }

        for side in &mut self.sides {
            side.reverse();
        }
        self.sides.swap(1, 3)
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
                            other_side: j,
                            flipped
                        });
                        tiles[t2].neighbours[j] = Some(TileConnection {
                            other: t1,
                            other_side: i,
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

    let right_dir = tiles[corners[0]].neighbours.iter()
        .enumerate()
        .filter_map(|(i, c)| c.map(|_| i))
        .nth(0)
        .unwrap();
    let down_dir = (right_dir + 1) % 4;

    // TODO: figure out left collumn first

    for row in 0..1 { // TODO 0..12
        let mut right_dir = right_dir; // TODO: rotate & flip instead of mutating this
        for col in 1..12 {
            let left_tile = layout[row][col-1];
            let conn = tiles[left_tile].neighbours[right_dir].unwrap();

            layout[row][col] = conn.other;
            right_dir = (conn.other_side + 2) % 4;
        }
    }
    dbg!(&layout[0]);

    for i in 0..10 {
        for t in &layout[0] {
            print!("{} ", tiles[*t].content[i].iter().collect::<String>());
        }
        println!();
    }

    let sea_monster: Vec<Vec<char>> = vec![
        "                  # ".chars().collect(),
        "#    ##    ##    ###".chars().collect(),
        " #  #  #  #  #  #   ".chars().collect(),
    ];
}
