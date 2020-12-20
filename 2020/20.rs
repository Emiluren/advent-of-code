struct Tile {
    id: usize,
    content: Vec<Vec<char>>,
    sides: [String; 4],
    neighbours: [Option<usize>; 4],
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
            content[9].iter().cloned().collect(),
            content.iter().map(|row| row[0]).collect(),
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
                if tiles[t1].neighbours[i] != None {
                    continue;
                }

                for j in 0..4 {
                    if tiles[t2].neighbours[j] != None {
                        continue;
                    }

                    let rev2: String = tiles[t2].sides[j].chars().rev().collect();
                    if tiles[t1].sides[i] == tiles[t2].sides[j] || tiles[t1].sides[i] == rev2 {
                        tiles[t1].neighbours[i] = Some(tiles[t2].id);
                        tiles[t2].neighbours[j] = Some(tiles[t1].id);
                    }
                }
            }
        }
    }

    let corner_product: usize = tiles.iter().filter_map(|t| {
        let neighbour_count = t.neighbours.iter().filter(|n| n.is_some()).count();
        if neighbour_count == 2 {
            Some(t.id)
        } else {
            None
        }
    }).product();
    println!("Part 1: {}", corner_product);
}
