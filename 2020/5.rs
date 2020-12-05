use std::fs;

fn main() {
    let contents = fs::read_to_string("5input").unwrap();

    let mut ids = Vec::new();
    for line in contents.lines() {
        let chars: Vec<_> = line.chars().collect();

        let mut row = 0;
        for i in 0..7 {
            match chars[i] {
                'F' => {}
                'B' => {
                    row += 2_u32.pow(6 - i as u32);
                }
                _ => {
                    panic!("Expected F or B, got {}", chars[i]);
                }
            }
        }

        let mut col = 0;
        for i in 7..10 {
            match chars[i] {
                'L' => {}
                'R' => {
                    col += 2_u32.pow(9 - i as u32);
                }
                _ => {
                    panic!("Expected L or R, got {}", chars[i]);
                }
            }
        }

        let id = row * 8 + col;
        ids.push(id);
    }
    ids.sort();

    println!("Part 1: {}", ids[ids.len() - 1]);

    let mut last_id = ids[0] - 1;
    for id in ids {
        if id != last_id + 1 {
            println!("Part 2: {}", last_id + 1);
            break;
        }
        last_id = id;
    }
}
