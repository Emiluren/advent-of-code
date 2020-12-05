use std::fs;

fn main() {
    let contents = fs::read_to_string("5input").unwrap();

    let mut max_id = 0;
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
        if id > max_id {
            max_id = id;
        }
    }

    println!("Part 1: {}", max_id);
}
