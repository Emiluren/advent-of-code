use std::fs::File;
use std::io::prelude::*;

enum Turn { Left, Straight, Right }

struct Cart {
    pos: (usize, usize),
    dir: char,
    next_turn: Turn,
}

fn main() -> std::io::Result<()> {
    let mut file = File::open("13_input")?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let input_lines: Vec<Vec<_>> = input.lines().map(|l| l.chars().collect()).collect();

    // Find carts
    let mut carts = Vec::new();
    let mut y = 0;
    let mut x = 0;
    for line in input.lines() {
        for c in line.chars() {
            if c == 'v' || c == '^' || c == '<' || c == '>' {
                carts.push(Cart { pos: (x, y), dir: c, next_turn: Turn::Left });
            }
            x += 1;
        }
        x = 0;
        y += 1;
    }

    // Simulate until crash
    'outer: loop {
        carts.sort_unstable_by(|c1, c2| {
            let (x1, y1) = c1.pos;
            let (x2, y2) = c2.pos;

            match y1.cmp(&y2) {
                std::cmp::Ordering::Equal => {
                    x1.cmp(&x2)
                },
                o => o
            }
        });
        let mut to_remove = Vec::new();

        for i in 0..carts.len() {
            if to_remove.contains(&i) {
                continue;
            }

            let (x, y) = carts[i].pos;

            let tile = input_lines[y as usize][x as usize];
            if tile == '+' {
                match carts[i].next_turn {
                    Turn::Left => {
                        carts[i].next_turn = Turn::Straight;
                        carts[i].dir = match carts[i].dir {
                            'v' => '>',
                            '<' => 'v',
                            '>' => '^',
                            '^' => '<',
                            _ => panic!("unknown dir {}", carts[i].dir)
                        };
                    },
                    Turn::Straight => carts[i].next_turn = Turn::Right,
                    Turn::Right => {
                        carts[i].next_turn = Turn::Left;
                        carts[i].dir = match carts[i].dir {
                            'v' => '<',
                            '<' => '^',
                            '>' => 'v',
                            '^' => '>',                            
                            _ => panic!("unknown dir {}", carts[i].dir)
                        };
                    },
                }
            } else if tile == '/' {
                carts[i].dir = match carts[i].dir {
                    'v' => '<',
                    '>' => '^',
                    '<' => 'v',
                    '^' => '>',
                    _ => panic!("cart has illegal direction {} at / turn", carts[i].dir),
                }
            } else if tile == '\\' {
                carts[i].dir = match carts[i].dir {
                    '<' => '^',
                    'v' => '>',
                    '^' => '<',
                    '>' => 'v',
                    _ => panic!("cart has illegal direction {} at \\ turn", carts[i].dir),
                }
            }

            carts[i].pos = match carts[i].dir {
                'v' => (x, y+1),
                '<' => (x-1, y),
                '>' => (x+1, y),
                '^' => (x, y-1),
                _ => panic!("unknown dir {}", carts[i].dir)
            };

            for j in 0..carts.len() {
                let (x2, y2) = carts[j].pos;

                if i != j && x == x2 && y == y2 && !to_remove.contains(&j) {
                    println!("crash at {},{}", x, y);
                    //break 'outer;
                    to_remove.push(i);
                    to_remove.push(j);
                }
            }
        }

        to_remove.sort_unstable_by(|a, b| b.cmp(a));
        for rem_index in to_remove {
            carts.remove(rem_index);
        }
        if carts.len() == 1 {
            println!("Last cart at {:?}", carts[0].pos);
            break 'outer;
        }
        //print_state(&input_lines, &carts);
    }

    Ok(())
}

fn print_state(input_lines: &Vec<Vec<char>>, carts: &Vec<Cart>) {
    for y in 0..input_lines.len() {
        'line: for x in 0..input_lines[y].len() {
            for i in 0..carts.len() {
                let (cx, cy) = carts[i].pos;
                if cx == x && cy == y {
                    print!("{}", carts[i].dir);
                    continue 'line;
                }
            }
            print!("{}", match input_lines[y][x] {
                        'v' => '|',
                        '^' => '|',
                        '<' => '-',
                        '>' => '-',
                        c => c,
                    })
        }
        println!();
    }

    std::thread::sleep(std::time::Duration::from_millis(100));
}
