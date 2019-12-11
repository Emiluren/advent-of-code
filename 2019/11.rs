use std::collections::HashMap;

enum OutputState {
    Paint, Turn
}

enum Dir {
    Left, Up, Right, Down
}
use Dir::*;

fn main() {
    let original_input = vec![
        3,8,1005,8,284,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,28,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,50,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,72,1006,0,24,1,1106,12,10,1006,0,96,1,1008,15,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,108,1006,0,54,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,134,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,155,1006,0,60,1006,0,64,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,183,1006,0,6,1006,0,62,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,211,1,108,0,10,2,1002,15,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,242,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,263,101,1,9,9,1007,9,1010,10,1005,10,15,99,109,606,104,0,104,1,21101,0,666526126996,1,21101,301,0,0,1105,1,405,21101,846138811028,0,1,21101,312,0,0,1106,0,405,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,248129978391,1,21101,359,0,0,1105,1,405,21101,97751403560,0,1,21102,1,370,0,1106,0,405,3,10,104,0,104,0,3,10,104,0,104,0,21101,988753585000,0,1,21101,393,0,0,1105,1,405,21102,867961709324,1,1,21102,404,1,0,1106,0,405,99,109,2,22102,1,-1,1,21102,40,1,2,21101,436,0,3,21102,1,426,0,1105,1,469,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,431,432,447,4,0,1001,431,1,431,108,4,431,10,1006,10,463,1102,0,1,431,109,-2,2106,0,0,0,109,4,1202,-1,1,468,1207,-3,0,10,1006,10,486,21102,1,0,-3,22101,0,-3,1,21202,-2,1,2,21102,1,1,3,21101,505,0,0,1106,0,510,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,533,2207,-4,-2,10,1006,10,533,22101,0,-4,-4,1105,1,601,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,1,552,0,1105,1,510,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,571,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,593,21202,-1,1,1,21102,1,593,0,106,0,468,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0
    ];

    let input_map = original_input.iter().enumerate()
        .map(|(i, v)| (i as i64, *v)).collect();

    let canvas = run_program(&input_map);
    println!("Part 1: {}", canvas.keys().count());

    let mut min_x = 0;
    let mut min_y = 0;
    let mut max_x = 0;
    let mut max_y = 0;

    for (x, y) in canvas.keys() {
        min_x = min_x.min(*x);
        min_y = min_y.min(*y);
        max_x = max_x.max(*x);
        max_y = max_y.max(*y);
    }

    println!("Part 2:");
    for y in min_y ..= max_y {
        for x in min_x ..= max_y {
            if canvas.get(&(x, y)) == Some(&true) {
                print!("X");
            } else {
                print!(" ");
            }
        }
        println!();
    }
}

fn print_memory(memory: &HashMap<i64, i64>) {
    let max_key = *memory.keys().max().unwrap();
    for i in 0 ..= max_key {
        print!("{} ", memory.get(&i).cloned().unwrap_or(0));
    }
    println!();
}

fn run_program(initial_memory: &HashMap<i64, i64>) -> HashMap<(i32, i32), bool> {
    let mut memory = initial_memory.clone();
    let mut relative_base = 0;
    let mut i = 0;

    macro_rules! get_data {
        ($mode:expr, $parameter:expr) => {
            match $mode {
                0 => memory.get(&$parameter).cloned().unwrap_or(0),
                1 => $parameter,
                2 => memory.get(&(relative_base + $parameter)).cloned().unwrap_or(0),
                _ => panic!("Invalid addressing mode {}", $mode)
            }
        }
    }

    macro_rules! set_data {
        ($mode:expr, $parameter:expr, $value:expr) => {
            match $mode {
                0 => memory.insert($parameter, $value),
                1 => panic!("Cannot set in immediate mode"),
                2 => memory.insert(relative_base + $parameter, $value),
                _ => panic!("Invalid addressing mode {}", $mode)
            };
        }
    }

    let mut pos = (0, 0);
    let mut next_output_state = OutputState::Paint;
    let mut canvas = HashMap::new();
    let mut dir = Dir::Up;

    loop {
        let op = memory[&i] % 100;
        let mode1 = (memory[&i] / 100) % 10;
        let mode2 = (memory[&i] / 1000) % 10;
        let mode3 = memory[&i] / 10_000;

        let get2 = || {
            (memory[&(i+1)], memory[&(i+2)])
        };

        let get3 = || {
            (memory[&(i+1)], memory[&(i+2)], memory[&(i+3)])
        };

        match op {
            1 => {
                let (in1, in2, out) = get3();
                let res = get_data!(mode1, in1) + get_data!(mode2, in2);
                set_data!(mode3, out, res);
                i += 4;
            }
            2 => {
                let (in1, in2, out) = get3();
                let res = get_data!(mode1, in1) * get_data!(mode2, in2);
                set_data!(mode3, out, res);
                i += 4;
            }
            3 => {
                let input = if *canvas.get(&pos).unwrap_or(&false) {
                    1
                } else {
                    0
                };
                let mem_pos = memory[&(i+1)];
                set_data!(mode1, mem_pos, input);
                i += 2;
            }
            4 => {
                let output = get_data!(mode1, memory[&(i+1)]);
                match next_output_state {
                    OutputState::Paint => {
                        canvas.insert(pos, output == 1);
                        next_output_state = OutputState::Turn;
                    }
                    OutputState::Turn => {
                        // 0 - turn left, 1 - turn right
                        dir = if output == 0 {
                            match dir {
                                Left => Down,
                                Up => Left,
                                Right => Up,
                                Down => Right,
                            }
                        } else {
                            match dir {
                                Left => Up,
                                Up => Right,
                                Right => Down,
                                Down => Left,
                            }
                        };
                        pos = match dir {
                            Left => (pos.0 - 1, pos.1),
                            Up => (pos.0, pos.1 - 1),
                            Right => (pos.0 + 1, pos.1),
                            Down => (pos.0, pos.1 + 1),
                        };
                        next_output_state = OutputState::Paint;
                    }
                };
                i += 2;
            }
            5 => {
                let (in1, in2) = get2();
                if get_data!(mode1, in1) != 0 {
                    i = get_data!(mode2, in2);
                } else {
                    i += 3;
                }
            }
            6 => {
                let (in1, in2) = get2();
                if get_data!(mode1, in1) == 0 {
                    i = get_data!(mode2, in2);
                } else {
                    i += 3;
                }
            }
            7 => {
                let (in1, in2, out) = get3();
                let res = if get_data!(mode1, in1) < get_data!(mode2, in2) {
                    1
                } else {
                    0
                };
                set_data!(mode3, out, res);
                i += 4;
            }
            8 => {
                let (in1, in2, out) = get3();
                let res = if get_data!(mode1, in1) == get_data!(mode2, in2) {
                    1
                } else {
                    0
                };
                set_data!(mode3, out, res);
                i += 4;
            }
            9 => {
                relative_base += get_data!(mode1, memory[&(i+1)]);
                i += 2;
            }
            99 => {
                break;
            }
            _ => {
                panic!("Unknown op code {}", op);
            }
        }
    }

    return canvas;
}
