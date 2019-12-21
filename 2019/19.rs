use std::collections::HashMap;

enum InputState { X, Y }

fn main() {
    let original_input = vec![
        109,424,203,1,21101,11,0,0,1105,1,282,21102,1,18,0,1105,1,259,2102,1,1,221,203,1,21102,1,31,0,1105,1,282,21102,38,1,0,1105,1,259,20101,0,23,2,22102,1,1,3,21101,1,0,1,21101,57,0,0,1105,1,303,2101,0,1,222,21002,221,1,3,21002,221,1,2,21102,1,259,1,21101,0,80,0,1105,1,225,21102,83,1,2,21102,1,91,0,1106,0,303,2101,0,1,223,20102,1,222,4,21101,0,259,3,21101,0,225,2,21101,225,0,1,21101,118,0,0,1106,0,225,20101,0,222,3,21101,34,0,2,21101,133,0,0,1105,1,303,21202,1,-1,1,22001,223,1,1,21102,1,148,0,1106,0,259,1201,1,0,223,20102,1,221,4,20101,0,222,3,21101,12,0,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21101,195,0,0,105,1,108,20207,1,223,2,20101,0,23,1,21102,1,-1,3,21102,214,1,0,1105,1,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,1202,-4,1,249,22101,0,-3,1,22101,0,-2,2,21201,-1,0,3,21101,0,250,0,1105,1,225,21201,1,0,-4,109,-5,2106,0,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2106,0,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,22101,0,-2,-2,109,-3,2106,0,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,21201,-2,0,3,21101,343,0,0,1105,1,303,1105,1,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,21201,-4,0,1,21101,384,0,0,1105,1,303,1106,0,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,21202,1,1,-4,109,-5,2106,0,0
    ];

    let input_map = original_input.iter().enumerate()
        .map(|(i, v)| (i as i64, *v)).collect();

    let mut count = 0;
    for y in 0..50 {
        for x in 0..50 {
            if run_program(&input_map, [x, y].iter().cloned()) {
                count += 1;
            }
        }
    }
    println!("Part 1: {}", count);
}

fn print_memory(memory: &HashMap<i64, i64>) {
    let max_key = *memory.keys().max().unwrap();
    for i in 0 ..= max_key {
        print!("{} ", memory.get(&i).cloned().unwrap_or(0));
    }
    println!();
}

fn run_program(initial_memory: &HashMap<i64, i64>, mut input: impl Iterator<Item=i64>) -> bool {
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
                let pos = memory[&(i+1)];
                set_data!(mode1, pos, input.next().unwrap());
                i += 2;
            }
            4 => {
                return get_data!(mode1, memory[&(i+1)]) == 1;
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

    panic!("Program terminated without outputting result")
}
