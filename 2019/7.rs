use std::sync::mpsc::{channel, Sender, Receiver};
use std::thread;

fn main() {
    let original_input: [isize; 499] = [
        3,8,1001,8,10,8,105,1,0,0,21,42,51,60,77,94,175,256,337,418,99999,3,9,1001,9,4,9,102,5,9,9,1001,9,3,9,102,5,9,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,1001,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,4,9,101,5,9,9,4,9,99,3,9,1002,9,5,9,101,3,9,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99
    ];

    let phases = [0, 1, 2, 3, 4];

    let mut max_v = 0;
    for phase_perm in permutations(phases) {
        let (mut sender, mut receiver) = channel();
        let mut threads = Vec::new();

        for i in 0..5 {
            let (new_sender, new_receiver) = channel();
            sender.send(phase_perm[i]).unwrap();

            if i == 0 {
                sender.send(0).unwrap();
            }

            let thread_sender = new_sender.clone();
            threads.push(thread::spawn(move|| {
                run_program(&original_input, receiver, thread_sender);
            }));

            sender = new_sender;
            receiver = new_receiver;
        }

        max_v = max_v.max(receiver.recv().unwrap());
    }
    println!("Part 1: {}", max_v);
}

fn permutations(mut values: [isize; 5]) -> Vec<[isize; 5]> {
    let mut permuts = Vec::new();
    permuts.push(values.clone());
    let mut c = vec![0; values.len()];

    let mut i = 0;
    while i < values.len() {
        if c[i] < i {
            if i % 2 == 0 {
                values.swap(0, i);
            } else {
                values.swap(c[i], i);
            }

            permuts.push(values.clone());
            c[i] += 1;
            i = 0;
        } else {
            c[i] = 0;
            i += 1;
        }
    }

    permuts
}

fn run_program(initial_memory: &[isize; 499], input: Receiver<isize>, output: Sender<isize>) {
    let mut memory = initial_memory.clone();
    let mut i = 0;
    loop {
        let op = memory[i] % 100;
        let mode1 = (memory[i] / 100) % 10;
        let mode2 = memory[i] / 1000;
        match op {
            1 => {
                let (in1, in2, out) = (memory[i+1], memory[i+2], memory[i+3]);
                memory[out as usize] = get_data(&memory, mode1, in1) + get_data(&memory, mode2, in2);
                i += 4;
            }
            2 => {
                let (in1, in2, out) = (memory[i+1], memory[i+2], memory[i+3]);
                memory[out as usize] = get_data(&memory, mode1, in1) * get_data(&memory, mode2, in2);
                i += 4;
            }
            3 => {
                let pos = memory[i+1];
                memory[pos as usize] = input.recv().unwrap();
                i += 2;
            }
            4 => {
                output.send(get_data(&memory, mode1, memory[i+1])).unwrap();
                i += 2;
            }
            5 => {
                let (in1, in2) = (memory[i+1], memory[i+2]);
                if get_data(&memory, mode1, in1) != 0 {
                    i = get_data(&memory, mode2, in2) as usize;
                } else {
                    i += 3;
                }
            }
            6 => {
                let (in1, in2) = (memory[i+1], memory[i+2]);
                if get_data(&memory, mode1, in1) == 0 {
                    i = get_data(&memory, mode2, in2) as usize;
                } else {
                    i += 3;
                }
            }
            7 => {
                let (in1, in2, out) = (memory[i+1], memory[i+2], memory[i+3]);
                memory[out as usize] = if get_data(&memory, mode1, in1) < get_data(&memory, mode2, in2) {
                    1
                } else {
                    0
                };
                i += 4;
            }
            8 => {
                let (in1, in2, out) = (memory[i+1], memory[i+2], memory[i+3]);
                memory[out as usize] = if get_data(&memory, mode1, in1) == get_data(&memory, mode2, in2) {
                    1
                } else {
                    0
                };
                i += 4;
            }
            99 => {
                break;
            }
            _ => {
                panic!("Unknown op code {}", op);
            }
        }
    }
}

fn get_data(memory: &[isize], mode: isize, parameter: isize) -> isize {
    match mode {
        0 => memory[parameter as usize],
        1 => parameter,
        _ => panic!("invalid addressing mode {}", mode)
    }
}
