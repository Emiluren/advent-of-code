use std::collections::{HashMap, HashSet};

fn main() {
    part_one();
    part_two();
}

fn part_one() {
    let alphabet: Vec<_> = ('A' as u8 ..= 'Z' as u8).map(|c| c as char).collect();
    let input = input();
    let mut order = Vec::new();

    while order.len() < alphabet.len() {
        for c in alphabet.iter() {
            if !order.contains(c) {
                match input.get(c) {
                    None => {
                        order.push(*c);
                        break;
                    },
                    Some(deps) => if deps.iter().all(|d| order.contains(d)) {
                        order.push(*c);
                        break;
                    }
                }
            }
        }
    }

    println!("{}", order.iter().collect::<String>());
}

fn part_two() {
    let alphabet: Vec<_> = ('A' as u8 ..= 'Z' as u8).map(|c| c as char).collect();
    let input = input();

    let mut time = 0;
    let mut current_jobs = ['.'; 5];
    let mut start_times = [0; 5];
    let mut finished_jobs = HashSet::new();

    loop {
        // Remove tasks when they are done
        for i in 0..5 {
            let job = current_jobs[i];
            if job != '.' {
                let task_time = 61 + job as u32 - 'A' as u32;
                if start_times[i] + task_time <= time {
                    current_jobs[i] = '.';
                    finished_jobs.insert(job);
                }
            }
        }

        // Stop when all work is finished
        if alphabet.iter().all(|c| finished_jobs.contains(c)) {
            break;
        }

        // Assign job to idle workers
        for i in 0..5 {
            let job = current_jobs[i];
            if job == '.' {
                for c in alphabet.iter() {
                    if !finished_jobs.contains(c) && !current_jobs.contains(c) {
                        match input.get(c) {
                            None => {
                                current_jobs[i] = *c;
                                start_times[i] = time;
                                break;
                            },
                            Some(deps) => if deps.iter().all(|d| finished_jobs.contains(d)) {
                                current_jobs[i] = *c;
                                start_times[i] = time;
                                break;
                            }
                        }
                    }
                }                
            }
        }

        time += 1;
    }

    println!("{}", time);
}

fn input() -> HashMap<char, Vec<char>> {
    let mut input = HashMap::new();
    input.insert('A', vec!['F']);
    input.insert('B', vec!['C', 'D', 'E', 'H', 'L', 'P', 'Q', 'W', 'Y', 'Z']);
    input.insert('D', vec!['C', 'G', 'H', 'S', 'U', 'W', 'Y']);
    input.insert('E', vec!['A', 'C', 'D', 'I', 'L', 'Q', 'R', 'X']);
    input.insert('F', vec!['X']);
    input.insert('H', vec!['A', 'I']);
    input.insert('I', vec!['F']);
    input.insert('J', vec!['F', 'L', 'M', 'Y']);
    input.insert('K', vec!['C']);
    input.insert('L', vec!['A', 'H', 'N', 'U', 'Y']);
    input.insert('M', vec!['C']);
    input.insert('N', vec!['U', 'Y']);
    input.insert('O', vec!['A', 'E', 'I', 'J', 'L', 'R', 'S', 'U', 'W']);
    input.insert('P', vec!['I', 'J', 'O', 'S', 'W', 'Y']);
    input.insert('Q', vec!['D', 'I', 'S', 'Y']);
    input.insert('R', vec!['D', 'H', 'K', 'L', 'T', 'W', 'Y']);
    input.insert('S', vec!['U', 'X']);
    input.insert('T', vec!['D', 'F', 'L', 'Q', 'S', 'U']);
    input.insert('V', vec!['B', 'D', 'E', 'F', 'H', 'N', 'P', 'R', 'Y', 'Z']);
    input.insert('W', vec!['G', 'U']);
    input.insert('Y', vec!['A', 'C', 'G']);
    input.insert('Z', vec!['D', 'G', 'K', 'O', 'P', 'Q', 'T', 'X', 'Y']);
    input
}


    // input.insert('A', vec!['E', 'H', 'L', 'O', 'Y']);
    // input.insert('B', vec!['V']);
    // input.insert('C', vec!['B', 'D', 'E', 'K', 'M', 'Y']);
    // input.insert('D', vec!['B', 'E', 'Q', 'R', 'T', 'V', 'Z']);
    // input.insert('E', vec!['B', 'O', 'V']);
    // input.insert('F', vec!['A', 'I', 'J', 'T', 'V']);
    // input.insert('G', vec!['D', 'W', 'Y', 'Z']);
    // input.insert('H', vec!['B', 'D', 'L', 'R', 'V']);
    // input.insert('I', vec!['E', 'H', 'O', 'P', 'Q']);
    // input.insert('J', vec!['O', 'P']);
    // input.insert('K', vec!['R', 'Z']);
    // input.insert('L', vec!['B', 'E', 'J', 'O', 'R', 'T']);
    // input.insert('M', vec!['J']);
    // input.insert('N', vec!['L', 'V']);
    // input.insert('O', vec!['P', 'Z']);
    // input.insert('P', vec!['B', 'V', 'Z']);
    // input.insert('Q', vec!['B', 'E', 'T', 'Z']);
    // input.insert('R', vec!['E', 'O', 'V']);
    // input.insert('S', vec!['D', 'O', 'P', 'Q', 'T']);
    // input.insert('T', vec!['R', 'Z']);
    // input.insert('U', vec!['D', 'L', 'N', 'O', 'S', 'T', 'W']);
    // input.insert('W', vec!['B', 'D', 'O', 'P', 'R']);
    // input.insert('X', vec!['E', 'F', 'S', 'Z']);
    // input.insert('Y', vec!['B', 'D', 'J', 'L', 'N', 'P', 'Q', 'R', 'V', 'Z']);
    // input.insert('Z', vec!['B', 'V']);
