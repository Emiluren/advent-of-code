use std::fs;
use std::collections::HashMap;

enum GuardState {
    Awake,
    Asleep(usize),
}

fn main() {
    let mut awake_times = HashMap::new();
    let contents = fs::read_to_string("4_input").expect("Cold not open 4_input");
    let mut current_guard: usize = 0;
    let mut guard_state = GuardState::Awake;

    for line in contents.lines() {
        if line.starts_with("#") {
            current_guard = line[1..].parse().unwrap();
        } else {
            let time: usize = line.parse().unwrap();
            if let GuardState::Asleep(fell_asleep_time) = guard_state {
                //let duration = time - fell_asleep_time;
                let entry = awake_times.entry(current_guard).or_insert(Vec::new());
                entry.push((fell_asleep_time, time));
                guard_state = GuardState::Awake;
            } else {
                guard_state = GuardState::Asleep(time);
            }
        }
    }

    part_one(&awake_times);
    part_two(&awake_times);
}

fn part_one(awake_times: &HashMap<usize, Vec<(usize, usize)>>) {
    let mut sleepiest_guard = 0;
    let mut longest_sleep_amount = 0;

    for (guard, guard_times) in awake_times {
        let mut duration = 0;

        for (start, end) in guard_times {
            duration += end - start;
        }

        if duration > longest_sleep_amount {
            sleepiest_guard = *guard;
            longest_sleep_amount = duration;
        }
    }

    let mut sleep_times_for_minute = [0; 60];
    for (start, end) in awake_times.get(&sleepiest_guard).unwrap() {
        for i in *start..*end {
            sleep_times_for_minute[i] += 1;
        }
    }

    let mut sleepiest_minute = 0;
    for i in 0..60 {
        if sleep_times_for_minute[i] > sleep_times_for_minute[sleepiest_minute] {
            sleepiest_minute = i;
        }
    }

    println!(
        "Part 1: guard {} * minute {} = {}",
        sleepiest_guard,
        sleepiest_minute,
        sleepiest_guard * sleepiest_minute
    );
}

fn part_two(awake_times: &HashMap<usize, Vec<(usize, usize)>>) {
    let mut top_guard = 0;
    let mut top_minute = 0;
    let mut top_times = 0;

    for (guard, times) in awake_times {
        let mut sleep_times_for_minute = [0; 60];

        for (start, end) in times {
            for i in *start..*end {
                sleep_times_for_minute[i] += 1;
            }
        }

        for i in 0..60 {
            if sleep_times_for_minute[i] > top_times {
                top_guard = *guard;
                top_minute = i;
                top_times = sleep_times_for_minute[i];
            }
        }
    }

    println!(
        "Part 2: guard {} * minute {} = {}",
        top_guard,
        top_minute,
        top_guard * top_minute
    );
}
