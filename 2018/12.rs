use std::collections::HashSet;

fn main() {
    let initial_state = "###.......##....#.#.#..###.##..##.....#....#.#.....##.###...###.#...###.###.#.###...#.####.##.#....#";

    let mut live_states = HashSet::new();
    live_states.insert("..###".to_string());
    live_states.insert("..#.#".to_string());
    live_states.insert("##.##".to_string());
    live_states.insert(".###.".to_string());
    live_states.insert("#####".to_string());
    live_states.insert("#.#..".to_string());
    live_states.insert(".##..".to_string());
    live_states.insert("...#.".to_string());
    live_states.insert("#.##.".to_string());
    live_states.insert("..#..".to_string());
    live_states.insert("##...".to_string());
    live_states.insert("###.#".to_string());
    live_states.insert("#..#.".to_string());
    live_states.insert("#.###".to_string());
    live_states.insert("###..".to_string());
    live_states.insert(".#...".to_string());
    live_states.insert("#.#.#".to_string());

    let mut current_state: Vec<_> = (".....".to_string() + initial_state + ".".repeat(140).as_str()).chars().collect();
    // After 129 iterations the pattern just shifts right
    for iteration in 0 .. 129 {
        let mut new_state = current_state.clone();
        for i in 2 .. current_state.len() - 2 {
            let slice: String = current_state[i-2 ..= i+2].iter().collect();
            new_state[i] = if live_states.contains(&slice) {
                '#'
            } else {
                '.'
            }
        }
        current_state = new_state;
        //println!("{}", current_state.iter().collect::<String>());

        if iteration == 19 {
            println!("Part 1: {}", calc_score(-5, &current_state));
        }
    }

    let additional_generations = 50_000_000_000 - 129;
    let final_score = calc_score(additional_generations - 5, &current_state);
    println!("Part 2: {}", final_score)
}

fn calc_score(leftmost_index: i64, current_state: &Vec<char>) -> i64 {
    let mut score = 0;
    for i in 0 .. current_state.len() {
        if current_state[i] == '#' {
            score += i as i64 + leftmost_index;
        }
    }
    score
}
