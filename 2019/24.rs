use std::collections::HashSet;

type State = Vec<Vec<char>>;

fn main() {
    let input = "#.###
.....
#..#.
##.##
..#.#";

    fn str_to_state(s: &str) -> State {
        s.split('\n').map(
            |s| s.chars().collect()
        ).collect()
    }

    let mut state: State = str_to_state(input);

    let mut old_states = HashSet::new();
    old_states.insert(state.clone());

    loop {
        state = tick1(&state);
        if old_states.contains(&state) {
            println!("first layout twice:");
            print_state(&state);
            println!("Part 1: {}", calc_biodiversity(&state));
            break;
        }
        old_states.insert(state.clone());
    }
}

fn print_state(state: &State) {
    for r in 0..state.len() {
        for c in 0..state[0].len() {
            print!("{}", state[r][c]);
        }
        println!()
    }
    println!()
}

fn calc_biodiversity(state: &State) -> usize {
    let mut tot = 0;

    let mut score = 1;
    for r in 0..state.len() {
        for c in 0..state[0].len() {
            if state[r][c] == '#' {
                tot += score;
            }
            score *= 2;
        }
    }
    tot
}

fn tick1(state: &State) -> State {
    let height = state.len();
    let width = state[0].len();

    let mut new_state = state.clone();
    for r in 0..height {
        for c in 0..width {
            let mut neighbours = 0;

            if r > 0 && state[r-1][c] == '#' {
                neighbours += 1;
            }
            if r < height - 1 && state[r+1][c] == '#' {
                neighbours += 1;
            }
            if c > 0 && state[r][c-1] == '#' {
                neighbours += 1;
            }
            if c < width - 1 && state[r][c+1] == '#' {
                neighbours += 1;
            }

            if state[r][c] == '#' {
                if neighbours != 1 {
                    new_state[r][c] = '.';
                }
            } else {
                if neighbours == 1 || neighbours == 2 {
                    new_state[r][c] = '#';
                }
            }
        }
    }
    new_state
}
