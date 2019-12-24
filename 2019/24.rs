use std::collections::HashSet;

type State = [[char; 5]; 5];

fn main() {
    let input = "#.###
.....
#..#.
##.##
..#.#";

    fn str_to_state(s: &str) -> State {
        let state_vec: Vec<[char; 5]> = s.split('\n').map(
            |s| {
                let row_vec: Vec<char> = s.chars().collect();
                let mut row = ['.'; 5];
                for i in 0..5 {
                    row[i] = row_vec[i];
                }
                row
            }
        ).collect();

        let mut state = [['.'; 5]; 5];
        for i in 0..5 {
            state[i] = state_vec[i];
        }
        state
    }

    let inital_state: State = str_to_state(input);
    let mut state = inital_state;

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

    let mut recursive_states = vec![[['.'; 5]; 5]; 201];
    recursive_states[100] = inital_state;

    for _ in 0..200 {
        tick2(&mut recursive_states);
    }

    let count = recursive_states.iter().flatten().flatten().fold(0, |sum, c| {
        if *c == '#' {
            sum + 1
        } else {
            sum
        }
    });

    // 1503 too low
    println!("Part 2: {}", count);
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

fn tick2(recursive_state: &mut Vec<State>) {
    let old_state = recursive_state.clone();
    for i in 0..201 {
        let state = old_state[i];

        for r in 0..5 {
            for c in 0..5 {
                if r == 2 && c == 2 {
                    continue;
                }

                let mut neighbours = 0;

                fn in_this_state(r: i8, c: i8) -> bool {
                    if r == 2 && c == 2 {
                        return false;
                    }

                    r >= 0 && r < 5 && c >= 0 && c < 5
                }

                let above_in_state = in_this_state(r as i8 - 1, c as i8);
                let below_in_state = in_this_state(r as i8 + 1, c as i8);
                let left_in_state = in_this_state(r as i8, c as i8 - 1);
                let right_in_state = in_this_state(r as i8, c as i8 + 1);

                if above_in_state && state[r-1][c] == '#' {
                    neighbours += 1;
                }
                if below_in_state && state[r+1][c] == '#' {
                    neighbours += 1;
                }
                if left_in_state && state[r][c-1] == '#' {
                    neighbours += 1;
                }
                if right_in_state && state[r][c+1] == '#' {
                    neighbours += 1;
                }

                // Check outer state
                if i > 0 {
                    let outer_state = old_state[i-1];
                    if r == 0 && outer_state[1][2] == '#' {
                        neighbours += 1;
                    }
                    if r == 4 && outer_state[3][2] == '#' {
                        neighbours += 1;
                    }
                    if c == 0 && outer_state[2][1] == '#' {
                        neighbours += 1;
                    }
                    if c == 4 && outer_state[2][3] == '#' {
                        neighbours += 1;
                    }
                }

                // Check inner state
                if i < 200 {
                    let inner_state = old_state[i+1];
                    if r == 1 && c == 2 {
                        for c in 0..5 {
                            if inner_state[0][c] == '#' {
                                neighbours += 1;
                            }
                        }
                    }
                    if r == 3 && c == 2 {
                        for c in 0..5 {
                            if inner_state[4][c] == '#' {
                                neighbours += 1;
                            }
                        }
                    }
                    if r == 2 && c == 1 {
                        for r in 0..5 {
                            if inner_state[r][0] == '#' {
                                neighbours += 1;
                            }
                        }
                    }
                    if r == 2 && c == 3 {
                        for r in 0..5 {
                            if inner_state[r][4] == '#' {
                                neighbours += 1;
                            }
                        }
                    }
                }

                if state[r][c] == '#' {
                    if neighbours != 1 {
                        recursive_state[i][r][c] = '.';
                    }
                } else {
                    if neighbours == 1 || neighbours == 2 {
                        recursive_state[i][r][c] = '#';
                    }
                }
            }
        }
    }
}
