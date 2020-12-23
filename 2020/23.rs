fn main() {
    let input = [1, 5, 6, 7, 9, 4, 8, 2, 3];
    //let input = [3, 8, 9, 1, 2, 5, 4, 6, 7];

    let mut start_cups: Vec<usize> = vec![0; 10];
    for (i, n) in input.iter().enumerate() {
        start_cups[*n] = input[(i+1) % input.len()];
    }

    let mut next_cup = start_cups.clone();
    let mut current = input[0];
    for _ in 0..100 {
        iterate_once(current, &mut next_cup);
        current = next_cup[current];
    }

    let mut cup_strings = Vec::new();
    let mut i = next_cup[1];
    while i != 1 {
        cup_strings.push(i.to_string());
        i = next_cup[i];
    }
    println!("Part 1: {}", cup_strings.join(""));

    let mut next_cup = start_cups.clone();
    for i in 10..=1_000_000 {
        next_cup.push(i+1);
    }
    next_cup[input[input.len() - 1]] = 10;
    let last_i = next_cup.len() - 1;
    next_cup[last_i] = input[0];

    let mut current = 1;
    for _ in 0..10_000_000 {
        iterate_once(current, &mut next_cup);
        current = next_cup[current];
    }

    let next1 = next_cup[1];
    let next2 = next_cup[next1];
    println!("Part 2: {}", next1 * next2)
}

fn iterate_once(current: usize, next_cup: &mut [usize]) {
    if current == 0 {
        panic!("current = 0");
    }
    let next1 = next_cup[current];
    let next2 = next_cup[next1];
    let next3 = next_cup[next2];

    let mut destination = current;
    'determine_destination: loop {
        destination -= 1;
        if destination < 1 {
            destination = next_cup.len() - 1;
        }
        if destination == next1 || destination == next2 || destination == next3 {
            continue 'determine_destination;
        }
        break;
    }

    next_cup[current] = next_cup[next3];
    next_cup[next1] = next2;
    next_cup[next2] = next3;
    next_cup[next3] = next_cup[destination];
    next_cup[destination] = next1;
}
