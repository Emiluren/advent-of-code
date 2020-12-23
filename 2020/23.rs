use std::collections::VecDeque;

fn main() {
    let input = [1, 5, 6, 7, 9, 4, 8, 2, 3];
    //let input = [3, 8, 9, 1, 2, 5, 4, 6, 7];

    let mut cups: VecDeque<u32> = input.iter().copied().collect();

    for _ in 0..100 {
        let current = cups.pop_front().unwrap();
        let next_three = [
            cups.pop_front().unwrap(),
            cups.pop_front().unwrap(),
            cups.pop_front().unwrap(),
        ];

        let mut destination = current;
        'determine_destination: loop {
            destination -= 1;
            if destination < 1 {
                destination = 9;
            }
            for i in 0..3 {
                if next_three[i] == destination {
                    continue 'determine_destination;
                }
            }
            break;
        }

        let i = cups.iter()
            .enumerate()
            .filter_map(|(i, n)| if *n == destination { Some(i) } else { None })
            .nth(0)
            .unwrap();

        for j in 0..3 {
            cups.insert(i+j+1, next_three[j]);
        }
        cups.push_back(current);
    }

    while *cups.front().unwrap() != 1 {
        cups.rotate_left(1);
    }
    let cup_strings: Vec<_> = cups.iter().skip(1).map(|n| n.to_string()).collect();
    println!("Part 1: {}", cup_strings.join(""));
}
