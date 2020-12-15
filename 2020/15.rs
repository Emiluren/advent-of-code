use std::collections::HashMap;

fn main() {
    let input = [0,8,15,2,12,1,4];

    let mut time_latest: HashMap<usize, usize> = input
        .iter()
        .take(input.len() - 1)
        .copied()
        .enumerate()
        .map(|(i, n)| (n, i))
        .collect();

    let mut last_num = input[input.len() - 1];
    for i in input.len()..30_000_000 {
        if i == 2020 {
            println!("Part 1: {}", last_num);
        }

        let num = time_latest.get(&last_num).map(|j| i - j - 1).unwrap_or(0);
        time_latest.insert(last_num, i - 1);
        last_num = num;
    }

    println!("Part 2: {}", last_num);
}
