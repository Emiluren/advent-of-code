fn main() {
    let input = [0,8,15,2,12,1,4];

    let mut time_latest = vec![None; 30_000_000];
    for (i, n) in input.iter().enumerate() {
        time_latest[*n] = Some(i);
    }

    let mut last_num = input[input.len() - 1];
    for i in input.len()..time_latest.len() {
        if i == 2020 {
            println!("Part 1: {}", last_num);
        }

        let num = time_latest[last_num].map(|j| i - j - 1).unwrap_or(0);
        time_latest[last_num] = Some(i - 1);
        last_num = num;
    }

    println!("Part 2: {}", last_num);
}
