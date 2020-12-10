use std::fs;

fn main () {
    let contents = fs::read_to_string("10input").unwrap();
    let mut numbers: Vec<u8> = contents.lines().map(|l| l.parse().unwrap()).collect();

    numbers.sort();

    let mut prev = 0;
    let mut count_1 = 0;
    let mut count_3 = 1;
    for n in &numbers {
        let diff = *n - prev;
        if diff == 3 {
            count_3 += 1;
        } else if diff == 1 {
            count_1 += 1;
        }

        prev = *n;
    }
    println!("Part 1: {}", count_1 * count_3);

    let mut ways_of_reaching = vec![0_u64; numbers.len()];
    for (i, n) in numbers.iter().enumerate() {
        for (j, m) in numbers.iter().enumerate().take(i).rev() {
            let diff = *n - *m;
            if diff > 3 {
                break;
            }

            ways_of_reaching[i] += ways_of_reaching[j];
        }

        if *n <= 3 {
            ways_of_reaching[i] += 1
        }
    }

    println!("Part 2: {}", ways_of_reaching[numbers.len() - 1]);
}
