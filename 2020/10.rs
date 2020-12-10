use std::fs;

fn main () {
    let contents = fs::read_to_string("10input").unwrap();
    let mut numbers: Vec<usize> = contents.lines().map(|l| l.parse().unwrap()).collect();

    numbers.sort();

    let mut prev = 0;
    let mut count_1 = 0;
    let mut count_3 = 1;
    for n in numbers {
        let diff = n - prev;
        if diff == 3 {
            count_3 += 1;
        } else if diff == 1 {
            count_1 += 1;
        }

        prev = n;
    }

    println!("Part 1: {}", count_1 * count_3)
}
