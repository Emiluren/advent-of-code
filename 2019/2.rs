fn main() {
    println!("part 1: {}", run_prog(12, 2));

    for n in 0..100 {
        for v in 0..100 {
            if run_prog(n, v) == 19690720 {
                println!("part 2: {}", 100 * n + v);
                break;
            }
        }
    }
}

fn run_prog(noun: usize, verb: usize) -> usize {
    let mut input = [
        1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 1, 13, 19, 1, 9, 19, 23, 2, 13, 23, 27, 2, 27, 13,
        31, 2, 31, 10, 35, 1, 6, 35, 39, 1, 5, 39, 43, 1, 10, 43, 47, 1, 5, 47, 51, 1, 13, 51, 55, 2, 55,
        9, 59, 1, 6, 59, 63, 1, 13, 63, 67, 1, 6, 67, 71, 1, 71, 10, 75, 2, 13, 75, 79, 1, 5, 79, 83, 2,
        83, 6, 87, 1, 6, 87, 91, 1, 91, 13, 95, 1, 95, 13, 99, 2, 99, 13, 103, 1, 103, 5, 107, 2, 107,
        10, 111, 1, 5, 111, 115, 1, 2, 115, 119, 1, 119, 6, 0, 99, 2, 0, 14, 0
    ];

    input[1] = noun;
    input[2] = verb;

    for i in 0 .. (input.len() / 4) {
        let i = i * 4;
        if let [op, in1, in2, out] = input[i..i+4] {
            match op {
                1 => { input[out] = input[in1] + input[in2] }
                2 => { input[out] = input[in1] * input[in2] }
                _ => { break }
            };
        }
    }

    input[0]
}
