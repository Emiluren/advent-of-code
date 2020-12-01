use std::fs;

fn main() {
    let contents = fs::read_to_string("1input").unwrap();
    let numbers: Vec<u32> = contents.lines().map(|line| line.parse().unwrap()).collect();

    'outer1: for i in 0..numbers.len() {
        for j in (i+1)..numbers.len() {
            let n = numbers[i];
            let m = numbers[j];

            if n + m == 2020 {
                println!("Part 1: {}", n*m);
                break 'outer1;
            }
        }
    }

    'outer2: for i in 0..numbers.len() {
        for j in (i+1)..numbers.len() {
            for k in (j+1)..numbers.len() {
                let n = numbers[i];
                let m = numbers[j];
                let o = numbers[k];

                if n + m + o == 2020 {
                    println!("Part 2: {}", n*m*o);
                    break 'outer2;
                }
            }
        }
    }
}
