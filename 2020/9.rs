use std::fs;

fn main() {
    let contents = fs::read_to_string("9input").unwrap();
    let numbers: Vec<usize> = contents.lines().map(|l| l.parse().unwrap()).collect();

    for (i, n) in numbers.iter().enumerate().skip(25) {
        let mut found_terms = false;

        // Check if any two of the previous 25 numbers sum to n
        'outer: for (j, term1) in numbers.iter().skip(i-25).take(25).enumerate() {
            for term2 in numbers.iter().skip(i-25+j+1).take(24-j) {
                if term1 + term2 == *n {
                    found_terms = true;
                    break 'outer;
                }
            }
        }

        if !found_terms {
            println!("Part 1: {}", n);
            break;
        }
    }
}
