use std::fs;

fn main() {
    let contents = fs::read_to_string("9input").unwrap();
    let numbers: Vec<usize> = contents.lines().map(|l| l.parse().unwrap()).collect();

    let mut invalid_number = None;

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
            invalid_number = Some(*n);
            break;
        }
    }

    let invalid_number = invalid_number.unwrap();
    println!("Part 1: {}", invalid_number);

    for (i, n) in numbers.iter().enumerate() {
        let mut sum = *n;

        let mut smallest = *n;
        let mut largest = *n;

        for m in numbers.iter().skip(i+1) {
            sum += m;

            smallest = smallest.min(*m);
            largest = largest.max(*m);

            if sum == invalid_number {
                println!("Part 2: {}", smallest + largest);
                return;
            } else if sum > invalid_number {
                break;
            }
        }
    }
}
