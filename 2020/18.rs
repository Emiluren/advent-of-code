fn main() {
    let input = std::fs::read_to_string("18input").unwrap();

    let mut sum = 0;
    for line in input.lines() {
        let mut chars = line.chars();
        sum += calc(&mut chars);
    }
    println!("Part 1: {}", sum);
}

fn calc(chars: &mut impl Iterator<Item = char>) -> u64 {
    let mut value = None;
    let mut op = None;
    while let Some(c) = chars.next() {
        match c {
            ')' => {
                break;
            }
            '+' => {
                assert!(value != None);
                op = Some((|a,b| a+b) as fn(u64, u64) -> u64);
            }
            '*' => {
                assert!(value != None);
                op = Some((|a,b| a*b) as fn(u64, u64) -> u64);
            }
            _ => {
                let new_v = if c == '(' {
                    calc(chars)
                } else {
                    c.to_digit(10).unwrap() as u64
                };

                if let Some(old_v) = value {
                    value = Some(op.unwrap()(new_v, old_v));
                } else {
                    value = Some(new_v);
                }
                op = None;
            }
        }
    }
    value.unwrap()
}
