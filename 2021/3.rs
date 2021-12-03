use std::io::BufRead;

fn main() {
    let file = std::fs::File::open("3input").unwrap();
    let mut input = Vec::new();

    for line in std::io::BufReader::new(file).lines() {
        let mut line_bits = Vec::new();
        for c in line.unwrap().chars() {
            line_bits.push(c == '1');
        }
        input.push(line_bits);
    }

    let mut gamma = Vec::new();
    let mut epsilon = Vec::new();
    for i in 0..input[0].len() {
        let omc = one_most_common(i, &input);
        gamma.push(omc);
        epsilon.push(!omc);
    }
    println!("Part 1: {}", num_to_int(&gamma) * num_to_int(&epsilon));

    let oxygen = find_rating(input.clone(), true);
    let co2 = find_rating(input.clone(), false);

    println!("Part 2: {}", num_to_int(&oxygen) * num_to_int(&co2));
}

fn num_to_int(num: &[bool]) -> i32 {
    let mut res = 0;
    for (i, b) in num.iter().enumerate() {
        let bit_i = num.len() - i - 1;
        if *b {
            res |= 1 << bit_i;
        }
    }
    res
}

fn find_rating(mut input: Vec<Vec<bool>>, is_oxygen: bool) -> Vec<bool> {
    let mut i = 0;
    while input.len() > 1 {
        let mut mc = one_most_common(i, &input);
        if !is_oxygen {
            mc = !mc;
        }
        // Would probably be easier with xor but whatever
        input.retain(|num| num[i] && mc || !num[i] && !mc);
        i += 1;
    }
    input[0].clone()
}

fn one_most_common(i: usize, input: &[Vec<bool>]) -> bool {
    let mut one_count = 0;
    for j in 0..input.len() {
        if input[j][i] {
            one_count += 1;
        }
    }

    one_count*2 >= input.len()
}

fn print_nums(input: &[Vec<bool>]) {
    for l in input {
        println!("{}", l.iter().map(|b| if *b { '1' } else { '0' }).collect::<String>());
    }
    println!();
}
