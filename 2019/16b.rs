fn main() {
    let input = [5, 9, 7, 6, 5, 2, 1, 6, 6, 3, 4, 9, 5, 2, 1, 4, 7, 7, 3, 5, 4, 1, 9, 5, 8, 8, 1, 8, 6, 1, 6, 8, 4, 1, 6, 8, 0, 7, 7, 8, 2, 3, 7, 9, 7, 3, 8, 2, 6, 4, 3, 1, 6, 9, 0, 3, 5, 8, 3, 1, 9, 1, 8, 4, 1, 3, 3, 2, 1, 7, 6, 6, 1, 5, 4, 0, 8, 5, 0, 1, 5, 7, 1, 8, 2, 2, 7, 9, 9, 9, 8, 5, 6, 9, 3, 4, 8, 6, 1, 0, 7, 9, 2, 3, 5, 9, 3, 1, 2, 0, 5, 9, 0, 3, 0, 6, 9, 6, 0, 2, 3, 3, 5, 3, 6, 3, 8, 8, 9, 8, 8, 0, 0, 5, 0, 2, 4, 5, 4, 6, 6, 0, 3, 7, 1, 1, 1, 4, 8, 1, 9, 7, 3, 1, 7, 5, 3, 0, 7, 5, 9, 7, 6, 1, 1, 0, 8, 1, 9, 2, 8, 7, 3, 3, 6, 8, 0, 3, 6, 4, 9, 3, 6, 5, 0, 9, 7, 9, 5, 1, 1, 6, 7, 0, 8, 4, 7, 4, 5, 3, 1, 5, 3, 4, 1, 9, 5, 1, 7, 5, 0, 2, 9, 5, 2, 3, 4, 1, 6, 5, 0, 8, 7, 1, 5, 2, 9, 6, 5, 2, 3, 4, 0, 9, 6, 5, 5, 7, 2, 6, 1, 6, 1, 7, 3, 1, 1, 6, 7, 9, 7, 3, 2, 5, 1, 8, 4, 4, 8, 7, 8, 6, 3, 3, 4, 8, 4, 6, 9, 4, 7, 3, 9, 2, 3, 5, 0, 2, 6, 0, 2, 6, 3, 4, 4, 4, 1, 6, 6, 4, 9, 8, 1, 6, 4, 4, 4, 9, 7, 2, 2, 8, 8, 2, 4, 2, 9, 1, 0, 3, 8, 3, 7, 9, 0, 7, 0, 6, 7, 4, 9, 0, 2, 0, 2, 2, 8, 3, 0, 0, 6, 3, 8, 8, 6, 1, 3, 2, 3, 9, 1, 0, 3, 0, 6, 5, 4, 9, 8, 4, 4, 4, 8, 5, 9, 7, 6, 5, 3, 1, 6, 4, 8, 6, 2, 2, 2, 8, 7, 3, 9, 1, 3, 0, 6, 7, 6, 4, 0, 0, 2, 6, 3, 4, 0, 9, 0, 8, 4, 4, 8, 9, 4, 9, 7, 5, 3, 2, 6, 3, 9, 2, 8, 9, 8, 1, 7, 7, 9, 2, 0, 8, 6, 1, 8, 5, 7, 5, 0, 5, 7, 5, 4, 3, 8, 4, 0, 6, 9, 1, 3, 7, 7, 1, 9, 0, 7, 4, 0, 4, 0, 0, 6, 4, 5, 2, 5, 9, 2, 5, 4, 4, 8, 1, 4, 9, 2, 9, 2, 7, 2, 7, 9, 6, 1, 9, 2, 6, 4, 6, 8, 4, 6, 3, 1, 4, 3, 6, 1, 0, 7, 4, 7, 8, 6, 7, 2, 8, 1, 7, 2, 3, 0, 8, 7, 1, 0, 8, 6, 4, 3, 7, 9, 0, 2, 3, 0, 2, 8, 8, 0, 7, 5, 8, 0, 9, 4, 8, 2, 0, 1, 1, 9, 9, 5, 4, 0, 3, 9, 6, 4, 6, 0, 3, 1, 0, 2, 8, 0, 5, 3, 3, 7, 7, 1, 5, 6, 6, 8, 2, 4, 6, 0, 3, 4, 5, 6, 5, 8, 1, 0, 4, 3, 2, 1, 5, 9, 9, 9, 4, 7, 3, 4, 2, 4, 3, 9, 5, 0, 4, 6, 5, 7, 0, 1, 3, 4, 2, 2, 1, 1, 8, 2, 8, 5, 2, 3, 6, 3, 8, 9, 1, 1, 1, 4, 3, 7, 4, 8, 1, 0, 2, 6, 3, 8, 8, 7, 8, 7, 5, 6, 3, 8, 3, 5, 5, 7, 3, 0, 6, 0, 5, 8, 9, 5, 6, 9, 5, 1, 2, 3, 5, 9, 8, 6, 3, 7, 1, 2, 1];
    let offset = 5976521;
    let mut state: Vec<u8> = std::iter::repeat(input.iter()).take(10000).flatten().skip(offset).cloned().collect();

    for _ in 0 .. 100 {
        for i in (0..state.len() - 1).rev() {
            state[i] = (state[i] + state[i+1]) % 10;
        }
    }

    let mut res_string = String::new();
    for i in 0..8 {
        res_string += &format!("{}", state[i]);
    }
    println!("Part 2: {}", res_string);
}
