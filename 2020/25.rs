fn main() {
    let public1: u64 = 19774466;
    let public2 = 7290641;

    let subject = 7;
    let divisor = 20201227;

    let mut loop_size = 1;
    let mut loop1 = None;
    let mut loop2 = None;
    let mut value = 1;
    while loop1.is_none() || loop2.is_none() {
        value *= subject;
        value %= divisor;

        if loop1.is_none() && value == public1 {
            println!("Found loop size 1: {}", loop_size);
            loop1 = Some(loop_size);
        }

        if loop2.is_none() && value == public2 {
            println!("Found loop size 2: {}", loop_size);
            loop2 = Some(loop_size);
        }

        loop_size += 1;
    }

    let loop1 = loop1.unwrap();
    let loop2 = loop2.unwrap();

    let mut key = 1;
    for _ in 0..loop2 {
        key *= public1;
        key %= divisor;
    }
    println!("Part 1: {}", key);
}
