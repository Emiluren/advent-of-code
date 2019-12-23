use std::collections::HashSet;

fn main() {
    let mut cards: Vec<_> = (0..=10_006).collect();

    cards.reverse();
    deal_with_increment(&mut cards, 57);
    cards.rotate_right(4643);
    deal_with_increment(&mut cards, 59);
    cards.rotate_left(5189);
    cards.reverse();
    deal_with_increment(&mut cards, 24);
    cards.rotate_left(3207);
    deal_with_increment(&mut cards, 63);
    cards.rotate_left(3839);
    deal_with_increment(&mut cards, 53);
    cards.rotate_right(1014);
    deal_with_increment(&mut cards, 21);
    cards.rotate_right(3150);
    cards.reverse();
    deal_with_increment(&mut cards, 39);
    cards.rotate_left(900);
    deal_with_increment(&mut cards, 6);
    cards.reverse();
    deal_with_increment(&mut cards, 65);
    cards.rotate_left(6108);
    deal_with_increment(&mut cards, 54);
    cards.rotate_left(6343);
    deal_with_increment(&mut cards, 26);
    cards.reverse();
    cards.rotate_left(8625);
    deal_with_increment(&mut cards, 8);
    cards.rotate_right(1956);
    cards.reverse();
    cards.rotate_left(8750);
    deal_with_increment(&mut cards, 43);
    cards.rotate_right(2930);
    deal_with_increment(&mut cards, 10);
    cards.rotate_right(2359);
    deal_with_increment(&mut cards, 34);
    cards.rotate_left(390);
    deal_with_increment(&mut cards, 46);
    cards.rotate_left(5467);
    cards.reverse();
    cards.rotate_left(61);
    deal_with_increment(&mut cards, 4);
    cards.rotate_right(332);
    cards.reverse();
    deal_with_increment(&mut cards, 74);
    cards.rotate_right(2568);
    deal_with_increment(&mut cards, 54);
    cards.reverse();
    deal_with_increment(&mut cards, 47);
    cards.rotate_right(9034);
    deal_with_increment(&mut cards, 74);
    cards.rotate_left(2174);
    cards.reverse();
    deal_with_increment(&mut cards, 63);
    cards.rotate_right(3966);
    deal_with_increment(&mut cards, 16);
    cards.rotate_left(1619);
    deal_with_increment(&mut cards, 43);
    cards.reverse();
    cards.rotate_left(2779);
    cards.reverse();
    cards.rotate_right(1441);
    deal_with_increment(&mut cards, 52);
    cards.rotate_left(362);
    deal_with_increment(&mut cards, 25);
    cards.rotate_right(5105);
    cards.reverse();
    deal_with_increment(&mut cards, 25);
    cards.rotate_left(5744);
    deal_with_increment(&mut cards, 69);
    cards.reverse();
    cards.rotate_left(6645);
    deal_with_increment(&mut cards, 49);
    cards.rotate_right(9379);
    deal_with_increment(&mut cards, 2);
    cards.rotate_left(2768);
    deal_with_increment(&mut cards, 21);
    cards.rotate_left(6900);
    deal_with_increment(&mut cards, 67);
    cards.rotate_right(4226);
    deal_with_increment(&mut cards, 12);
    cards.rotate_left(2541);
    deal_with_increment(&mut cards, 70);
    cards.rotate_right(9160);
    deal_with_increment(&mut cards, 19);
    cards.reverse();
    cards.rotate_right(7165);
    deal_with_increment(&mut cards, 74);
    cards.reverse();
    deal_with_increment(&mut cards, 65);
    cards.rotate_left(298);
    deal_with_increment(&mut cards, 24);
    cards.reverse();
    deal_with_increment(&mut cards, 29);
    cards.rotate_left(7412);
    deal_with_increment(&mut cards, 30);
    cards.rotate_right(3224);
    cards.reverse();
    cards.rotate_right(7213);
    deal_with_increment(&mut cards, 45);
    cards.rotate_left(8295);

    println!("Part 1: {}", cards.iter().position(|c| *c == 2019).unwrap());

    let shuffle_times = 101_741_582_076_661;

    let mut increment = 1;
    let mut offset = 0;

    increment *= -1;
    offset += increment;
    increment *= modinv(57, DECK_SIZE);
    offset += -4643 * increment;
    increment *= modinv(59, DECK_SIZE);
    offset += 5189 * increment;
    increment *= -1;
    offset += increment;
    increment *= modinv(24, DECK_SIZE);
    offset += 3207 * increment;
    increment *= modinv(63, DECK_SIZE);
    offset += 3839 * increment;
    increment *= modinv(53, DECK_SIZE);
    offset += -1014 * increment;
    increment *= modinv(21, DECK_SIZE);
    offset += -3150 * increment;
    increment *= -1;
    offset += increment;
    increment *= modinv(39, DECK_SIZE);
    offset += 900 * increment;
    increment *= modinv(6, DECK_SIZE);
    increment *= -1;
    offset += increment;
    increment *= modinv(65, DECK_SIZE);
    offset += 6108 * increment;
    increment *= modinv(54, DECK_SIZE);
    offset += 6343 * increment;
    increment *= modinv(26, DECK_SIZE);
    increment *= -1;
    offset += increment;
    offset += 8625 * increment;
    increment *= modinv(8, DECK_SIZE);
    offset += -1956 * increment;
    increment *= -1;
    offset += increment;
    offset += 8750 * increment;
    increment *= modinv(43, DECK_SIZE);
    offset += -2930 * increment;
    increment *= modinv(10, DECK_SIZE);
    offset += -2359 * increment;
    increment *= modinv(34, DECK_SIZE);
    offset += 390 * increment;
    increment *= modinv(46, DECK_SIZE);
    offset += 5467 * increment;
    increment *= -1;
    offset += increment;
    offset += 61 * increment;
    increment *= modinv(4, DECK_SIZE);
    offset += -332 * increment;
    increment *= -1;
    offset += increment;
    increment *= modinv(74, DECK_SIZE);
    offset += -2568 * increment;
    increment *= modinv(54, DECK_SIZE);
    increment *= -1;
    offset += increment;
    increment *= modinv(47, DECK_SIZE);
    offset += -9034 * increment;
    increment *= modinv(74, DECK_SIZE);
    offset += 2174 * increment;
    increment *= -1;
    offset += increment;
    increment *= modinv(63, DECK_SIZE);
    offset += -3966 * increment;
    increment *= modinv(16, DECK_SIZE);
    offset += 1619 * increment;
    increment *= modinv(43, DECK_SIZE);
    increment *= -1;
    offset += increment;
    offset += 2779 * increment;
    increment *= -1;
    offset += increment;
    offset += -1441 * increment;
    increment *= modinv(52, DECK_SIZE);
    offset += 362 * increment;
    increment *= modinv(25, DECK_SIZE);
    offset += -5105 * increment;
    increment *= -1;
    offset += increment;
    increment *= modinv(25, DECK_SIZE);
    offset += 5744 * increment;
    increment *= modinv(69, DECK_SIZE);
    increment *= -1;
    offset += increment;
    offset += 6645 * increment;
    increment *= modinv(49, DECK_SIZE);
    offset += -9379 * increment;
    increment *= modinv(2, DECK_SIZE);
    offset += 2768 * increment;
    increment *= modinv(21, DECK_SIZE);
    offset += 6900 * increment;
    increment *= modinv(67, DECK_SIZE);
    offset += -4226 * increment;
    increment *= modinv(12, DECK_SIZE);
    offset += 2541 * increment;
    increment *= modinv(70, DECK_SIZE);
    offset += -9160 * increment;
    increment *= modinv(19, DECK_SIZE);
    increment *= -1;
    offset += increment;
    offset += -7165 * increment;
    increment *= modinv(74, DECK_SIZE);
    increment *= -1;
    offset += increment;
    increment *= modinv(65, DECK_SIZE);
    offset += 298 * increment;
    increment *= modinv(24, DECK_SIZE);
    increment *= -1;
    offset += increment;
    increment *= modinv(29, DECK_SIZE);
    offset += 7412 * increment;
    increment *= modinv(30, DECK_SIZE);
    offset += -3224 * increment;
    increment *= -1;
    offset += increment;
    offset += -7213 * increment;
    increment *= modinv(45, DECK_SIZE);
    offset += 8295 * increment;

    println!("increment = {}, offset = {}", increment, offset);

    let i = mod_pow(increment, shuffle_times, DECK_SIZE);
    println!("i {}", i);
    let o = offset * (1 - i) * modinv(1 - increment, DECK_SIZE);

    let res = modulo(o + 2020 * i, DECK_SIZE);

    // 17 630 994 690 691 too low
    // 33 955 503 124 298 too low
    // 84 812 307 900 501 too low
    // 111 253 207 841 782 wrong
    // 26 473 957 160 995 wrong
    // 111 102 642 744 155 wrong
    println!("Part 2: {}", res);
}

fn deal_with_increment(cards: &mut Vec<u16>, n: usize) {
    let cards_copy = cards.clone();
    let mut pos = 0;

    for card in cards_copy {
        cards[pos] = card;
        pos = (pos + n) % cards.len();
    }
}

fn modulo(n: i64, div: i64) -> i64 {
    (n % div + div) % div
}

const DECK_SIZE: i64 = 119_315_717_514_047;

fn cut(n: i64, card_to_find: i64) -> i64 {
    if n < 0 {
        let n = -n as i64;
        if card_to_find >= n {
            card_to_find - n
        } else {
            card_to_find + (DECK_SIZE - n)
        }
    } else {
        let n = n as i64;
        if card_to_find >= DECK_SIZE - n {
            card_to_find - (DECK_SIZE - n)
        } else {
            card_to_find + n
        }
    }
}

fn mod_pow(mut base: i64, mut exp: i64, modulus: i64) -> i64 {
    if modulus == 1 {
        return 0
    }
    let mut result = 1;
    base = base % modulus;
    while exp > 0 {
        if exp % 2 == 1 {
            result = result * base % modulus;
        }
        exp = exp >> 1;
        base = base * base % modulus
    }
    result
}

fn xgcd(mut a: i64, mut b: i64) -> (i64, i64, i64) {
    // From https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
    // return (g, x, y) such that a*x + b*y = g = gcd(a, b)
    let mut aa = [1, 0];
    let mut bb = [0, 1];

    loop {
        let q = a / b;
        a = a % b;
        aa[0] = aa[0] - q*aa[1];
        bb[0] = bb[0] - q*bb[1];

        if a == 0 {
            return (b, aa[1], bb[1]);
        }

        let q = b / a;
        b = b % a;
        aa[1] = aa[1] - q*aa[0];
        bb[1] = bb[1] - q*bb[0];

        if b == 0 {
            return (a, aa[0], bb[0]);
        }
    }
}

fn modinv(a: i64, b: i64) -> i64 {
    return mod_pow(a, b-2, b);
    // return x such that (x * a) % b == 1
    let (g, x, _) = xgcd(a, b);
    if g == 1 {
        x % b
    } else {
        panic!("Could not find modular inverse of {} and {}", a, b)
    }
}

fn deal_big(card_to_find: i64, n: i64) -> i64 {
    let n_inv = modinv(n, DECK_SIZE);
    (n_inv * card_to_find) % DECK_SIZE
}

fn new_stack(card_to_find: i64) -> i64 {
    DECK_SIZE - card_to_find - 1
}
