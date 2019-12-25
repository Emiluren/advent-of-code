#[derive(Clone, Copy)]
enum ShuffleInstruction { NewStack, Cut(i64), DealIncrement(i64) }
use ShuffleInstruction::*;

fn main() {
    let input = [
        NewStack,
        DealIncrement(57),
        Cut(-4643),
        DealIncrement(59),
        Cut(5189),
        NewStack,
        DealIncrement(24),
        Cut(3207),
        DealIncrement(63),
        Cut(3839),
        DealIncrement(53),
        Cut(-1014),
        DealIncrement(21),
        Cut(-3150),
        NewStack,
        DealIncrement(39),
        Cut(900),
        DealIncrement(6),
        NewStack,
        DealIncrement(65),
        Cut(6108),
        DealIncrement(54),
        Cut(6343),
        DealIncrement(26),
        NewStack,
        Cut(8625),
        DealIncrement(8),
        Cut(-1956),
        NewStack,
        Cut(8750),
        DealIncrement(43),
        Cut(-2930),
        DealIncrement(10),
        Cut(-2359),
        DealIncrement(34),
        Cut(390),
        DealIncrement(46),
        Cut(5467),
        NewStack,
        Cut(61),
        DealIncrement(4),
        Cut(-332),
        NewStack,
        DealIncrement(74),
        Cut(-2568),
        DealIncrement(54),
        NewStack,
        DealIncrement(47),
        Cut(-9034),
        DealIncrement(74),
        Cut(2174),
        NewStack,
        DealIncrement(63),
        Cut(-3966),
        DealIncrement(16),
        Cut(1619),
        DealIncrement(43),
        NewStack,
        Cut(2779),
        NewStack,
        Cut(-1441),
        DealIncrement(52),
        Cut(362),
        DealIncrement(25),
        Cut(-5105),
        NewStack,
        DealIncrement(25),
        Cut(5744),
        DealIncrement(69),
        NewStack,
        Cut(6645),
        DealIncrement(49),
        Cut(-9379),
        DealIncrement(2),
        Cut(2768),
        DealIncrement(21),
        Cut(6900),
        DealIncrement(67),
        Cut(-4226),
        DealIncrement(12),
        Cut(2541),
        DealIncrement(70),
        Cut(-9160),
        DealIncrement(19),
        NewStack,
        Cut(-7165),
        DealIncrement(74),
        NewStack,
        DealIncrement(65),
        Cut(298),
        DealIncrement(24),
        NewStack,
        DealIncrement(29),
        Cut(7412),
        DealIncrement(30),
        Cut(-3224),
        NewStack,
        Cut(-7213),
        DealIncrement(45),
        Cut(8295),
    ];

    let mut cards: Vec<_> = (0..=10_006).collect();

    for instuction in input.iter().cloned() {
        match instuction {
            NewStack => {
                cards.reverse();
            }
            DealIncrement(inc) => {
                deal_with_increment(&mut cards, inc as usize)
            }
            Cut(i) => {
                if i < 0 {
                    cards.rotate_right(i.abs() as usize);
                } else {
                    cards.rotate_left(i as usize);
                }
            }
        }
    }

    println!("Part 1: {}", cards.iter().position(|c| *c == 2019).unwrap());

    let shuffle_times = 101_741_582_076_661;

    let mut increment = 1;
    let mut offset = 0;

    for instuction in input.iter().cloned() {
        match instuction {
            NewStack => {
                increment *= -1;
                offset += increment;
            }
            DealIncrement(inc) => {
                increment *= modinv(inc, DECK_SIZE);
            }
            Cut(i) => {
                offset += i * increment;
            }
        }
        println!("increment = {}, offset = {}", increment, offset);
    }

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
