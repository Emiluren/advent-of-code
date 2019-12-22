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

    let mut card_to_find = 2020;
    let shuffle_times: usize = 101_741_582_076_661;
    let mut iteration = 0;

    let mut found_set = HashSet::new();
    found_set.insert(2020);
    let mut found_vec = vec![2020];

    let mut found_cycle = false;

    while iteration < shuffle_times {
        if iteration % 1_000_000 == 0 {
            //println!("On iteration {}", iteration);
        }

        card_to_find = cut(8295, card_to_find);
        card_to_find = deal_big(card_to_find, 45);
        card_to_find = cut(-7213, card_to_find);
        card_to_find = new_stack(card_to_find);
        card_to_find = cut(-3224, card_to_find);
        card_to_find = deal_big(card_to_find, 30);
        card_to_find = cut(7412, card_to_find);
        card_to_find = deal_big(card_to_find, 29);
        card_to_find = new_stack(card_to_find);
        card_to_find = deal_big(card_to_find, 24);
        card_to_find = cut(298, card_to_find);
        card_to_find = deal_big(card_to_find, 65);
        card_to_find = new_stack(card_to_find);
        card_to_find = deal_big(card_to_find, 74);
        card_to_find = cut(-7165, card_to_find);
        card_to_find = new_stack(card_to_find);
        card_to_find = deal_big(card_to_find, 19);
        card_to_find = cut(-9160, card_to_find);
        card_to_find = deal_big(card_to_find, 70);
        card_to_find = cut(2541, card_to_find);
        card_to_find = deal_big(card_to_find, 12);
        card_to_find = cut(-4226, card_to_find);
        card_to_find = deal_big(card_to_find, 67);
        card_to_find = cut(6900, card_to_find);
        card_to_find = deal_big(card_to_find, 21);
        card_to_find = cut(2768, card_to_find);
        card_to_find = deal_big(card_to_find, 2);
        card_to_find = cut(-9379, card_to_find);
        card_to_find = deal_big(card_to_find, 49);
        card_to_find = cut(6645, card_to_find);
        card_to_find = new_stack(card_to_find);
        card_to_find = deal_big(card_to_find, 69);
        card_to_find = cut(5744, card_to_find);
        card_to_find = deal_big(card_to_find, 25);
        card_to_find = new_stack(card_to_find);
        card_to_find = cut(-5105, card_to_find);
        card_to_find = deal_big(card_to_find, 25);
        card_to_find = cut(362, card_to_find);
        card_to_find = deal_big(card_to_find, 52);
        card_to_find = cut(-1441, card_to_find);
        card_to_find = new_stack(card_to_find);
        card_to_find = cut(2779, card_to_find);
        card_to_find = new_stack(card_to_find);
        card_to_find = deal_big(card_to_find, 43);
        card_to_find = cut(1619, card_to_find);
        card_to_find = deal_big(card_to_find, 16);
        card_to_find = cut(-3966, card_to_find);
        card_to_find = deal_big(card_to_find, 63);
        card_to_find = new_stack(card_to_find);
        card_to_find = cut(2174, card_to_find);
        card_to_find = deal_big(card_to_find, 74);
        card_to_find = cut(-9034, card_to_find);
        card_to_find = deal_big(card_to_find, 47);
        card_to_find = new_stack(card_to_find);
        card_to_find = deal_big(card_to_find, 54);
        card_to_find = cut(-2568, card_to_find);
        card_to_find = deal_big(card_to_find, 74);
        card_to_find = new_stack(card_to_find);
        card_to_find = cut(-332, card_to_find);
        card_to_find = deal_big(card_to_find, 4);
        card_to_find = cut(61, card_to_find);
        card_to_find = new_stack(card_to_find);
        card_to_find = cut(5467, card_to_find);
        card_to_find = deal_big(card_to_find, 46);
        card_to_find = cut(390, card_to_find);
        card_to_find = deal_big(card_to_find, 34);
        card_to_find = cut(-2359, card_to_find);
        card_to_find = deal_big(card_to_find, 10);
        card_to_find = cut(-2930, card_to_find);
        card_to_find = deal_big(card_to_find, 43);
        card_to_find = cut(8750, card_to_find);
        card_to_find = new_stack(card_to_find);
        card_to_find = cut(-1956, card_to_find);
        card_to_find = deal_big(card_to_find, 8);
        card_to_find = cut(8625, card_to_find);
        card_to_find = new_stack(card_to_find);
        card_to_find = deal_big(card_to_find, 26);
        card_to_find = cut(6343, card_to_find);
        card_to_find = deal_big(card_to_find, 54);
        card_to_find = cut(6108, card_to_find);
        card_to_find = deal_big(card_to_find, 65);
        card_to_find = new_stack(card_to_find);
        card_to_find = deal_big(card_to_find, 6);
        card_to_find = cut(900, card_to_find);
        card_to_find = deal_big(card_to_find, 39);
        card_to_find = new_stack(card_to_find);
        card_to_find = cut(-3150, card_to_find);
        card_to_find = deal_big(card_to_find, 21);
        card_to_find = cut(-1014, card_to_find);
        card_to_find = deal_big(card_to_find, 53);
        card_to_find = cut(3839, card_to_find);
        card_to_find = deal_big(card_to_find, 63);
        card_to_find = cut(3207, card_to_find);
        card_to_find = deal_big(card_to_find, 24);
        card_to_find = new_stack(card_to_find);
        card_to_find = cut(5189, card_to_find);
        card_to_find = deal_big(card_to_find, 59);
        card_to_find = cut(-4643, card_to_find);
        card_to_find = deal_big(card_to_find, 57);
        card_to_find = new_stack(card_to_find);

        iteration += 1;

        // println!("{}", card_to_find);
        // if card_to_find > 100_000_000_000_000 {
        //     break
        // }

        // let cycle_start = 140024;

        // if card_to_find == 17630994690691 {
        //     println!("Found {} at {} ", card_to_find, iteration);
        // }

        if !found_cycle && found_set.contains(&card_to_find)
        {
            let first_found_index = found_vec.iter().position(|c| *c == card_to_find).unwrap();
            let cycle_length = iteration - first_found_index;
            let iterations_left = (shuffle_times - first_found_index) % cycle_length;
            let new_iteration = shuffle_times - iterations_left;

            println!("\nCycle found after {} iterations", iteration);
            println!("First found {} at {}", card_to_find, first_found_index);
            println!("cycle length {}", cycle_length);
            println!("iteration changed to {}\n", new_iteration);

            while iteration + cycle_length < shuffle_times {
                iteration += cycle_length;
            }

            println!("Iterations = {}", iteration);

            found_cycle = true;
        }

        if !found_cycle {
            found_set.insert(card_to_find);
            found_vec.push(card_to_find);
        }
    }

    // 17 630 994 690 691 too low
    // 33 955 503 124 298 too low
    println!("Part 2: {}", card_to_find);
}

fn deal_with_increment(cards: &mut Vec<u16>, n: usize) {
    let cards_copy = cards.clone();
    let mut pos = 0;

    for card in cards_copy {
        cards[pos] = card;
        pos = (pos + n) % cards.len();
    }
}

const DECK_SIZE: usize = 119_315_717_514_047;

fn cut(n: i64, card_to_find: usize) -> usize {
    if n < 0 {
        let n = -n as usize;
        if card_to_find >= n {
            card_to_find - n
        } else {
            card_to_find + (DECK_SIZE - n)
        }
    } else {
        let n = n as usize;
        if card_to_find >= DECK_SIZE - n {
            card_to_find - (DECK_SIZE - n)
        } else {
            card_to_find + n
        }
    }
}

fn xgcd(mut a: usize, mut b: usize) -> (usize, usize, usize) {
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

fn modinv(a: usize, b: usize) -> usize {
    // return x such that (x * a) % b == 1
    let (g, x, _) = xgcd(a, b);
    if g == 1 {
        x % b
    } else {
        panic!("Could not find modular inverse of {} and {}", a, b)
    }
}

fn deal_big(card_to_find: usize, n: usize) -> usize {
    let n_inv = modinv(n, DECK_SIZE);
    (n_inv * card_to_find) % DECK_SIZE
}

fn new_stack(card_to_find: usize) -> usize {
    DECK_SIZE - card_to_find - 1
}
