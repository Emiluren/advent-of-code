use std::collections::VecDeque;

fn main() {
    let input = std::fs::read_to_string("22input").unwrap();
    let mut lines = input.lines();

    let mut player1: VecDeque<u8> = VecDeque::new();
    lines.next().unwrap(); // Skip "Player 1:"
    while let Some(line) = lines.next() {
        if line == "" {
            break;
        }

        player1.push_back(line.parse().unwrap());
    }

    let mut player2: VecDeque<u8> = VecDeque::new();
    lines.next().unwrap(); // Skip "Player 2:"
    while let Some(line) = lines.next() {
        if line == "" {
            break;
        }

        player2.push_back(line.parse().unwrap());
    }

    while player1.len() > 0 && player2.len() > 0 {
        let card1 = player1.pop_front().unwrap();
        let card2 = player2.pop_front().unwrap();

        if card1 > card2 {
            player1.push_back(card1);
            player1.push_back(card2);
        } else {
            player2.push_back(card2);
            player2.push_back(card1);
        }
    }

    let winner = if player1.len() == 0 {
        player2
    } else {
        player1
    };

    let score: usize = winner.into_iter()
        .rev()
        .enumerate()
        .map(|(i, card)| (i + 1) * card as usize)
        .sum();
    println!("Part 1: {}", score);
}
