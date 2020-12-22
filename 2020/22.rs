use std::collections::{HashSet, VecDeque};

fn main() {
    let input = std::fs::read_to_string("22input").unwrap();
    let mut lines = input.lines();

    let mut player1: VecDeque<usize> = VecDeque::new();
    lines.next().unwrap(); // Skip "Player 1:"
    while let Some(line) = lines.next() {
        if line == "" {
            break;
        }

        player1.push_back(line.parse().unwrap());
    }

    let mut player2: VecDeque<usize> = VecDeque::new();
    lines.next().unwrap(); // Skip "Player 2:"
    while let Some(line) = lines.next() {
        if line == "" {
            break;
        }

        player2.push_back(line.parse().unwrap());
    }

    let player1_start = player1.clone();
    let player2_start = player2.clone();

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

    let score = |player1: VecDeque<usize>, player2: VecDeque<usize>| {
        let winner = if player1.len() == 0 {
            player2
        } else {
            player1
        };

        winner.into_iter()
            .rev()
            .enumerate()
            .map(|(i, card)| (i + 1) * card)
            .sum::<usize>()
    };
    println!("Part 1: {}", score(player1, player2));

    let (player1, player2) = recursive_combat(player1_start, player2_start);
    println!("Part 2: {}", score(player1, player2));
}

fn recursive_combat(mut player1: VecDeque<usize>, mut player2: VecDeque<usize>) -> (VecDeque<usize>, VecDeque<usize>) {
    let mut previous_states = HashSet::new();

    while player1.len() > 0 && player2.len() > 0 {
        if previous_states.contains(&(player1.clone(), player2.clone())) {
            break;
        }
        previous_states.insert((player1.clone(), player2.clone()));

        let card1 = player1.pop_front().unwrap();
        let card2 = player2.pop_front().unwrap();

        let winner = if card1 > player1.len() || card2 > player2.len() {
            if card1 > card2 {
                1
            } else {
                2
            }
        } else {
            let (sub_player1, sub_player2) = recursive_combat(
                player1.iter().take(card1).copied().collect(),
                player2.iter().take(card2).copied().collect(),
            );

            if sub_player1.len() == 0 {
                2
            } else if sub_player2.len() == 0 {
                1
            } else {
                1
            }
        };

        if winner == 1 {
            player1.push_back(card1);
            player1.push_back(card2);
        } else {
            assert_eq!(winner, 2);
            player2.push_back(card2);
            player2.push_back(card1);
        }
    }

    (player1, player2)
}
