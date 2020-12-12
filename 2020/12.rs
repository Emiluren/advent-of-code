use std::fs;

#[derive(Copy, Clone, PartialEq, Eq)]
enum Direction { North, East, South, West }
use Direction::*;

impl Direction {
    fn turn_left1(self) -> Self {
        match self {
            North => West, East => North, South => East, West => South
        }
    }

    fn turn_right1(self) -> Self {
        match self {
            North => East, East => South, South => West, West => North
        }
    }

    fn invert(self) -> Self {
        match self {
            North => South, East => West, South => North, West => East
        }
    }

    fn turn_left(self, amount: i32) -> Self {
        match amount {
            90 => self.turn_left1(),
            180 => self.invert(),
            270 => self.turn_right1(),
            _ => panic!("Unknown amount {}", amount),
        }
    }

    fn turn_right(self, amount: i32) -> Self {
        match amount {
            90 => self.turn_right1(),
            180 => self.invert(),
            270 => self.turn_left1(),
            _ => panic!("Unknown amount {}", amount),
        }
    }
}

fn main() {
    let contents = fs::read_to_string("12input").unwrap();

    let mut pos_x: i32 = 0;
    let mut pos_y: i32 = 0;
    let mut dir = East;
    for instruction in contents.lines() {
        let action = instruction.chars().nth(0).unwrap();
        let amount = instruction.chars().skip(1).collect::<String>().parse().unwrap();

        match action {
            'N' => pos_y += amount,
            'S' => pos_y -= amount,
            'E' => pos_x += amount,
            'W' => pos_x -= amount,
            'L' => dir = dir.turn_left(amount),
            'R' => dir = dir.turn_right(amount),
            'F' => match dir {
                North => pos_y += amount,
                East => pos_x += amount,
                South => pos_y -= amount,
                West => pos_x -= amount,
            }
            _ => panic!("Unknown action {}", action),
        }
    }

    println!("Part 1: {}", pos_x.abs() + pos_y.abs());
}
