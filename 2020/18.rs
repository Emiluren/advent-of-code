use std::collections::VecDeque;

fn main() {
    let input = std::fs::read_to_string("18input").unwrap();

    let mut sum1 = 0;
    let mut sum2 = 0;
    for line in input.lines() {
        sum1 += eval_rpn(to_rpn(line.chars(), false));
        sum2 += eval_rpn(to_rpn(line.chars(), true));
    }
    println!("Part 1: {}", sum1);
    println!("Part 2: {}", sum2);
}

// Using shunting-yard algorithm (https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
fn to_rpn(chars: impl Iterator<Item = char>, part2: bool) -> impl Iterator<Item = char> {
    let mut output_queue = VecDeque::new();
    let mut operator_stack = Vec::new();
    for c in chars {
        if c.is_digit(10) {
            output_queue.push_back(c);
        } else if c == '+' || c == '*' {
            while let Some(op) = operator_stack.last().copied() {
                if op == '+' || op == '*' && (!part2 || c == '*') {
                    output_queue.push_back(op);
                    operator_stack.pop().unwrap();
                } else {
                    break;
                }
            }
            operator_stack.push(c)
        } else if c == '(' {
            operator_stack.push(c);
        } else if c == ')' {
            while let Some(op) = operator_stack.last().copied() {
                if op != '(' {
                    output_queue.push_back(op);
                    operator_stack.pop().unwrap();
                } else {
                    break;
                }
            }
            if operator_stack.last().copied() != Some('(') {
                panic!("Unbalanced parentheses");
            } else {
                operator_stack.pop().unwrap();
            }
        } else {
            panic!("Unknown token: {}", c);
        }
    }
    while let Some(op) = operator_stack.pop() {
        output_queue.push_back(op);
    }
    output_queue.into_iter()
}

fn eval_rpn(chars: impl Iterator<Item = char>) -> u64 {
    let mut value_stack = Vec::new();
    for c in chars {
        if let Some(d) = c.to_digit(10) {
            value_stack.push(d as u64)
        } else if c == '+' {
            let v1 = value_stack.pop().unwrap();
            let v2 = value_stack.pop().unwrap();
            value_stack.push(v1 + v2);
        } else {
            assert_eq!(c, '*');
            let v1 = value_stack.pop().unwrap();
            let v2 = value_stack.pop().unwrap();
            value_stack.push(v1 * v2);
        }
    }
    value_stack.pop().unwrap()
}
