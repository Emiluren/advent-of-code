enum Expr {
    Num(u64),
    Add,
    Mul,
    Paren(Vec<Expr>)
}
use Expr::*;

fn main() {
    let input = std::fs::read_to_string("18input").unwrap();

    let mut sum1 = 0;
    for line in input.lines() {
        let mut chars = line.chars();
        let expr = parse(&mut chars);
        sum1 += eval_1(expr);
    }
    println!("Part 1: {}", sum1);
}

fn parse(chars: &mut impl Iterator<Item = char>) -> Vec<Expr> {
    let mut expressions = Vec::new();
    while let Some(c) = chars.next() {
        match c {
            ')' => {
                break;
            }
            '+' => {
                expressions.push(Add)
            }
            '*' => {
                expressions.push(Mul)
            }
            '(' => {
                expressions.push(Paren(parse(chars)));
            }
            _ => {
                expressions.push(Num(c.to_digit(10).unwrap() as u64));
            }
        }
    }
    expressions
}

fn eval_1(exprs: impl IntoIterator<Item = Expr>) -> u64 {
    let mut value = None;
    let mut op = None;
    for e in exprs {
        let mut new_v = None;
        match e {
            Add => {
                assert!(value != None);
                op = Some((|a,b| a+b) as fn(u64, u64) -> u64);
            }
            Mul => {
                assert!(value != None);
                op = Some((|a,b| a*b) as fn(u64, u64) -> u64);
            }
            Num(n) => {
                new_v = Some(n);
            }
            Paren(es) => {
                new_v = Some(eval_1(es));
            }
        }

        if let Some(v) = new_v {
            if let Some(old_v) = value {
                value = Some(op.unwrap()(v, old_v));
            } else {
                value = Some(v);
            }
            op = None;
        }
    }
    value.unwrap()
}
