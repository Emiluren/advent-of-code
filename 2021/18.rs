use std::fs::File;
use std::io::prelude::*;

enum Snailfish { Pair(Box<Snailfish>, Box<Snailfish>), V(u8) }
use Snailfish::*;

impl std::fmt::Display for Snailfish {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Pair(sn1, sn2) => write!(f, "[{},{}]", *sn1, sn2),
            V(v) => write!(f, "{}", v),
        }
    }
}

impl Snailfish {
    fn explode(&self, depth: u8) -> Option<(Snailfish, Option<u8>, Option<u8>)> {
        match self {
            V(_) => None,
            Pair(sn1, sn2) => {
                if depth > 4 {
                    if let (V(v1), V(v2)) = (*sn1, *sn2) {
                        return Some((V(0), Some(v1), Some(v2)));
                    }
                }

                if let Some((new_left, add_left, add_right)) = sn1.explode(depth+1) {
                    Some((
                        Pair(Box::new(new_left), Box::new(add_deep_left(*sn2, add_right))),
                        add_left,
                        None,
                    ))
                } else if let Some((new_right, add_left, add_right)) = sn2.explode(depth+1) {
                    Some((
                        Pair(Box::new(add_deep_right(*sn1, add_left)), Box::new(new_right)),
                        None,
                        add_right,
                    ))
                } else {
                    None
                }
            }
        }
    }
}

fn parse_snailfish(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Snailfish {
    if *chars.peek().unwrap() == '[' {
        chars.next().unwrap();
        let sn1 = parse_snailfish(chars);
        assert!(chars.next().unwrap() == ',');
        let sn2 = parse_snailfish(chars);
        assert!(chars.next().unwrap() == ']');
        Pair(Box::new(sn1), Box::new(sn2))
    } else {
        V(chars.next().unwrap().to_string().parse().unwrap())
    }
}

fn read_from_input_file() -> std::io::Result<Vec<Snailfish>> {
    let mut file = File::open("18input")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents.lines().map(|line| parse_snailfish(&mut line.chars().peekable())).collect())
}

fn add_deep_left(sf: Snailfish, add: Option<u8>) -> Snailfish {
    match (sf, add) {
        (sf, None) => sf,
        (Pair(sn1, sn2), Some(add)) =>
            Pair(Box::new(add_deep_left(*sn1, Some(add))), sn2),
        (V(v), Some(add)) => V(v + add),
    }
}

fn add_deep_right(sf: Snailfish, add: Option<u8>) -> Snailfish {
    match (sf, add) {
        (sf, None) => sf,
        (Pair(sn1, sn2), Some(add)) =>
            Pair(sn1, Box::new(add_deep_right(*sn2, Some(add)))),
        (V(v), Some(add)) => V(v + add),
    }
}

fn main() -> std::io::Result<()> {
    let input = read_from_input_file()?;
    println!("{}", input[0]);
    Ok(())
}
