use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;

enum Snailfish { Pair(Rc<Snailfish>, Rc<Snailfish>), V(u8) }
use Snailfish::*;

impl std::fmt::Display for Snailfish {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Pair(sn1, sn2) => write!(f, "[{},{}]", *sn1, sn2),
            V(v) => write!(f, "{}", v),
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
        Pair(Rc::new(sn1), Rc::new(sn2))
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

fn add_deep_left(sf: &Snailfish, add: u8) -> Snailfish {
    match sf {
        Pair(sn1, sn2) =>
            Pair(Rc::new(add_deep_left(sn1, add)), sn2.clone()),
        V(v) => V(v + add),
    }
}

fn add_deep_right(sf: &Snailfish, add: u8) -> Snailfish {
    match sf {
        Pair(sn1, sn2) =>
            Pair(sn1.clone(), Rc::new(add_deep_right(sn2, add))),
        V(v) => V(v + add),
    }
}

impl Snailfish {
    fn explode(&self, depth: u8) -> Option<(Snailfish, u8, u8)> {
        match self {
            V(_) => None,
            Pair(sn1, sn2) => {
                if depth > 4 {
                    if let (V(ref v1), V(ref v2)) = (sn1.as_ref(), sn2.as_ref()) {
                        return Some((V(0), *v1, *v2));
                    }
                }

                if let Some((new_left, add_left, add_right)) = sn1.explode(depth+1) {
                    Some((
                        Pair(
                            Rc::new(new_left),
                            Rc::new(add_deep_left(sn2, add_right)),
                        ),
                        add_left,
                        0,
                    ))
                } else if let Some((new_right, add_left, add_right)) = sn2.explode(depth+1) {
                    Some((
                        Pair(
                            Rc::new(add_deep_right(sn1, add_left)),
                            Rc::new(new_right),
                        ),
                        0,
                        add_right,
                    ))
                } else {
                    None
                }
            }
        }
    }
}

fn main() -> std::io::Result<()> {
    let input = read_from_input_file()?;
    println!("{}", input[0]);
    Ok(())
}
