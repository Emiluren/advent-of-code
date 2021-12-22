use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;

#[derive(Clone)]
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

fn parse_snailfish(chars: &mut std::str::Chars<'_>) -> Result<Snailfish, String> {
    match chars.next() {
        Some('[') => {
            let sn1 = parse_snailfish(chars)?;
            if chars.next() != Some(',') {
                return Err("Expected ,".to_string());
            }
            let sn2 = parse_snailfish(chars)?;
            if chars.next() != Some(']') {
                return Err("Expected ]".to_string());
            }
            Ok(Pair(Rc::new(sn1), Rc::new(sn2)))
        }
        Some(c) => Ok(V(c.to_string().parse::<u8>().map_err(|e| e.to_string())?)),
        None => {
            Err("Expected [ or number".to_string())
        }
    }
}

impl std::str::FromStr for Snailfish {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_snailfish(&mut s.chars())
    }
}

fn read_from_input_file() -> std::io::Result<Vec<Snailfish>> {
    let mut file = File::open("18input")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents.lines().map(|line| line.parse().unwrap()).collect())
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

    fn split(&self) -> Option<Snailfish> {
        match self {
            V(v) => {
                if *v >= 10 {
                    Some(Pair(Rc::new(V(v/2)), Rc::new(V(v/2 + v%2))))
                } else {
                    None
                }
            }
            Pair(sn1, sn2) => {
                if let Some(new_sn1) = sn1.split() {
                    Some(Pair(Rc::new(new_sn1), sn2.clone()))
                } else if let Some(new_sn2) = sn2.split() {
                    Some(Pair(sn1.clone(), Rc::new(new_sn2)))
                } else {
                    None
                }
            }
        }
    }

    fn reduce(&self) -> Snailfish {
        let mut fish = self.clone();
        while let Some(newfish) = fish.explode(1).map(|(f, _, _)| f).or_else(|| fish.split()) {
            fish = newfish;
        }
        fish
    }

    fn magnitude(&self) -> u16 {
        match self {
            V(v) => *v as u16,
            Pair(sn1, sn2) => sn1.magnitude()*3 + sn2.magnitude()*2,
        }
    }
}

fn part1(input: &[Snailfish]) -> u16 {
    let mut last_fish: Option<Snailfish> = None;
    for snailfish in input {
        let sf = match last_fish {
            Some(last_fish) => Pair(Rc::new(last_fish), Rc::new(snailfish.clone())),
            None => snailfish.clone(),
        };
        last_fish = Some(sf.reduce());
    }
    last_fish.unwrap().magnitude()
}

fn part2(input: &[Snailfish]) -> u16 {
    input.iter().map(|f1| {
        input.iter().map(|f2| {
            Pair(Rc::new(f1.clone()), Rc::new(f2.clone())).reduce().magnitude()
        }).max().unwrap()
    }).max().unwrap()
}

fn main() -> std::io::Result<()> {
    let input = read_from_input_file()?;

    // let sn = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]".parse::<Snailfish>().unwrap();
    // println!("{}", sn);
    // let sn2 = sn.explode(1).unwrap().0;
    // println!("{}", sn2);
    // let sn3 = sn2.explode(1).unwrap().0;
    // println!("{}", sn3);
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));

    Ok(())
}
