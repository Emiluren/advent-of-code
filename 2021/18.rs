use std::fs::File;
use std::io::prelude::*;

#[derive(Copy, Clone)]
enum Snailfish { Pair(usize, usize), V(u8) }
use Snailfish::*;

fn parse_snailfish(chars: &mut std::str::Chars<'_>, fishes: &mut Vec<Snailfish>) -> Result<usize, String> {
    match chars.next() {
        Some('[') => {
            let sn1 = parse_snailfish(chars, fishes)?;
            if chars.next() != Some(',') {
                return Err("Expected ,".to_string());
            }
            let sn2 = parse_snailfish(chars, fishes)?;
            if chars.next() != Some(']') {
                return Err("Expected ]".to_string());
            }
            fishes.push(Pair(sn1, sn2));
        }
        Some(c) => {
            fishes.push(V(c.to_string().parse::<u8>().map_err(|e| e.to_string())?));
        }
        None => {
            return Err("Expected [ or number".to_string());
        }
    }
    Ok(fishes.len() - 1)
}

fn read_from_input_file() -> std::io::Result<(Vec<usize>, Vec<Snailfish>)> {
    let mut file = File::open("18input")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut fishes = Vec::new();
    let mut top = Vec::new();
    for line in contents.lines() {
        top.push(parse_snailfish(&mut line.chars(), &mut fishes).unwrap());
    }
    Ok((top, fishes))
}

fn add_deep_left(sf: Snailfish, add: u8, fishes: &mut Vec<Snailfish>) -> usize {
    let new_fish = match sf {
        Pair(sn1, sn2) =>
            Pair(add_deep_left(fishes[sn1], add, fishes), sn2),
        V(v) => V(v + add),
    };
    fishes.push(new_fish);
    fishes.len() - 1
}

fn add_deep_right(sf: Snailfish, add: u8, fishes: &mut Vec<Snailfish>) -> usize {
    let new_fish = match sf {
        Pair(sn1, sn2) =>
            Pair(sn1, add_deep_right(fishes[sn2], add, fishes)),
        V(v) => V(v + add),
    };
    fishes.push(new_fish);
    fishes.len() - 1
}

impl Snailfish {
    fn explode(self, depth: u8, fishes: &mut Vec<Snailfish>) -> Option<(usize, u8, u8)> {
        match self {
            V(_) => None,
            Pair(sn1, sn2) => {
                if depth > 4 {
                    if let (V(v1), V(v2)) = (fishes[sn1], fishes[sn2]) {
                        fishes.push(V(0));
                        return Some((fishes.len() - 1, v1, v2));
                    }
                }

                if let Some((new_left, add_left, add_right)) = fishes[sn1].explode(depth+1, fishes) {
                    let new_fish = Pair(new_left, add_deep_left(fishes[sn2], add_right, fishes));
                    fishes.push(new_fish);
                    Some((fishes.len() - 1, add_left, 0))
                } else if let Some((new_right, add_left, add_right)) = fishes[sn2].explode(depth+1, fishes) {
                    let new_fish = Pair(add_deep_right(fishes[sn1], add_left, fishes), new_right);
                    fishes.push(new_fish);
                    Some((fishes.len() - 1, 0, add_right))
                } else {
                    None
                }
            }
        }
    }

    fn split(self, fishes: &mut Vec<Snailfish>) -> Option<usize> {
        match self {
            V(v) => {
                if v >= 10 {
                    let sn1 = fishes.len();
                    fishes.push(V(v/2));
                    let sn2 = fishes.len();
                    fishes.push(V(v/2 + v%2));
                    fishes.push(Pair(sn1, sn2));
                    Some(fishes.len() - 1)
                } else {
                    None
                }
            }
            Pair(sn1, sn2) => {
                if let Some(new_sn1) = fishes[sn1].split(fishes) {
                    fishes.push(Pair(new_sn1, sn2));
                    Some(fishes.len() - 1)
                } else if let Some(new_sn2) = fishes[sn2].split(fishes) {
                    fishes.push(Pair(sn1, new_sn2));
                    Some(fishes.len() - 1)
                } else {
                    None
                }
            }
        }
    }

    fn reduce(mut i: usize, fishes: &mut Vec<Snailfish>) -> usize {
        let mut fish = fishes[i];
        while let Some(new_i) = fish.explode(1, fishes).map(|(f, _, _)| f).or_else(|| fish.split(fishes)) {
            i = new_i;
            fish = fishes[i];
        }
        i
    }

    fn magnitude(self, fishes: &mut Vec<Snailfish>) -> u16 {
        match self {
            V(v) => v as u16,
            Pair(sn1, sn2) => fishes[sn1].magnitude(fishes)*3 + fishes[sn2].magnitude(fishes)*2,
        }
    }
}

fn part1(top: &[usize], fishes: &mut Vec<Snailfish>) -> u16 {
    let mut last_fish: Option<usize> = None;
    for i in top {
        let sf = match last_fish {
            Some(last_fish) => {
                fishes.push(Pair(last_fish, *i));
                fishes.len() - 1
            },
            None => *i,
        };
        last_fish = Some(Snailfish::reduce(sf, fishes));
    }
    fishes[last_fish.unwrap()].magnitude(fishes)
}

fn part2(top: &[usize], fishes: &mut Vec<Snailfish>) -> u16 {
    top.iter().map(|f1| {
        top.iter().map(|f2| {
            fishes.push(Pair(*f1, *f2));
            let reduced = Snailfish::reduce(fishes.len() - 1, fishes);
            fishes[reduced].magnitude(fishes)
        }).max().unwrap()
    }).max().unwrap()
}

fn main() -> std::io::Result<()> {
    let (top, mut fishes) = read_from_input_file()?;

    println!("Part 1: {}", part1(&top, &mut fishes));
    println!("Part 2: {}", part2(&top, &mut fishes));

    Ok(())
}
