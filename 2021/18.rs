use std::fs::File;
use std::io::prelude::*;

enum Snailfish { Pair(Box<(Snailfish, Snailfish)>), V(u8) }
use Snailfish::*;

enum Diff<C, S> { Changed(C), Same(S) }

impl<C, S> Diff<C, S> {
    fn try_if_same(self, f: impl Fn(S) -> Diff<C, S>) -> Diff<C, S> {
        match self {
            Diff::Changed(s) => Diff::Changed(s),
            Diff::Same(s) => f(s),
        }
    }

    fn map_changed(self, f: impl Fn(C) -> C) -> Diff<C, S> {
        match self {
            Diff::Changed(s) => Diff::Changed(f(s)),
            Diff::Same(s) => Diff::Same(s),
        }
    }
}

// struct ConsFn {
//     memory: &'a mut [(Snailfish, Snailfish)],
//     current: usize,
// }

// impl ConsFn {
//     fn new(memory: &'a mut [(Snailfish, Snailfish)]) -> ConsFn {
//         ConsFn { memory, current: 0 }
//     }

//     fn cons(&'a mut self, sn1: Snailfish, sn2: Snailfish) -> Snailfish {
//         let new = &mut self.memory[self.current];
//         *new = (sn1, sn2);
//         self.current += 1;
//         Pair(new)
//     }
// }

impl std::fmt::Display for Snailfish {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Pair(b) => write!(f, "[{},{}]", b.0, b.1),
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
            Ok(Pair(Box::new((sn1, sn2))))
        }
        Some(c) => Ok(V(c.to_string().parse::<u8>().map_err(|e| e.to_string())?)),
        None => {
            Err("Expected [ or number".to_string())
        }
    }
}

// impl std::str::FromStr for Snailfish {
//     type Err = String;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         parse_snailfish(&mut s.chars())
//     }
// }

fn read_from_input_file() -> std::io::Result<Vec<Snailfish>> {
    let mut file = File::open("18input")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut input = Vec::new();
    for line in contents.lines() {
        input.push(parse_snailfish(&mut line.chars()).unwrap());
    }
    Ok(input)
}

fn add_deep_left(sf: Snailfish, add: u8) -> Snailfish {
    if add == 0 {
        return sf;
    }
    match sf {
        Pair(b) =>
            Pair(Box::new((add_deep_left(b.0, add), b.1))),
        V(v) => V(v + add),
    }
}

fn add_deep_right(sf: Snailfish, add: u8) -> Snailfish {
    if add == 0 {
        return sf;
    }
    match sf {
        Pair(b) =>
            Pair(Box::new((b.0, add_deep_right(b.1, add)))),
        V(v) => V(v + add),
    }
}

impl Snailfish {
    fn explode(self, depth: u8) -> Diff<(Snailfish, u8, u8), Snailfish> {
        match self {
            V(_) => Diff::Same(self),
            Pair(b) => {
                if depth > 4 {
                    if let (V(ref v1), V(ref v2)) = b.as_ref() {
                        return Diff::Changed((V(0), *v1, *v2));
                    }
                }

                match b.0.explode(depth+1) {
                    Diff::Changed((new_left, add_left, add_right)) => (
                        Diff::Changed((Pair(Box::new((
                            new_left,
                            add_deep_left(b.1, add_right),
                        ))),
                                       add_left,
                                       0,
                        ))),
                    Diff::Same(b0) => match b.1.explode(depth+1) {
                        Diff::Changed((new_right, add_left, add_right)) => (
                            Diff::Changed((Pair(Box::new((
                                add_deep_right(b0, add_left),
                                new_right,
                            ))),
                                           0,
                                           add_right,
                            ))),
                        Diff::Same(s) => Diff::Same(s),
                    }
                }
            }
        }
    }

    fn split(self) -> Diff<Snailfish, Snailfish> {
        match self {
            V(v) => {
                if v >= 10 {
                    Diff::Changed(Pair(Box::new((V(v/2), V(v/2 + v%2)))))
                } else {
                    Diff::Same(V(v))
                }
            }
            Pair(b) => {
                if let Diff::Changed(new_sn1) = b.0.split() {
                    Diff::Changed(Pair(Box::new((new_sn1, b.1))))
                } else if let Diff::Changed(new_sn2) = b.1.split() {
                    Diff::Changed(Pair(Box::new((b.0, new_sn2))))
                } else {
                    Diff::Same(Pair(b))
                }
            }
        }
    }

    fn reduce(self) -> Snailfish {
        let mut fish = self;
        loop {
            match fish.explode(1) {
                Diff::Changed((f, _, _)) => fish = f,
                Diff::Same(f) => match f.split() {
                    Diff::Changed(f) => fish = f,
                    Diff::Same(f) => return f,
                }
            }
        }
    }

    fn magnitude(self) -> u16 {
        match self {
            V(v) => v as u16,
            Pair(b) => b.0.magnitude()*3 + b.1.magnitude()*2,
        }
    }
}

fn part1(input: &[Snailfish]) -> u16 {
    let mut last_fish: Option<Snailfish> = None;
    for snailfish in input {
        let sf = match last_fish {
            Some(last_fish) => Pair(Box::new((last_fish, *snailfish))),
            None => *snailfish,
        };
        last_fish = Some(sf.reduce());
    }
    last_fish.unwrap().magnitude()
}

fn part2(input: &[Snailfish]) -> u16 {
    let mut max_v = None;
    for f1 in input {
        for f2 in input {
            let new = Pair(Box::new((*f1, *f2))).reduce().magnitude();
            max_v = max_v.map(|v: u16| v.max(new)).or(Some(new));
        }
    }
    max_v.unwrap()
}

fn main() -> std::io::Result<()> {
    //let mut memory: Vec<_> = (1..100_000).map(|_| (V(0), V(0))).collect();
    //let mut cons = ConsFn::new(&mut memory);

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
