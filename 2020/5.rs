use std::fs;

fn main() {
    let contents = fs::read_to_string("5input").unwrap();

    let mut ids = Vec::new();
    for line in contents.lines() {
        let id_string = line
            .replace("F", "0")
            .replace("B", "1")
            .replace("L", "0")
            .replace("R", "1");
        let id = usize::from_str_radix(&id_string, 2).unwrap();
        ids.push(id);
    }
    ids.sort();

    println!("Part 1: {}", ids[ids.len() - 1]);

    let mut last_id = ids[0] - 1;
    for id in ids {
        if id != last_id + 1 {
            println!("Part 2: {}", last_id + 1);
            break;
        }
        last_id = id;
    }
}
