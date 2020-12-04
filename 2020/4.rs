use std::fs;

fn main() {
    let contents = fs::read_to_string("4input").unwrap();
    let passports: Vec<Vec<&str>> = contents.split("\n\n").map(|s| s.split_ascii_whitespace().collect()).collect();

    let required_fields = ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"];
    let north_pole_creds = ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"];

    let mut valid_count = 0;
    for pass in &passports {
        let mut fields: Vec<_> = pass.iter().map(|field| {
            let mut split_field = field.split(':');
            let field_name = split_field.next().unwrap();
            field_name
        }).collect();
        fields.sort();

        if fields == north_pole_creds || fields == required_fields {
            valid_count += 1;
        }
    }

    println!("Part 1: {}", valid_count);
}
