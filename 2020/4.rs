use std::collections::HashMap;
use std::fs;

fn main() {
    let contents = fs::read_to_string("4input").unwrap();
    let passports: Vec<Vec<&str>> = contents.split("\n\n").map(|s| s.split_ascii_whitespace().collect()).collect();

    let required_fields = ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"];
    let north_pole_creds = ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"];

    let eye_colors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];

    let mut valid_count1 = 0;
    let mut valid_count2 = 0;
    for pass in &passports {
        let pass_map: HashMap<_, _> = pass.iter().map(|field| {
            let mut split_field = field.split(':');
            let field_name = split_field.next().unwrap();
            let field_value = split_field.next().unwrap();
            (field_name, field_value)
        }).collect();

        let mut fields: Vec<_> = pass_map.keys().cloned().collect();
        fields.sort();

        if fields == north_pole_creds || fields == required_fields {
            valid_count1 += 1;

            let mut invalid_field = false;
            for (key, val) in pass_map {
                match key {
                    "byr" => {
                        let year: usize = val.parse().unwrap();
                        if year < 1920 || year > 2002 {
                            invalid_field = true;
                            break;
                        }
                    }
                    "iyr" => {
                        let year: usize = val.parse().unwrap();
                        if year < 2010 || year > 2020 {
                            invalid_field = true;
                            break;
                        }
                    }
                    "eyr" => {
                        let year: usize = val.parse().unwrap();
                        if year < 2020 || year > 2030 {
                            invalid_field = true;
                            break;
                        }
                    }
                    "hcl" => {
                        if val.chars().nth(0) != Some('#') {
                            invalid_field = true;
                            break;
                        }

                        let color = val.chars().skip(1);
                        if color.clone().count() != 6 || !color.clone().all(|c| c.is_digit(16)) {
                            invalid_field = true;
                            break;
                        }
                    }
                    "hgt" => {
                        let mut digits = String::new();
                        let mut chars = val.chars().peekable();

                        while chars.peek().map(|d| d.is_digit(10)).unwrap_or(false) {
                            digits.push(chars.next().unwrap());
                        }

                        let height = digits.parse().unwrap_or(0);
                        let unit: String = chars.collect();
                        match unit.as_str() {
                            "cm" => {
                                if height < 150 || height > 193 {
                                    invalid_field = true;
                                    break;
                                }
                            }
                            "in" => {
                                if height < 59 || height > 76 {
                                    invalid_field = true;
                                    break;
                                }
                            }
                            _ => {
                                invalid_field = true;
                                break;
                            }
                        }
                    }
                    "ecl" => {
                        if !eye_colors.contains(&val) {
                            invalid_field = true;
                            break;
                        }
                    }
                    "pid" => {
                        if val.chars().count() != 9 || !val.chars().all(|c| c.is_digit(10)) {
                            invalid_field = true;
                            break;
                        }
                    }
                    "cid" => {}
                    _ => {
                        panic!("Unknown field {}:{}", key, val)
                    }
                }
            }

            if !invalid_field {
                valid_count2 += 1;
            }
        }
    }

    println!("Part 1: {}", valid_count1);
    println!("Part 2: {}", valid_count2);
}
