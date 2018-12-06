use std::collections::HashSet;

fn main() {
    part_one();
    part_two();
}

fn part_one() {
    let mut point_amount = [0; 50];
    let mut infinite_areas = HashSet::new();

    for yi in 0..1200 {
        let y = yi - 400;
        for xi in 0..1200 {
            let x = xi - 400;
            let mut closest_point = 99;
            let mut closest_distance = 400 * 400;

            for i in 0..50 {
                let (px, py) = INPUT[i];
                let dist = (px - x).abs() + (py - y).abs();
                if dist < closest_distance {
                    closest_point = i;
                    closest_distance = dist;
                } else if dist == closest_distance {
                    closest_point = 99;
                }
            }

            if closest_point != 99 {
                point_amount[closest_point] += 1;

                if yi == 0 || yi == 1199 || xi == 0 || xi == 1199 {
                    infinite_areas.insert(closest_point);
                }
            }
        }
    }

    let biggest = (0..50)
        .filter(|i| !infinite_areas.contains(&i))
        .fold(0, |current_max, i| current_max.max(point_amount[i]));

    println!("Part 1: {}", biggest);
}

fn part_two() {
    let mut point_amount = 0;

    for yi in 0..1200 {
        let y = yi - 400;
        for xi in 0..1200 {
            let x = xi - 400;
            let mut dist_sum = 0;

            for i in 0..50 {
                let (px, py) = INPUT[i];
                let dist = (px - x).abs() + (py - y).abs();
                dist_sum += dist;
            }

            if dist_sum < 10000 {
                point_amount += 1;
            }
        }
    }

    println!("Part 2: {}", point_amount);
}

const INPUT: [(i32, i32); 50] = [
    (61, 90),
    (62, 191),
    (68, 201),
    (72, 316),
    (73, 244),
    (77, 117),
    (84, 121),
    (94, 355),
    (95, 285),
    (106, 46),
    (121, 290),
    (123, 63),
    (131, 188),
    (141, 193),
    (170, 60),
    (172, 202),
    (173, 154),
    (177, 97),
    (199, 205),
    (205, 47),
    (218, 44),
    (229, 262),
    (235, 312),
    (259, 82),
    (259, 97),
    (261, 198),
    (270, 136),
    (270, 318),
    (273, 103),
    (273, 120),
    (278, 288),
    (286, 172),
    (289, 130),
    (289, 268),
    (300, 279),
    (302, 247),
    (306, 239),
    (308, 333),
    (309, 234),
    (313, 297),
    (315, 128),
    (316, 95),
    (322, 69),
    (337, 351),
    (338, 149),
    (338, 57),
    (340, 75),
    (349, 77),
    (355, 94),
    (358, 66),
];
