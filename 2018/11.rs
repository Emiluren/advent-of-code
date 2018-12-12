fn main() {
    let input = 9424;
    let mut data = [[0; 300]; 300];

    for x in 1..300 {
        for y in 1..300 {
            let rack_id = x as i32 + 10;
            let power = (y as i32 * rack_id + input) * rack_id;
            data[x - 1][y - 1] = power / 100 % 10 - 5;
        }
    }

    let mut largest_coord = (0, 0);
    let mut largest_sum = -5 * 9;
    for x in 0..298 {
        for y in 0..298 {
            let mut sum = 0;
            for dx in 0..3 {
                for dy in 0..3 {
                    sum += data[x + dx][y + dy];
                }
            }

            if sum > largest_sum {
                largest_sum = sum;
                largest_coord = (x + 1, y + 1);
            }
        }
    }

    println!("Part 1: {:?}", largest_coord);

    let mut summed_areas = [[0; 300]; 300];
    for x in 0..300 {
        for y in 0..300 {
            summed_areas[x][y] =
                data[x][y] +
                if x == 0 { 0 } else { summed_areas[x - 1][y] } +
                if y == 0 { 0 } else { summed_areas[x][y - 1] } -
                if x == 0 || y == 0 { 0 } else { summed_areas[x - 1][y - 1] };
        }
    }

    let mut largest_sum = -5 * 300 * 300;
    let mut largest_x_y_size = (0, 0, 0);
    for size in 1 ..= 300 {
        for x in 0 ..= (300 - size) {
            for y in 0 ..= (300 - size) {
                let sum = area_sum(x, y, size, &summed_areas);
                if sum > largest_sum {
                    largest_sum = sum;
                    largest_x_y_size = (x + 1, y + 1, size);
                }
            }
        }
    }
    println!("Part 2: {:?} (power {})", largest_x_y_size, largest_sum);
}

fn print_area(x: usize, y: usize, size: usize, data: &[[i32; 300]]) {
    for dy in 0..size {
        for dx in 0..size {
            print!("{: <5} ", data[x + dx][y + dy]);
        }
        println!();
    }
}

fn area_sum(x: usize, y: usize, size: usize, data: &[[i32; 300]]) -> i32 {
    let xmax = x + size - 1;
    let ymax = y + size - 1;

    let bottom_left = if x == 0 { 0 } else { data[x - 1][ymax] };
    let top_right = if y == 0 { 0 } else { data[xmax][y - 1] };
    let top_left = if x == 0 || y == 0 { 0 } else { data[x - 1][y - 1] };

    let sum = top_left + data[xmax][ymax] - top_right - bottom_left;
    sum
}
