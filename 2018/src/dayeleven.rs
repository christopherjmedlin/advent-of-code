use std::cmp::{max, min};

pub fn day_eleven(input: Vec<String>) -> (String, String) {
    let serial: u32 = input[0].parse().unwrap();
    (part_one(serial), String::new())
}

fn part_one(serial: u32) -> String {
    let mut grid = [[0; 300]; 300];

    for x in 1..301 {
        for y in 1..301 {
            let rack_id = x + 10;
            let mut power: i32 = rack_id * y;
            power += serial as i32;
            power *= rack_id;
            
            let digits: Vec<char> = power.to_string()
                              .chars()
                              .collect();
            power = digits[digits.len() - 3].to_string().parse().unwrap();
            power -= 5;

            grid[(y - 1) as usize][(x - 1) as usize] = power;
        }
    }
    
    let mut highest = 0;
    let mut highest_coords = (0, 0, 0);
    for x in 0..300 {
        for y in 0..300 {
            let mut total_power = 0;
            let bound = min(301 - x, 301 - y);
            
            for size in 1..bound {
                for x2 in x..x + size {
                    total_power += grid[y + size-1][x2 as usize];
                }

                for y2 in y..y + size-1 {
                    total_power += grid[y2 as usize][x + size-1];
                }
                
                if total_power > highest {
                    highest = total_power;
                    highest_coords.0 = x+1;
                    highest_coords.1 = y+1;
                    highest_coords.2 = size;
                }
            }
        }
    }

    format!("x: {}, y: {}, s: {}", highest_coords.0, highest_coords.1, highest_coords.2)
}
