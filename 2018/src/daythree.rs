
pub fn day_three(input: Vec<String>) -> (String, String) {
    (part_one(&input), part_two(&input))
}

fn part_one(input: &Vec<String>) -> String {
    let mut map = [[0; 1200]; 1200];

    for line in input.iter() {
        let mut split: Vec<&str> = line.split(" @ ").collect();
        let line = split[1];
        split = line.split(": ").collect();
        let split2: Vec<&str> = split[0].split(",").collect();
        let split3: Vec<&str> = split[1].split("x").collect();

        let x: u32 = split2[0].parse().unwrap();
        let y: u32 = split2[1].parse().unwrap();
        let width: u32 = split3[0].parse().unwrap();
        let height: u32 = split3[1].parse().unwrap();
        
        for i in x..x+width {
            for j in y..y+height {
                map[j as usize][i as usize] += 1;
            }
        }
    }
    
    let mut overlaps = 0;
    for arr in map.iter() {
        for num in arr.iter() {
            if num > &1 {
                overlaps += 1;
            }
        }
    }
    
    overlaps.to_string()
}

fn part_two(input: &Vec<String>) -> String {
    let mut map = [[0; 1200]; 1200];
    let mut tiles: Vec<(usize, usize, usize, usize, usize)> = Vec::new();
    
    // first, do the string parsing nonsense so we only have to do it once
    for line in input.iter() {
        let mut split: Vec<&str> = line.split(" @ ").collect();
        let id: usize = split[0][1..].parse().unwrap();
        let line = split[1];
        split = line.split(": ").collect();
        let split2: Vec<&str> = split[0].split(",").collect();
        let split3: Vec<&str> = split[1].split("x").collect();

        let x: usize = split2[0].parse().unwrap();
        let y: usize = split2[1].parse().unwrap();
        let width: usize = split3[0].parse().unwrap();
        let height: usize = split3[1].parse().unwrap();
        
        tiles.push((id, x, y, width, height));
    }
    
    // next, do the drawing of the claims
    for &(id, x, y, width, height) in tiles.iter() {
        for i in x..x+width {
            for j in y..y+height {
                map[j as usize][i as usize] += 1;
            }
        }
    }
    
    // finally, search for unoverlapped claim
    let mut result = 0;
    let mut result_found: bool;
    for &(id, x, y, width, height) in tiles.iter() {
        result = 0;
        result_found = true;
        for i in x..x+width {
            for j in y..y+height {
                if map[j as usize][i as usize] > 1 {
                    result_found = false;
                    break;
                };
            }
        }

        if result_found {
            result = id;
            break;
        }
    }

    result.to_string()
}
