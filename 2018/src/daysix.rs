use std::collections::HashMap;

pub fn day_six(input: Vec<String>) -> (String, String) {
    (part_one(&input), part_two(&input))
}

struct Area {
    point: (u16, u16),
    area: u32,
    infinite: bool
}

struct Bounds {
    max_x: u16,
    max_y: u16,
    min_x: u16,
    min_y: u16
}

fn get_bounds(areas: &Vec<Area>) -> Bounds {
    let mut bounds = Bounds {
        max_x: 0,
        max_y: 0,
        min_x: 500,
        min_y: 500,
    };
    for area in areas.iter() {
        let x = area.point.0;
        let y = area.point.1;

        if x > bounds.max_x {
            bounds.max_x = x;
        } else if x < bounds.min_x {
            bounds.min_x = x;
        }

        if y > bounds.max_y {
            bounds.max_y = y;
        } else if y < bounds.min_y {
            bounds.min_y = y;
        }
    }
    bounds
}

fn manhattan_check(bounds: &Bounds, areas: &mut Vec<Area>, infinite: bool) {
    for x in bounds.min_x..bounds.max_x + 1 {
        for y in bounds.min_y..bounds.max_y + 1 {
            let mut smallest_distance = 999;
            let mut smallest_distance_area_index = 0;
            // set whenever a duplicate manhattan distance is found
            let mut duplicate = 999;

            for i in 0..areas.len() {
                let area = &areas[i];
                let dist = (area.point.0 as i32 - x as i32).abs()
                         + (area.point.1 as i32 - y as i32).abs();
                if dist < smallest_distance {
                    smallest_distance = dist;
                    smallest_distance_area_index = i;
                } else if dist == smallest_distance {
                    duplicate = dist;;
                }
            }

            if duplicate != smallest_distance {
                areas[smallest_distance_area_index].area += 1;
                areas[smallest_distance_area_index].infinite = infinite;
            }
        } 
    } 
}

fn parse_input(input: &Vec<String>) -> Vec<Area> {
    let mut areas: Vec<Area> = Vec::new();

    for line in input.iter() {
        let mut split: Vec<&str> = line.split(", ").collect();

        let point = (split[0].parse().unwrap(),
                 split[1].parse().unwrap());
        areas.push(Area {
            point: point,
            area: 0,
            infinite: false
        })
    }

    areas
}

fn part_one(input: &Vec<String>) -> String {
    let mut areas = parse_input(input);
    
    let bounds = get_bounds(&areas);
    
    manhattan_check(&bounds, &mut areas, false);
    manhattan_check(&Bounds{
        max_x: bounds.max_x + 1,
        min_x: bounds.max_x + 1,
        max_y: bounds.max_y + 1,
        min_y: bounds.min_y - 1,
    }, &mut areas, true);
    manhattan_check(&Bounds{
        max_x: bounds.min_x - 1,
        min_x: bounds.min_x - 1,
        max_y: bounds.max_y + 1,
        min_y: bounds.min_y - 1,
    }, &mut areas, true);
    manhattan_check(&Bounds{
        max_x: bounds.max_x + 1,
        min_x: bounds.min_x - 1,
        max_y: bounds.max_y + 1,
        min_y: bounds.max_y + 1,
    }, &mut areas, true);
    manhattan_check(&Bounds{
        max_x: bounds.max_x + 1,
        min_x: bounds.min_x - 1,
        max_y: bounds.min_y - 1,
        min_y: bounds.min_y - 1,
    }, &mut areas, true);
    
    let mut largest_area = 0;
    for area in areas.iter() {
        if area.area > largest_area && !area.infinite {
            largest_area = area.area;
        }
        println!("{}: {}", area.infinite, area.area);
    }

    largest_area.to_string()
}

fn part_two(input: &Vec<String>) -> String {
    let mut areas = &parse_input(input);
    let bounds = get_bounds(&areas);
    
    let mut region = 0;
    for x in bounds.min_x..bounds.max_x + 1 {
        for y in bounds.min_y..bounds.max_y + 1 {
            let mut distance_sum = 0;
            
            for area in areas {
                distance_sum += (area.point.0 as i32 - x as i32).abs()
                             + (area.point.1 as i32 - y as i32).abs();
            }

            if distance_sum < 10000 {
                region += 1;
            }
        }
    }
    
    region.to_string()
}
