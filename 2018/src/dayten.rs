use regex::Regex;
use std::cmp::{max, min};
use std::fs::File;
use std::io::Write;

pub fn day_ten(input: Vec<String>) -> (String, String) {
    let re = Regex::new("<(.{6}), (.{6})>.{11}(.{2}), (.{2})>").unwrap();
    let mut points: Vec<Point> = Vec::with_capacity(input.len());

    for line in input.iter() {
        let captures = re.captures(line).unwrap();
        
        let x: i32 = captures.get(1).unwrap().as_str()
                             .replace(" ", "0")
                             .parse().unwrap();
        let y: i32 = captures.get(2).unwrap().as_str()
                             .replace(" ", "0")
                             .parse().unwrap();
        let x_vel: i32 = captures.get(3).unwrap().as_str()
                             .replace(" ", "0")
                             .parse().unwrap();
        let y_vel: i32 = captures.get(4).unwrap().as_str()
                             .replace(" ", "0")
                             .parse().unwrap();
        points.push(Point {
            x: x,
            y: y,
            x_vel: x_vel,
            y_vel: y_vel
        });
    }

    File::create("output.txt");

    let part_two = stars(points).to_string();

    (String::from("Answer above"), part_two)
}

#[derive(Clone)]
struct Point {
    x: i32,
    y: i32,
    x_vel: i32,
    y_vel: i32
}

fn stars(mut input: Vec<Point>) -> u32 {
    let mut point_graph = [['.'; 90]; 15];
    let mut found = false;
    let mut seconds: u32 = 0;

    while !found {
        let mut bound_left = 1000;
        let mut bound_right = -1000;
        let mut bound_up = -1000;
        let mut bound_down = 1000;

        for point in input.iter_mut() {
            point.x += point.x_vel;
            point.y += point.y_vel;

            bound_left = min(bound_left, point.x);
            bound_right = max(bound_right, point.x);
            bound_down = min(bound_down, point.y);
            bound_up = max(bound_up, point.y);
        }
        
        if bound_right - bound_left > 90 ||
           bound_up - bound_down > 15 {
            if found {break;}
        } else {
            found = true;
            
            for point in input.iter() {
                point_graph[(bound_up - point.y) as usize][(bound_right - point.x) as usize] = '#';
            }
            
            for row in point_graph.iter() {
                let string: String = row.iter().collect();
                println!("{}", string);
            }
        }

        seconds += 1;
    }

    seconds
}
