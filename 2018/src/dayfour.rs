use regex::Regex;
use chrono::prelude::*;
use std::collections::HashMap;

pub fn day_four(input: Vec<String>) -> (String, String) {
    let mut sorted_input = input.clone();
    sorted_input.sort();

    (part_one(&sorted_input), part_two(&sorted_input))
}

fn generate_sleep_hash_map(input: &Vec<String>) -> HashMap<u32, [u8; 59]> {
    let re = Regex::new(r"\[\d{4}-(\d{2})-(\d{2}) (\d{2}):(\d{2})] ([A-z]*) ([#-z]*)")
                    .unwrap();
    let mut sleep_map: HashMap<u32, [u8; 59]> = HashMap::new();

    let mut current_guard = 0;
    let mut last_sleep: u8 = 0;
    for line in input.iter() {
        let captures = re.captures(line).unwrap();
        
        let record_type = captures.get(5).unwrap().as_str();
        let minute: u8 = captures.get(4).unwrap().as_str().parse().unwrap();

        match record_type {
            "Guard" => {
                current_guard = captures.get(6)
                                        .unwrap()
                                        .as_str()[1..]
                                        .parse()
                                        .unwrap();
                if !sleep_map.contains_key(&current_guard) {
                    sleep_map.insert(current_guard, [0; 59]);
                }
            },
            "falls" => {
                last_sleep = minute;
            },
            "wakes" => {
                let sleep_arr = sleep_map.get_mut(&current_guard).unwrap();
                for i in last_sleep..minute {
                    sleep_arr[i as usize] += 1;
                }
            }
            &_ => {panic!("Invalid input");}
        }
    }
    
    sleep_map
}

fn part_one(input: &Vec<String>) -> String {
    let sleep_map = generate_sleep_hash_map(input);

    let mut highest_sleep_count = 0;
    let mut highest_sleep_guard: u32 = 0;
    for key in sleep_map.keys() {
        let mut total_sleep: u64 = 0;
        
        for minute in sleep_map.get(key).unwrap().iter() {
            total_sleep += *minute as u64;
        }
        if total_sleep > highest_sleep_count {
            highest_sleep_guard = *key;
            highest_sleep_count = total_sleep;
        }
    }
    
    let mut highest_minute_value: u8 = 0;
    let mut highest_minute: usize = 0;
    for (i, minute) in sleep_map.get(&highest_sleep_guard).unwrap().iter().enumerate() {
        if *minute > highest_minute_value {
            highest_minute = i;
            highest_minute_value = *minute;
        }
    }

    (highest_sleep_guard * highest_minute as u32).to_string()
}

fn part_two(input: &Vec<String>) -> String {
    let sleep_map = generate_sleep_hash_map(input);
    
    let mut largest_value = 0;
    let mut largest_minute: usize = 0;
    let mut largest_guard: u32 = 0;
    for guard in sleep_map.keys() {
        for (minute, value) in sleep_map.get(guard).unwrap().iter().enumerate() {
            if *value >= largest_value {
                largest_value = *value;
                largest_minute = minute;
                largest_guard = *guard;
            }
        }
    }

    (largest_minute as u32 * largest_guard).to_string()
}
