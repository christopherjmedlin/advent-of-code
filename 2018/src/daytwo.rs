use std::collections::HashMap;

pub fn day_two(input: Vec<String>) -> (String, String) {
    let mut identical_characters = String::new();
    let mut correct_boxes_found = false;
    let mut result: String = String::new();

    for line1 in input.iter() {
        for line2 in input.iter() {
            let mut different_characters = 0;
            for pair in line1.chars().zip(line2.chars()) {
                let (character1, character2) = pair;
                if character1 != character2 {
                    different_characters += 1;
                }
            }

            if different_characters == 1 {
                result = String::new();
                for pair in line1.chars().zip(line2.chars()) {
                    let (character1, character2) = pair;
                    if character1 == character2 {
                        result.push(character1);
                    }
                }
                println!("{}", line1);
                println!("{}", line2);
            }
        }
    }

    (String::from("¯\\_(ツ)_/¯"), result.to_string())
}
