use linkedlist::CircularDoublyLinkedList;
use std::collections::VecDeque;

pub fn day_fourteen(input: Vec<String>) -> (String, String) {
    let num_recipes: u32 = input[0].parse().expect("Invalid input");
    recipes(&num_recipes)
}

fn recipes(num_recipes: &u32) -> (String, String) {
    let mut recipes = CircularDoublyLinkedList::new();
    let mut current_num_recipes = 2;

    recipes.insert(3);
    let mut first_elf = recipes.get_cursor().unwrap();
    recipes.insert(7);
    let mut second_elf = recipes.get_cursor().unwrap();
    let mut end = recipes.get_cursor().unwrap();

    let mut pattern: VecDeque<u8> = VecDeque::new();
    let mut pattern_recipe = 0;
    let mut queue: VecDeque<u8> = VecDeque::new();

    for digit in num_recipes.to_string().chars().map(|c| c as u8 - 48) {
        pattern.push_back(digit);
    }
    for i in 0..pattern.len() {
        queue.push_back(0);
    }

    while current_num_recipes < num_recipes + 10 || pattern_recipe == 0 {
        let sum = recipes.nodes[first_elf].val + recipes.nodes[second_elf].val;
        for digit in sum.to_string().chars().map(|c| c as u8 - 48) {
            current_num_recipes += 1;
            recipes.insert(digit as u32);
            
            queue.pop_front();
            queue.push_back(digit);

            if queue == pattern && pattern_recipe == 0 {
                pattern_recipe = current_num_recipes - pattern.len() as u32
            }
        }
        // could probably just be end += 1
        end = recipes.get_cursor().unwrap();

        recipes.set_cursor(first_elf);
        let mut moves = (recipes.current().unwrap() + 1) as usize;
        recipes.forward(moves);
        first_elf = recipes.get_cursor().unwrap();
        recipes.set_cursor(second_elf);
        moves = (recipes.current().unwrap() + 1) as usize;
        recipes.forward(moves);
        second_elf = recipes.get_cursor().unwrap();
        
        recipes.set_cursor(end);
    }

    recipes.back((current_num_recipes - num_recipes) as usize);
    let mut digits = String::new();
    for i in 0..10 {
        recipes.forward(1);
        digits.push((recipes.current().unwrap() as u8 + 48) as char);
    }

    (digits, pattern_recipe.to_string())
}
