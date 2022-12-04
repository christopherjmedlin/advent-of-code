use std::collections::HashMap;
use std::fmt;

pub fn day_twelve(input: Vec<String>) -> (String, String) {
    let mut init_state = PotState::new();
    let mut split: Vec<&str> = input[0].split(" ").collect();
    for (i, character) in split[2].chars().enumerate() {
        init_state.set_pot(i as isize, character);
    }

    let mut reaction_map: HashMap<String, char> = HashMap::new();
    for i in 2..input.len() {
        split = input[i].split(" => ").collect();
        reaction_map.insert(split[0].to_string(), 
                            split[1].chars().next().unwrap());
    }

    (part_one(&reaction_map, &init_state), part_two(&reaction_map, &init_state))
}

#[derive(Clone)]
struct PotState {
    pots: HashMap<isize, char>,
    lowest_pot: isize,
    highest_pot: isize,
}

impl PotState {
    pub fn new() -> PotState {
        PotState {
            pots: HashMap::new(),
            lowest_pot: 0,
            highest_pot: 0,
        }
    }
    
    pub fn set_pot(&mut self, pot: isize, character: char) {
        if pot < self.lowest_pot {self.lowest_pot = pot}
        if pot > self.highest_pot {self.highest_pot = pot}
        self.pots.insert(pot, character);
    }

    pub fn get_pot(&self, pot: isize) -> char {
        if self.pots.contains_key(&pot) {
            *self.pots.get(&pot).unwrap()
        } else {
            '.'
        }
    }
}

impl fmt::Display for PotState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();
        for i in self.lowest_pot..self.highest_pot {
	    string.push(self.get_pot(i));    
        }
        write!(f, "{}", string)
    }
}

fn generation(state: &PotState, reaction_map: &HashMap<String, char>) -> PotState {
    let mut new_state = PotState::new();
    for pot in state.lowest_pot-2..state.highest_pot+3 {
        let mut slice: String = String::new();
        let mut first_plant = false;

        for pot2 in pot-2..pot+3 {
            slice.push(state.get_pot(pot2));
        }
        
        if reaction_map.contains_key(&slice) {
            let pattern = reaction_map.get(&slice).unwrap();
            if *pattern == '.' {
                if first_plant {
                    new_state.set_pot(pot, '.');
                }
            } else {
                first_plant = true;
                new_state.set_pot(pot, '#');
            }
        } else {
            if first_plant {
                new_state.set_pot(pot, '.');
            }
        }
    }
    
    //println!("{}", state);
    new_state
}

fn total(state: &PotState) -> isize {
    let mut total = 0;
    for (i, value) in state.pots.iter() {
        if *value == '#' {
            total += i;
        }
    }

    total
}

fn part_one(reaction_map: &HashMap<String, char>, init_state: &PotState) -> String {
    let mut current_state = init_state.clone();
    let mut new_state: PotState;

    for i in 0..20 {
        current_state = generation(&current_state, reaction_map);
    }
    
    /*
    let mut total = 0;
    for (i, value) in current_state.pots.iter() {
        if *value == '#' {
            total += i;
        }
    }
    */
    
    total(&current_state).to_string()
}

fn part_two(reaction_map: &HashMap<String, char>, init_state: &PotState) -> String {
    let mut old_total = 0;
    let mut diff = 0;
    let mut state = init_state.clone();

    for i in 0..100 {
        state = generation(&state, reaction_map);
        let new_total = total(&state);
        diff = new_total - old_total;
        old_total = new_total;
    }

    old_total += diff * 49999999900;
    println!("{}", diff);

    old_total.to_string()
}
