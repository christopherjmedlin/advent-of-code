use std::collections::{BTreeSet, HashMap};

pub fn day_seven(input: Vec<String>) -> (String, String) {
    (part_one(&input), part_two(&input))
}

struct Step {
    parents: Vec<char>,
    children: Vec<char>,
    visited: bool,
    secs_left: u8,
}

fn create_step_map(input: &Vec<String>) -> HashMap<char, Step> {
    let mut step_map: HashMap<char, Step> = HashMap::new();

    // prepopulate map
    for character in 65..91 {
        step_map.insert(character as u8 as char, Step {
            parents: Vec::new(),
            children: Vec::new(),
            visited: false,
            secs_left: character as u8 - 64,
        });
    }

    for line in input.iter() {
        let split: Vec<&str> = line.split(' ').collect();
        let pre_step = split[1].to_string().chars().next().unwrap();
        let post_step = split[7].to_string().chars().next().unwrap();

        {
            let step = step_map.get_mut(&pre_step).unwrap();
            step.children.push(post_step);
        } {
            let step = step_map.get_mut(&post_step).unwrap();
            step.parents.push(pre_step);
        }
    }

    step_map
}

fn find_roots(step_map: &HashMap<char, Step>) -> Vec<char> {
    let mut roots: Vec<char> = Vec::new();
    for (k, v) in step_map.iter() {
        if v.parents.len() == 0 {
            roots.push(*k);
        }
    }
    roots
}

/**
1.Go through the input file, reading one line at a time
  and alter a hash map of objects, where each object is a letter, all of its parents, all of its children, and whether it has been visited yet.
   ex: Step A must be before B ==> A.children.add(B) AND B.parents.add(A)
2.After the entire tree is built, start anywhere, and go up. It doesn't matter which parent is chosen at each step. After you reach a letter
  with no parents, you are at the root. Root is the first character of your answer.

4.add root's children to the TODOs
3.Until TODOs is empty do the following:
  start at the first in TODOs alphabetically,
  -if the letter you're at has all of its parents "visited" bits as 1, set that letter as visited, add it to the solution, 
   add all of its children to the TODOs (MAKE SURE NOT TO HAVE DUPLICATES), REMOVE THIS LETTER FROM TODOs, start the loop over
  -else, go to the next letter in TODOs
*/
fn part_one(input: &Vec<String>) -> String {
    // first vector in pair is parents (or preceding steps)
    // next is children (or following steps)
    
    let mut step_map: HashMap<char, Step> = create_step_map(input);
    let mut todos: Vec<char> = find_roots(&step_map);
    
    todos.sort();
    let mut solution: Vec<char> = Vec::new();
    let mut i = 0;
    while todos.len() > 0 {
        todos.sort();
        let mut ready = true;
        let step_character = todos[i];

        {
            let step = step_map.get(&step_character).unwrap();

            for parent in step.parents.iter() {
                if !step_map.get(parent).unwrap().visited {
                    ready = false;
                }
            }
            if step.parents.len() == 0 {
                ready = true;
            }
        }
        if ready {
            solution.push(step_character);
            todos.remove(i);
            let step = step_map.get_mut(&step_character).unwrap();
            step.visited = true;
            
            for character in step.children.iter() {
                if !todos.contains(character) {
                    todos.push(*character);
                }
            }

            todos.sort();
            i = 0;
        }
        else {
            i += 1;
        }
    }
    
    let s: String = solution.iter().collect();
    s
}

/**
  AOC 2018 Day 7 Part 2

1.Go through the input file, reading one line at a time
  and alter a hash map of objects, where each object is a letter, all of its parents, all of its children, and whether it has been visited yet.
   ex: Step A must be before B ==> A.children.add(B) AND B.parents.add(A)

add all roots to the TODOs, alphabetically

3.Until TODOs is empty do the following:
  -work on the first N nodes only if each one of them has all of its prerequisites met
  -if any of the nodes is "done", set it as visited, remove it, (add it to the solution if you want) 
    shift the TODOs up and append all of its children ALPHABETICALLY at the end of TODOs, ALLOWING NO DUPLICATES TO RESULT FROM INSERTION, remove it from TODOs
*/
fn part_two(input: &Vec<String>) -> String {
    String::from("¯\\_(ツ)_/¯")
}
