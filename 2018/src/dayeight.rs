
pub fn day_eight(input: Vec<String>) -> (String, String) {
    (part_one(&input), part_two(&input))
}

fn count_node_metadata(node_data: &Vec<u8>, index: &usize) -> (u32, usize) {
    let num_nodes = node_data[*index];
    let num_metadata = node_data[index + 1];
    let mut total_metadata: u32 = 0;
    let mut new_index = *index + 2;

    for i in 0..num_nodes {
        let result = count_node_metadata(node_data, &new_index);
        total_metadata += result.0;
        new_index += result.1;
    }

    for i in 0..num_metadata {
        total_metadata += node_data[new_index] as u32;
        new_index += 1;
    }

    (total_metadata, new_index - index)
}

fn get_node_value(node_data: &Vec<u8>, index: &usize) -> (u32, usize) {
    let num_nodes = node_data[*index];
    let num_metadata = node_data[index + 1];
    let mut total_metadata: u32 = 0;
    let mut new_index = *index + 2;
    let mut child_values: Vec<u32> = Vec::new();
    let uindex = *index as u32;
    
    for i in 0..num_nodes {
        let result = get_node_value(node_data, &new_index);
        child_values.push(result.0);
        new_index += result.1;
    }
    
    let mut val: u32 = 0;
    for i in 0..num_metadata {
        let child_index = node_data[new_index] as usize;
        if child_index > 0 {
            if num_nodes == 0 {
                val += child_index as u32;
            } else if child_index - 1 < child_values.len() {
                val += child_values[child_index - 1] as u32;
            }
        }

        new_index += 1;
    }

    
    (val, new_index - index)
}

fn part_one(input: &Vec<String>) -> String {
    let node_data: Vec<u8> = input[0].split(" ")
                                     .map(|x| x.parse::<u8>().unwrap())
                                     .collect();

    count_node_metadata(&node_data, &0).0.to_string()
}

fn part_two(input: &Vec<String>) -> String {
    let node_data: Vec<u8> = input[0].split(" ")
                                     .map(|x| x.parse::<u8>().unwrap())
                                     .collect();

    get_node_value(&node_data, &0).0.to_string()
}
