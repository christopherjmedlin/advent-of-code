
pub fn day_one(input: Vec<String>) -> (String, String) {
    let mut frequency: i32 = 0;
    let mut reached_frequencies: [bool; 150000] = [false; 150000];
    let mut result_found: bool = false;
    
    while !result_found {
        for line in input.iter() {
            let sign = &line[0..1];
            let num: &i32 = &line[1..line.len()]
                      .parse::<i32>().unwrap();

            if sign == "+" {
                frequency += num;    
            } else {
                frequency -= num;
            }
            
            if frequency > 0 {
                if reached_frequencies[frequency as usize] {
                    result_found = true;
                    break;
                } else {
                    reached_frequencies[frequency as usize] = true;
                }
            }   
        }
    }

    (String::from("¯\\_(ツ)_/¯"), frequency.to_string())
}
