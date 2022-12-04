use linkedlist::CircularDoublyLinkedList;

pub fn day_nine(input: Vec<String>) -> (String, String) {
    let split: Vec<&str> = input[0].split(" ").collect();
    let players: u32 = split[0].parse().unwrap();
    let marbles: u32 = split[6].parse().unwrap();
    (highest_score(players, marbles), highest_score(players, marbles * 100))
}

fn highest_score(players: u32, marbles: u32) -> String {
    let mut played_marbles = CircularDoublyLinkedList::new();
    let mut current_player: usize = 0;
    let mut player_scores: Vec<u32> = Vec::with_capacity(players as usize);
    for i in 0..players {
        player_scores.push(0);
    }
    played_marbles.insert(0);

    for marble in 1..marbles + 1 {
        if marble % 23 == 0 {
            player_scores[current_player] += marble;
            played_marbles.back(7);
            
            player_scores[current_player] += played_marbles.current().unwrap();
            played_marbles.remove();
        } else {
            played_marbles.forward(1);
            played_marbles.insert(marble); 
        }

        current_player += 1;
        current_player %= player_scores.len();
    }

    let mut high_score: u32 = 0;
    for score in player_scores.iter() {
        if *score > high_score {
            high_score = *score;
        }
    }

    high_score.to_string()   
}
