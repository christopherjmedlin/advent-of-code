pub fn day_thirteen(input: Vec<String>) -> (String, String) {
    let mut rails: Vec<Vec<char>> = Vec::new();
    let mut carts: Vec<Cart> = Vec::new();

    for (y, line) in input.iter().enumerate() {
        let mut char_vector = Vec::new();
        for (x, character) in line.chars().enumerate() {
            match character {
                '>' => {
                    carts.push(Cart::new((x, y), Direction::Right));
                    char_vector.push('-');
                },
                '^' => {
                    carts.push(Cart::new((x, y), Direction::Up));
                    char_vector.push('|');
                },
                'v' => {
                    carts.push(Cart::new((x, y), Direction::Down));
                    char_vector.push('|');
                },
                '<' => {
                    carts.push(Cart::new((x, y), Direction::Left));
                    char_vector.push('|');
                },
                _ => {
                    char_vector.push(character);
                }
            }
        }
        rails.push(char_vector);
    }

    (part_one(&rails, &carts), String::new())
}

#[derive(Clone)]
enum Direction {
    Up,
    Down,
    Left,
    Right
}

#[derive(Clone)]
struct Cart {
    location: (usize, usize),
    dir: Direction,
    last_turn: String,
}

impl Cart {
    pub fn new(location: (usize, usize), dir: Direction) -> Cart {
        Cart {
            location: location,
            dir: dir,
            last_turn: String::from("right")
        }
    }

    pub fn turn_right(&mut self) {
        self.dir = match self.dir {
            Direction::Left => {Direction::Up},
            Direction::Up => {Direction::Right},
            Direction::Right => {Direction::Down},
            Direction::Down => {Direction::Left},
        }
    }

    pub fn turn_left(&mut self) {
        self.dir = match self.dir {
            Direction::Left => {Direction::Down},
            Direction::Up => {Direction::Left},
            Direction::Right => {Direction::Up},
            Direction::Down => {Direction::Right},
        }
    }

    pub fn intersection_turn(&mut self) {
        self.last_turn = match self.last_turn.as_str() {
            "right" => {self.turn_left(); String::from("left")},
            "left" => {String::from("straight")},
            "straight" => {self.turn_right(); String::from("right")},
            &_ => {panic!("huh?!")}
        }
    }

    pub fn detect_crash(&self, carts: &Vec<Cart>) -> Option<(usize, usize)> {
        let mut occurences = 0;
        let mut crash = (0, 0);
        
        for cart in carts {
            if cart.location == self.location {
                occurences += 1;
                crash = cart.location;
            }
        }

        //println!("{}", occurences);
        if occurences > 1 {
            Some(crash)
        } else {
            None
        }
    }
}

fn part_one(rails: &Vec<Vec<char>>, carts: &Vec<Cart>) -> String {
    let mut crash = false;
    let mut first_crash = (0, 0);
    let mut carts = carts.to_vec();

    while !crash {
        println!("{}, {}", carts[0].location.0, carts[0].location.1);
        let x = carts[0].location.0;
        let y = carts[0].location.1;
        x;
        for cart_index in 0..carts.len() {
            {
                let mut cart = &mut carts[cart_index];
                match cart.dir {
                    Direction::Up => {
                        cart.location.1 -= 1;
                    },
                    Direction::Down => {
                        cart.location.1 += 1;
                    },
                    Direction::Left => {
                        cart.location.0 -= 1;
                    },
                    Direction::Right => {
                        cart.location.0 += 1;
                    }
                }
                
                match rails[cart.location.1][cart.location.0] {
                    '\\' => {
                        match cart.dir {
                            Direction::Right | Direction::Left => {cart.turn_right()},
                            Direction::Up | Direction::Down => {cart.turn_left()},
                        }
                    },
                    '/' => {
                        match cart.dir {
                            Direction::Right | Direction::Left => {cart.turn_left()},
                            Direction::Up | Direction::Down => {cart.turn_right()},
                        }
                    },
                    '+' => {
                        cart.intersection_turn();                   
                    },
                    _ => {}
                }
            }
            
            match &carts[cart_index].detect_crash(&carts) {
                Some(crash_coords) => {
                    first_crash = *crash_coords;
                    crash = true;
                },
                None => {}
            }
        }
    }

    format!("{}, {}", first_crash.0, first_crash.1)
}
