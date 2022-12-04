
pub struct CircularDoublyLinkedList {
    pub nodes: Vec<Node>,
    cursor: Option<usize>,
}

pub struct Node {
    pub val: u32,
    pub prev: usize,
    pub next: usize,
}

impl CircularDoublyLinkedList {
    pub fn new() -> CircularDoublyLinkedList {
        CircularDoublyLinkedList {
            nodes: Vec::new(),
            cursor: None
        }
    }
    
    // Inserts after current node
    pub fn insert(&mut self, val: u32) {
        let i = self.nodes.len();

        let (prev, next) = match self.cursor {
            Some(current) => {
                let next = self.nodes[current].next;
                self.nodes[next].prev = i;
                self.nodes[current].next = i;
                (current, next)
            },
            None => {
                (i, i)
            }
        };
        
        self.nodes.push(Node {
            val: val,
            prev: prev,
            next: next,
        });
        self.cursor = Some(i);
    }
    
    // Removes current node
    pub fn remove(&mut self) {
        let prev = self.nodes[self.cursor.unwrap()].prev;
        let next = self.nodes[self.cursor.unwrap()].next;
        self.nodes[prev].next = next;
        self.nodes[next].prev = prev;
        self.cursor = Some(next);
    }
    
    // Moves cursor back <num> nodes
    pub fn back(&mut self, num: usize) {
        if self.cursor != None {
            let mut cursor = self.cursor.unwrap();
            for i in 0..num {
                cursor = self.nodes[cursor].prev;  
            }

            self.cursor = Some(cursor);
        }
    }

    // Moves cursor forward <num> nodes
    pub fn forward(&mut self, num: usize) {
        if self.cursor != None {
            let mut cursor = self.cursor.unwrap();
            for i in 0..num {
                cursor = self.nodes[cursor].next;  
            }

            self.cursor = Some(cursor);
        }
    }

    // Gets current node value
    pub fn current(&mut self) -> Option<u32> {
        match self.cursor {
            Some(current) => {
                Some(self.nodes[current].val)
            }
            None => {
                None
            }
        }
    }

    pub fn set_cursor(&mut self, index: usize) {
        self.cursor = Some(index);
    }

    pub fn get_cursor(&self) -> Option<usize> {
        self.cursor
    }
}
