use crate::io::read_type;
use crate::mark::Mark;
use crate::player::Player;
use crate::Board;

pub struct Human {
    name: String,
}

impl Human {
    pub fn new_with_name(name: String) -> Human {
        Human { name }
    }
}

impl Player for Human {
    fn next_move(&self, board: &Board, mark: Mark) -> usize {
        loop {
            println!("You move player: {}, mark: {}", self.name, mark);
            println!("{}", board.to_string(true));
            println!("Enter number in 1..9");
            let res = read_type::<usize>();
            match res {
                None => {
                    println!("Is not a number, try again")
                }
                Some(x) => {
                    if !(1..=9).contains(&x) {
                        println!("Number is not in 1..9, try again");
                        continue;
                    }
                    if !board.is_move_correct(x - 1) {
                        println!("This cell is not free, try again");
                        continue;
                    }
                    return x - 1;
                }
            }
        }
    }

    fn get_name(&self) -> String {
        self.name.clone()
    }
}
