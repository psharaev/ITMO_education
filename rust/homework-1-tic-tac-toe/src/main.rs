use crate::board::Board;
use crate::game::{Game, WhoWin};
use crate::io::{choose_who_first_move, read_line};
use crate::player::Player;
use player::computer::Computer;
use player::human::Human;

mod board;
mod game;
mod io;
mod mark;
mod player;
mod who_move;

fn main() {
    'main_loop: loop {
        let first_player = Human::new_with_name("human".to_string());
        let second_player = Computer::new();
        let who_first_move =
            choose_who_first_move(first_player.get_name(), second_player.get_name());
        let game = Game::new(first_player, second_player, who_first_move);
        let (board, who_win) = game.run();
        println!("Ended board:");
        println!("{}", board.to_string(false));
        match who_win {
            WhoWin::FirstPlayerWin => {
                println!("First player win!")
            }
            WhoWin::SecondPlayerWin => {
                println!("Second player win!")
            }
            WhoWin::Draw => {
                println!("Nobody win, draw");
            }
        }
        loop {
            println!("Try again? (y/n)");
            let line = read_line();
            let trim_line = line.trim();
            if trim_line.eq(&"y".to_string()) {
                break;
            } else if trim_line.eq(&"n".to_string()) {
                break 'main_loop;
            } else {
                println!("Is not 'y' or 'n'");
            }
        }
    }
}
