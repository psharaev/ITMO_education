use crate::mark::Mark;
use crate::player::Player;
use crate::who_move::WhoMove;
use crate::Board;

pub struct Game<F: Player, S: Player> {
    board: Board,
    who_move: WhoMove,
    first_player: F,
    second_player: S,
}

pub enum WhoWin {
    FirstPlayerWin,
    SecondPlayerWin,
    Draw,
}

impl<F: Player, S: Player> Game<F, S> {
    pub fn new(first_player: F, second_player: S, who_first_move: WhoMove) -> Game<F, S> {
        Game {
            board: Board::new(),
            who_move: who_first_move,
            first_player,
            second_player,
        }
    }

    pub fn run(mut self) -> (Board, WhoWin) {
        loop {
            let index;
            let seated_mark;
            let who_moved = self.who_move;
            match self.who_move {
                WhoMove::FirstPlayer => {
                    let mark = Mark::X;
                    index = self.first_player.next_move(&self.board, mark);
                    self.board.set_cell(index, mark);
                    seated_mark = mark;
                    self.who_move = WhoMove::SecondPlayer
                }
                WhoMove::SecondPlayer => {
                    let mark = Mark::O;
                    index = self.second_player.next_move(&self.board, mark);
                    self.board.set_cell(index, mark);
                    seated_mark = mark;
                    self.who_move = WhoMove::FirstPlayer;
                }
            }
            if self.board.is_win(seated_mark) {
                return (self.board, move_to_win(who_moved));
            }

            if self.board.get_used_cells() == 9_usize {
                return (self.board, WhoWin::Draw);
            }
        }
    }
}

fn move_to_win(who_move: WhoMove) -> WhoWin {
    match who_move {
        WhoMove::FirstPlayer => WhoWin::FirstPlayerWin,
        WhoMove::SecondPlayer => WhoWin::SecondPlayerWin,
    }
}
