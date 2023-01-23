use crate::mark::Mark;
use crate::Board;

pub mod computer;
pub mod human;

pub trait Player {
    fn next_move(&self, board: &Board, mark: Mark) -> usize;

    fn get_name(&self) -> String;
}
