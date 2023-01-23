use crate::board::{COUNT_CELLS, EDGE_SIZE};
use crate::mark::Mark;
use crate::player::Player;
use crate::Board;
use std::cmp::{max, min};

pub struct Computer {
    name: String,
}

impl Computer {
    pub fn new() -> Computer {
        Self::new_with_name("computer".to_string())
    }

    pub fn new_with_name(name: String) -> Computer {
        Computer { name }
    }
}

impl Player for Computer {
    fn next_move(&self, board: &Board, _mark: Mark) -> usize {
        find_best_move(*board, _mark)
    }

    fn get_name(&self) -> String {
        self.name.clone()
    }
}

fn find_best_move(mut board: Board, mark: Mark) -> usize {
    let mut best_val = -10000;
    let mut best_move = 0;

    for i in 0..COUNT_CELLS {
        if board.get_cell_by_id(i) == Mark::None {
            board.set_cell(i, mark);

            let move_val = minimax(&mut board, 0, false, mark);

            board.set_cell(i, Mark::None);

            if move_val > best_val {
                best_move = i;
                best_val = move_val;
            }
        }
    }

    best_move
}

fn minimax(board: &mut Board, depth: usize, is_max: bool, mark: Mark) -> i32 {
    let score = evaluate(board, mark);

    if score == 10 || score == -10 {
        return score;
    }

    if !board.has_free_space() {
        return 0;
    }

    if is_max {
        let mut best = -10000;

        for i in 0..COUNT_CELLS {
            if board.get_cell_by_id(i) == Mark::None {
                board.set_cell(i, mark);

                best = max(best, minimax(board, depth + 1, !is_max, mark));

                board.set_cell(i, Mark::None);
            }
        }
        best
    } else {
        let mut best = 10000;

        for i in 0..COUNT_CELLS {
            if board.get_cell_by_id(i) == Mark::None {
                board.set_cell(i, mark.invert());

                best = min(best, minimax(board, depth + 1, !is_max, mark));

                board.set_cell(i, Mark::None);
            }
        }
        best
    }
}

fn evaluate(board: &Board, mark: Mark) -> i32 {
    for row in 0..EDGE_SIZE {
        if board.get_cell(row, 0) == board.get_cell(row, 1)
            && board.get_cell(row, 1) == board.get_cell(row, 2)
        {
            if board.get_cell(row, 0) == mark {
                return 10;
            } else if board.get_cell(row, 0) != Mark::None {
                return -10;
            }
        }
    }

    for col in 0..EDGE_SIZE {
        if board.get_cell(0, col) == board.get_cell(1, col)
            && board.get_cell(1, col) == board.get_cell(2, col)
        {
            if board.get_cell(0, col) == mark {
                return 10;
            } else if board.get_cell(0, col) != Mark::None {
                return -10;
            }
        }
    }

    if board.get_cell(0, 0) == board.get_cell(1, 1) && board.get_cell(1, 1) == board.get_cell(2, 2)
    {
        if board.get_cell(0, 0) == mark {
            return 10;
        } else if board.get_cell(0, 0) != Mark::None {
            return -10;
        }
    }

    if board.get_cell(0, 2) == board.get_cell(1, 1) && board.get_cell(1, 1) == board.get_cell(2, 0)
    {
        if board.get_cell(0, 2) == mark {
            return 10;
        } else if board.get_cell(0, 2) != Mark::None {
            return -10;
        }
    }

    0
}
