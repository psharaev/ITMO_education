use crate::mark::Mark;
use crate::mark::Mark::None;

pub const EDGE_SIZE: usize = 3;
pub const COUNT_CELLS: usize = EDGE_SIZE * EDGE_SIZE;

#[derive(Copy, Clone)]
pub struct Board {
    board: [Mark; COUNT_CELLS],
    used_cells: usize,
}

impl Board {
    pub fn get_used_cells(&self) -> usize {
        self.used_cells
    }

    pub fn set_cell(&mut self, index: usize, mark: Mark) {
        if mark != None && self.board[index] == None {
            self.used_cells += 1
        }
        if mark == None && self.board[index] != None {
            self.used_cells -= 1
        }
        self.board[index] = mark;
    }

    pub fn has_free_space(&self) -> bool {
        self.used_cells < COUNT_CELLS
    }

    pub fn is_move_correct(&self, index: usize) -> bool {
        (0..COUNT_CELLS).contains(&index) && self.board[index] == None
    }

    pub fn get_cell_by_id(&self, index: usize) -> Mark {
        self.board[index]
    }

    pub fn get_cell(&self, row: usize, col: usize) -> Mark {
        self.get_cell_by_id(row * EDGE_SIZE + col)
    }

    pub fn new() -> Board {
        Board {
            board: [None; COUNT_CELLS],
            used_cells: 0_usize,
        }
    }

    pub fn to_string(self, with_indexes: bool) -> String {
        let mut formatted_board: String = "+---".repeat(EDGE_SIZE) + "+\n";
        for row in 0..EDGE_SIZE {
            let offset = row * EDGE_SIZE;
            for col in 0..EDGE_SIZE {
                if with_indexes {
                    formatted_board += &*format!("| {} ", self.get_symbol_with_index(offset + col));
                } else {
                    formatted_board += &*format!("| {} ", self.get_symbol(offset + col));
                }
            }
            formatted_board += "|\n";
        }
        formatted_board.push_str("+---".repeat(EDGE_SIZE).as_str());
        formatted_board += "+\n";
        formatted_board
    }

    pub fn is_win(&self, mark: Mark) -> bool {
        for row in 0..EDGE_SIZE {
            if self.is_fill_horizontal(row, mark) {
                return true;
            }
        }
        for col in 0..EDGE_SIZE {
            if self.is_fill_vertical(col, mark) {
                return true;
            }
        }

        if self.is_fill_main_diagonal(mark) {
            return true;
        }

        if self.is_fill_side_diagonal(mark) {
            return true;
        }

        false
    }

    fn is_fill_horizontal(&self, row: usize, mark: Mark) -> bool {
        for col in 0..EDGE_SIZE {
            if self.get_cell(row, col) != mark {
                return false;
            }
        }
        true
    }

    fn is_fill_vertical(&self, col: usize, mark: Mark) -> bool {
        for row in 0..EDGE_SIZE {
            if self.get_cell(row, col) != mark {
                return false;
            }
        }
        true
    }

    fn is_fill_main_diagonal(&self, mark: Mark) -> bool {
        for i in 0..EDGE_SIZE {
            if self.get_cell(i, i) != mark {
                return false;
            }
        }
        true
    }

    fn is_fill_side_diagonal(&self, mark: Mark) -> bool {
        for i in 0..EDGE_SIZE {
            if self.get_cell(i, EDGE_SIZE - 1 - i) != mark {
                return false;
            }
        }
        true
    }

    fn get_symbol_with_index(&self, index: usize) -> String {
        match self.board[index] {
            Mark::X => "X".to_string(),
            Mark::O => "0".to_string(),
            None => (index + 1).to_string(),
        }
    }

    fn get_symbol(&self, index: usize) -> String {
        match self.board[index] {
            Mark::X => "X".to_string(),
            Mark::O => "0".to_string(),
            None => " ".to_string(),
        }
    }
}
