use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Mark {
    X,
    O,
    None,
}

impl Mark {
    pub fn invert(&self) -> Mark {
        match self {
            Mark::X => Mark::O,
            Mark::O => Mark::X,
            Mark::None => Mark::None,
        }
    }
}

impl fmt::Display for Mark {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Mark::X => {
                write!(f, "X")
            }
            Mark::O => {
                write!(f, "0")
            }
            Mark::None => {
                write!(f, " ")
            }
        }
    }
}
