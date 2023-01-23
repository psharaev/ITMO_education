use crate::who_move::WhoMove;
use std::io::BufRead;
use std::str::FromStr;

pub fn read_line() -> String {
    let mut stdin = std::io::stdin().lock();
    let mut line = String::new();
    stdin.read_line(&mut line).unwrap();
    line
}

pub fn read_type<T: FromStr>() -> Option<T> {
    let r = read_line().trim().parse::<T>().ok();
    r
}

pub fn choose_who_first_move(first_name: String, second_name: String) -> WhoMove {
    loop {
        println!("Enter who first move:");
        println!("1 - {}", first_name);
        println!("2 - {}", second_name);
        let res = read_type::<i32>();
        match res {
            None => {
                println!("Is not a number, try again")
            }
            Some(number) => {
                if number == 1 {
                    return WhoMove::FirstPlayer;
                }
                if number == 2 {
                    return WhoMove::SecondPlayer;
                }
                println!("Is not 1 or 2, try again");
            }
        }
    }
}
