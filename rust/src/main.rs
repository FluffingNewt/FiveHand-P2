use std::env;
use crate::modules::fivehand::FiveHand;
mod modules;

// Main Method Calls
fn main() {
    let args: Vec<String> = env::args().collect();
    let f = if args.len() > 2 { args[2].clone() } else { String::new() };
    let mut game = FiveHand::new(f.clone());
    game.play(f);
}