#[path = "../lib.rs"]
mod lib;

use std::env;
use std::fs;

use lib::card::normal_card::*;
use lib::card::Card;
use lib::hand::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = get_input(&args[1]);

    let mut hands: Vec<Hand<NormalCard>> = input
        .lines()
        .map(|l| parse_line::<NormalCard>(l))
        .map(|(bid, cards)| Hand::new(bid, &cards))
        .collect();

    hands.sort();

    let total: u32 = hands
        .iter()
        .enumerate()
        .map(|(ind, hand)| (ind as u32 + 1) * hand.bid)
        .sum();

    println!("{total}");
}

fn get_input(path: &str) -> String {
    fs::read_to_string(path).expect("Could not read file")
}

fn parse_line<C>(line: &str) -> (u32, Vec<C>)
where
    C: Card,
{
    let parts: Vec<&str> = line.split(" ").collect();

    let bid: u32 = parts[1]
        .parse()
        .expect(&format!("Could not parse bid: {}", parts[1]));
    let cards = C::cards_from_string(parts[0]);

    (bid, cards)
}
