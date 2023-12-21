mod part1;

use part1::{get_input, parse_input, Game};
use std::env;

#[derive(Clone, Debug)]
struct Card {
    numbers: Game,
    copies: usize,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input: Vec<String> = get_input(&args[1]).lines().map(|s| s.to_string()).collect();

    let games: Vec<Game> = input.iter().map(parse_input).collect();

    let num_cards = process_games(games);

    println!("{num_cards}")
}

fn process_games(games: Vec<Game>) -> u32 {
    let mut cards: Vec<Card> = games
        .iter()
        .map(|g| Card {
            numbers: g.clone(),
            copies: 1,
        })
        .collect();

    let mut index = 0;

    loop {
        let current_card = cards[index].clone();

        let num_matches_for_card = current_card.numbers.num_matches();

        for i in 1..=num_matches_for_card {
            cards[index + i as usize].copies += current_card.copies;
        }

        index += 1;
        if index >= games.len() {
            break;
        }
    }

    cards.iter().map(|c| c.copies as u32).sum()
}
