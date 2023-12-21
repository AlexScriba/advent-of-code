use std::collections::HashSet;
use std::env;
use std::fs;

#[derive(Clone, Debug)]
pub struct Game {
    pub winning_numbers: HashSet<i32>,
    pub card_numbers: Vec<i32>,
}

impl Game {
    pub fn num_matches(&self) -> u32 {
        self.card_numbers
            .iter()
            .filter(|n| self.winning_numbers.contains(n))
            .count()
            .try_into()
            .unwrap()
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input: Vec<String> = get_input(&args[1]).lines().map(|s| s.to_string()).collect();

    let games: Vec<Game> = input.iter().map(parse_input).collect();

    let total: u32 = games.iter().map(get_points_for_game).sum();

    println!("{total}");
}

pub fn get_input(file_path: &String) -> String {
    fs::read_to_string(file_path).expect("Cannot read file")
}

pub fn get_points_for_game(game: &Game) -> u32 {
    let num_matches = game.num_matches();

    match num_matches {
        0 => 0,
        n => 2_u32.pow(n - 1),
    }
}

pub fn parse_input(input: &String) -> Game {
    let parts: Vec<&str> = input.split("|").collect();

    let winning_numbers = parse_winning_numbers(&parts[0]);
    let card_numbers = parse_card_numbers(&parts[1]);

    Game {
        winning_numbers,
        card_numbers,
    }
}

pub fn parse_winning_numbers(input: &str) -> HashSet<i32> {
    let num_string: String = input.split(":").collect::<Vec<_>>()[1].to_string();

    let numbers = get_numbers_from_string(&num_string);

    HashSet::from_iter(numbers.iter().cloned())
}

pub fn parse_card_numbers(input: &str) -> Vec<i32> {
    get_numbers_from_string(input)
}

pub fn get_numbers_from_string(string: &str) -> Vec<i32> {
    string
        .trim()
        .split(" ")
        .filter(|n| !n.is_empty())
        .map(|n| {
            n.parse::<i32>()
                .expect(&format!("Could not parse number [{n}]"))
        })
        .collect()
}
