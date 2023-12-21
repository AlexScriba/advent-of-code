use regex::Regex;
use std::env;
use std::fs;

type Num = f64;

pub struct Race {
    pub time: i32,
    pub distance: i32,
}

impl Race {
    pub fn new(time: i32, distance: i32) -> Race {
        Race { time, distance }
    }
}

pub struct Inputs {
    pub races: Vec<Race>,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = get_input(&args[1]);

    let inputs = process_input(&input);

    let wins: Vec<usize> = inputs.races.iter().map(|r| num_ways_to_win(r)).collect();

    let prod: usize = wins.iter().product();

    println!("{}", prod);
}

pub fn num_ways_to_win(race: &Race) -> usize {
    let (lower, upper) = solve_travel_time(race.time as i64, race.distance as i64);
    let wins = get_ints_between(lower, upper);

    wins.len()
}

pub fn process_input(in_str: &String) -> Inputs {
    let times: Vec<i32> = split_to_nums(&get_after_tag(in_str, "Time"));
    let distances: Vec<i32> = split_to_nums(&get_after_tag(in_str, "Distance"));

    let zipped: Vec<Race> = times
        .iter()
        .zip(distances.iter())
        .map(|(t, d)| Race::new(*t, *d))
        .collect();

    Inputs { races: zipped }
}

pub fn get_after_tag(text: &String, tag: &str) -> String {
    let re: Regex = Regex::new(&format!(r"(?:{}:\s*)(.*)", tag)).unwrap();

    let test = re
        .captures(text)
        .expect("Could not parse regex")
        .get(1)
        .expect("Could not get first capture")
        .as_str();

    test.to_string()
}

pub fn split_to_nums(string: &String) -> Vec<i32> {
    string
        .trim()
        .split(" ")
        .filter(|s| !s.is_empty())
        .map(|n| n.parse::<i32>().expect("Not a number: {n}"))
        .collect()
}

pub fn get_input(path: &String) -> String {
    fs::read_to_string(path).expect("Could not read file")
}

pub fn solve_travel_time(time_limit: i64, record: i64) -> (Num, Num) {
    solve_x(1.0, -time_limit as f64, record as f64)
}

pub fn solve_x(a: Num, b: Num, c: Num) -> (Num, Num) {
    let sqr = b.powf(2.0) - 4.0 * a * c;

    if sqr.is_sign_negative() {
        panic!("Value under the square root is negative! {sqr}");
    }

    let divisor = 2.0 * a;
    let sqrt = sqr.sqrt();

    (-(b + sqrt) / divisor, -(b - sqrt) / divisor)
}

pub fn get_ints_between(lower: Num, upper: Num) -> Vec<i32> {
    let start = lower.floor() as i32;
    let end = upper.ceil() as i32;

    (start + 1..end).collect()
}
