mod part_1;

use part_1::*;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = get_input(&args[1]);

    let time_str = get_after_tag(&input, "Time");
    let distance_str = get_after_tag(&input, "Distance");

    let time = remove_spaces_and_parse(&time_str);
    let distance = remove_spaces_and_parse(&distance_str);

    dbg!(&time);
    dbg!(&distance);

    let (lower, upper) = solve_travel_time(time, distance);

    dbg!(&lower);
    dbg!(&upper);

    println!("{}", upper.floor() - lower.floor());
}

fn remove_spaces_and_parse(string: &String) -> i64 {
    let interim = string
        .trim()
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect::<String>();

    dbg!(&interim);

    interim.parse().expect("Could not parse number")
}
