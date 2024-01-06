use std::env;
use std::fs;

use day_09::base_sequence::BaseSequence;
use day_09::rev_sequence::RevSequence;
use day_09::sequence::Sequence;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_contents = get_input(&args[1]);

    let mut total_base = 0;
    let mut total_rev = 0;
    for line in file_contents.lines() {
        let nums = parse(line);

        let mut seq_base = BaseSequence::new(nums.clone());
        seq_base.extrapolate();
        total_base += seq_base.get_last();

        let mut seq_rev = RevSequence::new(nums);
        seq_rev.extrapolate();
        total_rev += seq_rev.get_last();
    }

    println!("Part 1: {total_base}");
    println!("Part 2: {total_rev}");
}

fn get_input(path: &str) -> String {
    fs::read_to_string(path).expect("Could not read file")
}

pub fn parse(line: &str) -> Vec<i32> {
    line.split(" ")
        .map(|n| n.parse::<i32>().expect("Could not parse num"))
        .collect()
}
