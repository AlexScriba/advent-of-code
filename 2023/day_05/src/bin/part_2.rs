#[path = "../domain/mod.rs"]
mod domain;
mod part_1;

use std::env;

use part_1::domain::mapping_pipeline::MappingPipeline;
use part_1::{get_input, parse_inputs};

#[derive(Debug)]
pub struct Input {
    pub seeds: Vec<i64>,
    pub pipeline: MappingPipeline,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let inputs: String = get_input(&args[1]);

    let parsed_input = parse_inputs(&inputs);

    let seeds = parsed_input.seeds;
    let pipeline = parsed_input.pipeline;

    let smallest = find_smallest(seeds, pipeline);

    println!("{smallest}");
}

fn find_smallest(seeds: Vec<i64>, pipeline: MappingPipeline) -> i64 {
    let mut index = 0;
    let mut min = std::i64::MAX;

    loop {
        let start = seeds[index];
        let length = seeds[index + 1];

        for i in start..(start + length) {
            let mapped = pipeline.map_through(i);

            if mapped < min {
                min = mapped;
            }
        }

        index += 2;

        if index >= seeds.len() {
            break;
        }
    }

    min
}
