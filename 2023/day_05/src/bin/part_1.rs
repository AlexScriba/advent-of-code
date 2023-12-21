#[path = "../domain/mod.rs"]
pub mod domain;

use std::env;
use std::fs;

use domain::mapping::parse_mapping;
use domain::mapping::Mapping;
use domain::mapping_pipeline::MappingPipeline;

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

    let mapped_seeds: Vec<i64> = seeds.iter().map(|s| pipeline.map_through(*s)).collect();

    let smallest = mapped_seeds.iter().min().expect("Mapped Seeds are empty");

    println!("{smallest}");
}

pub fn get_input(path: &String) -> String {
    fs::read_to_string(path).expect("Could not read file")
}

pub fn parse_inputs(input: &String) -> Input {
    let lines: Vec<String> = input.split("\n").map(|l| l.to_string()).collect();

    let seeds = parse_seeds(&lines[0]);

    let map_lines = &lines[1..];
    let mut mappings: Vec<Mapping> = vec![];
    let mut saved_lines: Vec<String> = vec![];
    let mut index = 0;
    loop {
        let line = &map_lines[index];

        if line.is_empty() && !saved_lines.is_empty() {
            let mapping = parse_mapping(&saved_lines);
            mappings.push(mapping);

            saved_lines = vec![];
            index += 1;

            if index >= map_lines.len() {
                break;
            }

            continue;
        } else if line.is_empty() {
            index += 1;
            if index >= map_lines.len() {
                break;
            }

            continue;
        }

        saved_lines.push(line.to_string());
        index += 1;

        if index >= map_lines.len() {
            break;
        }
    }

    let pipeline = MappingPipeline { layers: mappings };

    Input { seeds, pipeline }
}

pub fn parse_seeds(line: &String) -> Vec<i64> {
    let parts: Vec<&str> = line.split(" ").collect();

    let nums = &parts[1..];

    nums.iter()
        .map(|n| {
            n.trim()
                .parse::<i64>()
                .expect(&format!("Could not parse seed number [{}]", n))
        })
        .collect()
}
