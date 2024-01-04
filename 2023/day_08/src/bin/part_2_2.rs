use std::env;
use std::fs;
use std::rc::Rc;

use day_08::cycler::Cycler;
use day_08::instruction::Instruction;
use day_08::math::lcm_vec;
use day_08::node::Node;
use day_08::parsing::parse_instructions;
use day_08::parsing::parse_network;

fn main() {
    let args: Vec<String> = env::args().collect();

    let file: String = get_inputs(&args[1]);
    let file_lines: Vec<&str> = file.lines().collect();

    let instruction_line = file_lines[0];
    let network_lines = &file_lines[2..];

    let instructions_list = parse_instructions(instruction_line);
    let instructions = Cycler::new(instructions_list);
    let network = parse_network(network_lines);

    let start_nodes: Vec<Node> = network
        .get_node_names()
        .iter()
        .filter(|n| is_starting_node(n))
        .map(|n| network.get_node(n))
        .collect();

    let path_lengths: Vec<u128> = start_nodes
        .iter()
        .map(|n| find_path_length(Rc::clone(n), &instructions) as u128)
        .collect();

    let result = lcm_vec(&path_lengths);

    println!("{}", result);
}

fn find_path_length(start_node: Node, instructions: &Cycler<Instruction>) -> u32 {
    let mut current_node = start_node;

    let mut count = 0;

    loop {
        if is_ending_node(&current_node.borrow().name) {
            break;
        }

        let instruction = instructions.next();

        let left = match &(current_node.borrow().left) {
            None => panic!("hello"),
            Some(l) => Rc::clone(l),
        };
        let right = match &(current_node.borrow().right) {
            None => panic!("hello"),
            Some(r) => Rc::clone(r),
        };

        current_node = match instruction {
            Instruction::L => left,
            Instruction::R => right,
        };

        count += 1;
    }

    count
}

pub fn get_inputs(path: &str) -> String {
    fs::read_to_string(path).expect("Could not read file.")
}

fn is_starting_node(name: &str) -> bool {
    name.chars().any(|c| c == 'A')
}

fn is_ending_node(name: &str) -> bool {
    name.chars().any(|c| c == 'Z')
}
