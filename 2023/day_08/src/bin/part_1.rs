use std::env;
use std::fs;
use std::rc::Rc;

use day_08::cycler::Cycler;
use day_08::instruction::Instruction;
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

    let mut current_node = network.get_node("AAA");
    let end_node = network.get_node("ZZZ");

    let mut count = 0;

    loop {
        if current_node == end_node {
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

    println!("{}", count);
}

pub fn get_inputs(path: &str) -> String {
    fs::read_to_string(path).expect("Could not read file.")
}
