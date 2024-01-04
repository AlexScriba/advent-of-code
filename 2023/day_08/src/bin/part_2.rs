mod part_1;

use std::collections::HashSet;
use std::env;
use std::rc::Rc;

use day_08::cycler::Cycler;
use day_08::instruction::Instruction;
use day_08::network::Network;
use day_08::node::Node;
use day_08::parsing::parse_instructions;
use day_08::parsing::parse_network;
use part_1::get_inputs;

fn main() {
    let args: Vec<String> = env::args().collect();

    let file: String = get_inputs(&args[1]);
    let file_lines: Vec<&str> = file.lines().collect();

    let instruction_line = file_lines[0];
    let network_lines = &file_lines[2..];

    let instructions_list = parse_instructions(instruction_line);
    let instructions = Cycler::new(instructions_list);
    let network = parse_network(network_lines);

    let mut current_nodes: Vec<Node> = network
        .get_node_names()
        .iter()
        .filter(|n| is_starting_node(n))
        .map(|n| network.get_node(n))
        .collect();

    let end_nodes = filter_and_hash(&network, is_ending_node);

    let mut count = 0;

    println!("starting loop.");

    loop {
        if current_nodes
            .iter()
            .all(|n| end_nodes.contains(&n.borrow().name))
        {
            break;
        }

        let instruction = instructions.next();

        for i in 0..current_nodes.len() {
            let node = &current_nodes[i];

            let left = match &(node.borrow().left) {
                None => panic!("hello"),
                Some(l) => Rc::clone(l),
            };
            let right = match &(node.borrow().right) {
                None => panic!("hello"),
                Some(r) => Rc::clone(r),
            };

            current_nodes[i] = match instruction {
                Instruction::L => left,
                Instruction::R => right,
            }
        }

        count += 1;
        if count%100_000 == 0 {
            println!("{}", count);
        }
    }

    println!("{}", count);
}

fn filter_and_hash(network: &Network, filter_func: fn(&str) -> bool) -> HashSet<String> {
    HashSet::from_iter(
        network
            .get_node_names()
            .into_iter()
            .filter(|n| filter_func(&n))
            .map(|n| n.to_owned()),
    )
}

fn is_starting_node(name: &str) -> bool {
    name.chars().any(|c| c == 'A')
}

fn is_ending_node(name: &str) -> bool {
    name.chars().any(|c| c == 'Z')
}
