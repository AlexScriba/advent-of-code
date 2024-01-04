use crate::{instruction::Instruction, network::Network};

pub fn parse_network(lines: &[&str]) -> Network {
    let network_lines: Vec<(String, (String, String))> =
        lines.iter().map(|l| destructure_for_network(l)).collect();

    let mut network = Network::new();
    network.insert_nodes(&network_lines);

    network
}

fn destructure_for_network(line: &str) -> (String, (String, String)) {
    let parts: Vec<&str> = line.split(" = ").collect();

    let name = parts[0];

    let padded_children: Vec<&str> = parts[1].split(", ").collect();
    let left = &padded_children[0][1..];
    let right = &padded_children[1][..3];

    (name.to_string(), (left.to_string(), right.to_string()))
}

pub fn parse_instructions(line: &str) -> Vec<Instruction> {
    let instructions = line.split("").filter(|c| !c.is_empty());

    instructions.map(|c| Instruction::new(c.trim())).collect()
}

#[cfg(test)]
mod parsing_tests {
    use super::*;
    use Instruction::*;

    #[test]
    fn parses_instructions() {
        let expected_instructions = vec![L, R];

        let input = "LR";

        let result = parse_instructions(&input);

        assert_eq!(expected_instructions, result);
    }

    #[test]
    fn destructures_nodes_correctly() {
        let expected = ("AAA".to_string(), ("BBB".to_string(), "CCC".to_string()));

        let input = "AAA = (BBB, CCC)";

        let result = destructure_for_network(&input);

        assert_eq!(expected, result);
    }
}
