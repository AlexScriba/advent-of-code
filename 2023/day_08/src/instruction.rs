#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum Instruction {
    L,
    R,
}

impl Instruction {
    pub fn new(inst: &str) -> Instruction {
        match inst {
            "R" => Instruction::R,
            "L" => Instruction::L,
            _ => panic!("Unknown instruction: {}", inst),
        }
    }
}
