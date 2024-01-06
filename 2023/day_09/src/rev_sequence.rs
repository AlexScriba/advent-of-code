use crate::{base_sequence::BaseSequence, sequence::Sequence};

pub struct RevSequence {
    sequence: BaseSequence
}

impl RevSequence {
    pub fn new(mut seq: Vec<i32>) -> RevSequence {
        seq.reverse();
        RevSequence { sequence: BaseSequence::new(seq) }
    }
}

impl Sequence for RevSequence {
    fn extrapolate(&mut self) {
        self.sequence.extrapolate();
    }

    fn get_last(&self) -> i32 {
        self.sequence.get_last()
    }
}
