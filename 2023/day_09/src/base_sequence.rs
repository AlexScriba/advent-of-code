use crate::sequence::Sequence;

#[derive(Debug, Eq, PartialEq)]
pub struct BaseSequence{
    sequence: Vec<i32>
}

impl Sequence for BaseSequence {
    fn extrapolate(&mut self){

        if is_equal_seq(&self.sequence) {
            self.sequence.push(self.sequence[0]);
            return;
        }

        let mut diff_seq = BaseSequence::new(seq_diff(&self.sequence));
        diff_seq.extrapolate();

        self.set_extrapolated_val(&diff_seq.sequence);
    }

    fn get_last(&self) -> i32 {
        *self.sequence.last().expect("Could not get last.")
    }
}

impl BaseSequence {
    pub fn new(sequence: Vec<i32>) -> BaseSequence {
        BaseSequence { sequence }
    }

    fn get_extrapolated_val(&self, diff_seq: &Vec<i32>) -> i32 {
        self.sequence.last().unwrap() + diff_seq.last().unwrap()
    }

    fn set_extrapolated_val(&mut self, diff_seq: &Vec<i32>) {
        if diff_seq.len() != self.sequence.len() {
            panic!("Diff Seq was not extrapolated yet.");
        }

        self.sequence.push(self.get_extrapolated_val(diff_seq));
    }
}

pub fn seq_diff(seq: &Vec<i32>) -> Vec<i32> {
    let mut diff_seq: Vec<i32> = vec![];

    for i in 1..seq.len() {
        let difference = seq[i] - seq[i-1];
        diff_seq.push(difference);
    }

    diff_seq
}

pub fn is_zero_seq(seq: &Vec<i32>) -> bool {
    seq.iter().all(|&n| n == 0)
}

pub fn is_equal_seq(seq: &Vec<i32>) -> bool {
    let first = &seq[0];
    seq.iter().all(|n| n == first)
}

#[cfg(test)]
mod base_sequence_tests {
    use super::*;

    #[test]
    fn extrapolate_works_on_linear() {
        let expected = vec![1,2,3,4];

        let mut start_seq = BaseSequence::new(vec![1,2,3]);
        start_seq.extrapolate();

        assert_eq!(expected, start_seq.sequence);
    }

    #[test]
    fn extrapolate_works_on_complex() {
        let expected = vec![1,2,4,7, 11];

        let mut start_seq = BaseSequence::new(vec![1,2,4,7]);
        start_seq.extrapolate();

        assert_eq!(expected, start_seq.sequence);
    }

}
