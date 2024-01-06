pub trait Sequence {
    fn extrapolate(&mut self);
    fn get_last(&self) -> i32;
}
