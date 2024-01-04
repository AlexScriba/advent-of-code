use std::cell::RefCell;

pub struct Cycler<T>{
    index: RefCell<usize>,
    items: Vec<T>
}

impl<T> Cycler<T> {
    pub fn new(items: Vec<T>) -> Cycler<T> {
        Cycler { index: RefCell::new(0), items}
    }

    pub fn next(&self) -> &T {
        let index = self.index.borrow().clone();
        let item = &self.items[index];

        *self.index.borrow_mut() = (index + 1)%(self.items.len());

        item
    }
}

#[cfg(test)]
mod cycler_test {
    use super::*;

    #[test]
    fn works() {
        let cycler = Cycler::new(vec![1, 2, 3]);

        assert_eq!(1, *cycler.next());
        assert_eq!(2, *cycler.next());
        assert_eq!(3, *cycler.next());
    }
}
