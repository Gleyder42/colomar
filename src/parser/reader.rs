use std::borrow::Borrow;
use std::cell::RefCell;
use std::io::Read;
use std::ops::{Add, AddAssign};

pub struct Reader<'a, T> {
    backing_vec: &'a Vec<T>,
    current_pos: RefCell<usize>
}

impl<'a, T> Reader<'a, T> {

    pub fn new(inner: &'a Vec<T>) -> Reader<T> {
        return Reader { backing_vec: inner, current_pos: RefCell::new(0) }
    }

    pub fn peek(&self) -> Option<&T> {
        return self.peek_nth(0);
    }

    pub fn peek_nth(&self, pos: usize) -> Option<&T> {
        self.backing_vec.get(*self.current_pos.borrow() + pos)
    }

    pub fn consume(&self) -> Option<&T> {
        self.consume_nth(0)
    }

    pub fn consume_nth(&self, pos: usize) -> Option<&T> {
        let value = self.backing_vec.get(*self.current_pos.borrow() + pos);
        self.current_pos.borrow_mut().add_assign(pos + 1);
        return value;
    }
}