pub struct Interner<T> {
    content: Vec<T>
}

impl<T> Interner<T> {

    pub fn new() -> Self {
        Interner { content: Vec::new() }
    }

    pub fn intern(&mut self, value: T) -> usize {
        self.content.push(value);
        self.content.len() - 1
    }

    pub fn lookup(&self, id: usize) -> &T {
        &self.content[id]
    }
}