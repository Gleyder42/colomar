use rustc_hash::FxHasher;
use std::fmt::{Display, Formatter};
use std::hash::Hasher;
use std::rc::Rc;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct HierOffset {
    parent: Option<Rc<HierOffset>>,
    value: u64,
}

impl Display for HierOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = self
            .iter()
            .map(|it| format!("{:#X}", it.value))
            .collect::<Vec<String>>()
            .join(">>>");
        f.write_str(&string)
    }
}

impl HierOffset {
    pub fn new_root(name: &impl AsRef<[u8]>) -> Rc<HierOffset> {
        Rc::new(Self::new(None, name))
    }

    pub fn new_with(name: &impl AsRef<[u8]>) -> HierOffset {
        Self::new(None, name)
    }

    fn from_parent(parent: Rc<Self>, name: &impl AsRef<[u8]>) -> Self {
        Self::new(Some(parent), name)
    }

    fn new(parent: Option<Rc<Self>>, name: &impl AsRef<[u8]>) -> Self {
        Self {
            parent,
            value: {
                let mut hasher = FxHasher::default();
                hasher.write(name.as_ref());
                hasher.finish()
            },
        }
    }

    /// Appends a name the the offset.  
    ///
    /// ## Important
    /// This methods expects self to be a reference to [Rc],
    /// because in the [crate::compiler::language::parser] your type is usually wrapped
    /// inside an [Rc]. It is usually a reference, because it is used within a [Fn] closure.
    fn refine(self: &Rc<Self>, name: &impl AsRef<[u8]>) -> Rc<Self> {
        Rc::new(Self::from_parent(self.clone(), name))
    }

    /// Panics if there is already a parent
    pub fn append_parent(mut self, parent: Self) -> Self {
        if let None = self.parent {
            self.parent = Some(Rc::new(parent));
            self
        } else {
            panic!("{} has already a parent", self)
        }
    }

    pub fn append(&self, name: &impl AsRef<[u8]>) -> Self {
        Self::from_parent(Rc::new(self.clone()), name)
    }

    pub fn same(&self) -> Self {
        self.clone()
    }

    fn iter(&self) -> HierarchicalOffsetIter {
        HierarchicalOffsetIter(Some(self))
    }
}

struct HierarchicalOffsetIter<'a>(Option<&'a HierOffset>);

impl<'a> Iterator for HierarchicalOffsetIter<'a> {
    type Item = &'a HierOffset;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.map(|it| {
            self.0 = it.parent.as_deref();
            it
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn test_size() {
        let location = HierOffset::new_root().refine(&"test").refine(&"b");

        println!("{:?}", location);
        println!("{}", size_of::<HierOffset>());
    }
}
