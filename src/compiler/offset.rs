use rustc_hash::FxHasher;
use std::fmt::{Display, Formatter};
use std::hash::Hasher;
use std::rc::Rc;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct HierarchicalOffset {
    parent: Option<Rc<HierarchicalOffset>>,
    value: u64,
}

impl Display for HierarchicalOffset {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = self
            .iter()
            .map(|it| format!("{:#X}", it.value))
            .collect::<Vec<String>>()
            .join(">>>");
        f.write_str(&string)
    }
}

impl HierarchicalOffset {
    pub fn new_root() -> Rc<HierarchicalOffset> {
        Rc::new(Self::new(None, &"root"))
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

    fn refine(self: Rc<Self>, name: &impl AsRef<[u8]>) -> Rc<Self> {
        Rc::new(Self::from_parent(self, name))
    }

    fn iter(&self) -> HierarchicalOffsetIter {
        HierarchicalOffsetIter(Some(self))
    }
}

struct HierarchicalOffsetIter<'a>(Option<&'a HierarchicalOffset>);

impl<'a> Iterator for HierarchicalOffsetIter<'a> {
    type Item = &'a HierarchicalOffset;

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
        let location = HierarchicalOffset::new_root().refine(&"test").refine(&"b");

        println!("{:?}", location);
        println!("{}", size_of::<HierarchicalOffset>());
    }
}
