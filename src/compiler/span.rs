use crate::compiler::{SpanLocation, Text};
use hash_hasher::HashedMap;
use smallvec::SmallVec;
use std::borrow::Cow;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

pub type Spacer = &'static Segment;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Segment(Text);

impl Segment {
    pub fn new(name: impl Into<Text>) -> Segment {
        Segment(name.into())
    }

    pub const fn spacer(name: &'static str) -> Segment {
        Segment(Text::new_inline(name))
    }
}

impl Display for Segment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct HierarchicalLocation {
    segments: SmallVec<[Cow<'static, Segment>; 8]>,
}

impl From<&'static str> for Spacer {
    fn from(value: &'static str) -> Self {
        &Segment::spacer(value)
    }
}

impl HierarchicalLocation {
    pub fn new() -> HierarchicalLocation {
        HierarchicalLocation {
            segments: SmallVec::new(),
        }
    }

    pub fn push_other(&mut self, mut location: HierarchicalLocation) {
        self.segments.append(&mut location.segments);
    }

    pub fn append_spacer(mut self, space: Spacer) -> Self {
        self.push_spacer(space);
        self
    }

    pub fn append(mut self, segment: Segment) -> Self {
        self.push(segment);
        self
    }

    pub fn push(&mut self, segment: Segment) {
        self.segments.push(Cow::Owned(segment));
    }

    pub fn push_spacer(&mut self, spacer: Spacer) {
        self.segments.push(Cow::Borrowed(spacer))
    }
}

impl Display for HierarchicalLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = self
            .segments
            .iter()
            .map(|segment| segment.to_string())
            .collect::<Vec<String>>()
            .join("/");
        f.write_str(&string)
    }
}

#[cfg(test)]
mod tests {
    static RULE_SPACE: Segment = Segment::spacer("rule");
    static COND_SPACE: Segment = Segment::spacer("cond");

    use super::*;
    use std::mem::size_of;

    #[test]
    fn test_segment_display() {
        let mut location = HierarchicalLocation::new();
        location.push_spacer(&RULE_SPACE);
        location.push(Segment::new("OngoingEachPlayer"));
        location.push_spacer(&COND_SPACE);
        location.push(Segment::new("player"));
        location.push(Segment::new("isReloading"));

        println!("{}", location);
    }
}
