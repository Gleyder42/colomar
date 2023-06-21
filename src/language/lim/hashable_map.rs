use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::marker::PhantomData;

use crate::language::HashableHashMap;
use serde::de::{Deserialize, Deserializer, MapAccess, Visitor};

// A Visitor is a type that holds methods that a Deserializer can drive
// depending on what is contained in the input data.
//
// In the case of a map we need generic type parameters K and V to be
// able to set the output type correctly, but don't require any state.
// This is an example of a "zero sized type" in Rust. The PhantomData
// keeps the compiler from complaining about unused generic type
// parameters.
struct MyMapVisitor<K, V>
where
    K: Hash + PartialEq + Eq + Ord,
    V: Hash + PartialEq,
{
    marker: PhantomData<fn() -> HashableHashMap<K, V>>,
}

impl<K, V> MyMapVisitor<K, V>
where
    K: Hash + PartialEq + Eq + Ord,
    V: Hash + PartialEq,
{
    fn new() -> Self {
        MyMapVisitor {
            marker: PhantomData,
        }
    }
}

// This is the trait that Deserializers are going to be driving. There
// is one method for each type of data that our type knows how to
// deserialize from. There are many other methods that are not
// implemented here, for example deserializing from integers or strings.
// By default those methods will return an error, which makes sense
// because we cannot deserialize a MyMap from an integer or string.
impl<'de, K, V> Visitor<'de> for MyMapVisitor<K, V>
where
    K: Hash + PartialEq + Eq + Ord + Deserialize<'de>,
    V: Hash + PartialEq + Deserialize<'de>,
{
    // The type that our Visitor is going to produce.
    type Value = HashableHashMap<K, V>;

    // Format a message stating what data this Visitor expects to receive.
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("Expected a hash map")
    }

    // Deserialize MyMap from an abstract "map" provided by the
    // Deserializer. The MapAccess input is a callback provided by
    // the Deserializer to let us see each entry in the map.
    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut map = HashMap::with_capacity(access.size_hint().unwrap_or(0));

        // While there are entries remaining in the input, add them
        // into our map.
        while let Some((key, value)) = access.next_entry()? {
            map.insert(key, value);
        }

        Ok(HashableHashMap(map))
    }
}

// This is the trait that informs Serde how to deserialize MyMap.
impl<'de, K, V> Deserialize<'de> for HashableHashMap<K, V>
where
    K: Hash + PartialEq + Eq + Ord + Deserialize<'de>,
    V: Hash + PartialEq + Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // Instantiate our Visitor and ask the Deserializer to drive
        // it over the input data, resulting in an instance of MyMap.
        deserializer.deserialize_map(MyMapVisitor::new())
    }
}
