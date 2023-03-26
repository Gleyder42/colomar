use std::collections::HashMap;
use std::hash::Hash;

pub type Multimap<K, V> = HashMap<K, Vec<V>>;

pub trait MultimapExt<K, V> {

    fn multimap_push(&mut self, key: K, value: V) ;
}

impl<K, V> MultimapExt<K, V> for HashMap<K, Vec<V>>
where K: Eq + PartialEq + Hash + Clone
{

    fn multimap_push(&mut self, key: K, value: V) {
        let values = self.get_mut(&key);
        let mut values = match values {
            Some(values) => values,
            None => {
                self.insert(key.clone(), Vec::new());
                self.get_mut(&key).unwrap()
            }
        };

        values.push(value);
    }
}