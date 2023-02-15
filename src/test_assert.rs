use std::fmt::Debug;

pub fn compare_vec<T: PartialEq>(a: &Vec<T>, b: &Vec<T>) -> bool {
    let matching = a.iter().zip(b.iter()).filter(|&(a, b)| a == b).count();
    matching == a.len() && matching == b.len()
}

pub fn assert_vec<T: PartialEq + Debug>(a: &Vec<T>, b: &Vec<T>) {
    a.iter()
        .zip(b)
        .for_each(|(actual, expected)| {
            assert_eq!(actual, expected, "Test if {:?} is equal to {:?}", actual, expected)
        })
}