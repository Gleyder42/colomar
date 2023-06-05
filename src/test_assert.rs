pub fn assert_vec<T: PartialEq + std::fmt::Debug>(a: &Vec<T>, b: &Vec<T>) {
    a.iter().zip(b).for_each(|(actual, expected)| {
        assert_eq!(
            actual, expected,
            "Test if {:?} is equal to {:?}",
            actual, expected
        )
    })
}
