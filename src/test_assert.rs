pub fn compare_vec<T: PartialEq>(a: &Vec<T>, b: &Vec<T>) -> bool {
    let matching = a.iter().zip(b.iter()).filter(|&(a, b)| a == b).count();
    matching == a.len() && matching == b.len()
}

pub fn assert_into_iter<A: IntoIterator, B: IntoIterator>(a: A, b: B)
    where
        <A as IntoIterator>::Item: PartialEq<<B as IntoIterator>::Item>,
        <A as IntoIterator>::Item: std::fmt::Debug,
        <B as IntoIterator>::Item: std::fmt::Debug
{
    a.into_iter()
        .zip(b)
        .for_each(|(actual, expected)| {
            assert_eq!(actual, expected, "Test if {:?} is equal to {:?}", actual, expected)
        })
}