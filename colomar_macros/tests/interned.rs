use colomar_macros::Interned;

#[derive(Interned)]
struct MyStruct {
    name: String,
    amount: i32,
}

#[test]
fn test_interned_id_exist() {
    MyStructId(salsa::InternId::from(10_u32));
    // If this test compiles, it also passes
    assert!(true);
}
