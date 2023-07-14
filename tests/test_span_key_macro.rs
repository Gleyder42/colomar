use colomar_macros::SpanKey;

#[derive(SpanKey)]
struct HelloWorld {
    name: String,
    amount: i32,
}
