use chumsky::Parser;
use compiler::language::lexer::{lexer, Token};
use compiler::span::StringInterner;
use compiler::test_assert::assert_vec;
use test_common::{TestDatabase, TestDatabaseHelper};

#[test]
#[should_panic]
fn test_end_is_consumed() {
    let interner = TestDatabase::default();
    let span_source_id = interner.intern_str("test_end_is_consumed");

    let code = "hello test νρσ";
    let _ = lexer(span_source_id, &interner).parse(code).unwrap();
}

#[test]
fn test_number_lexer() {
    let code = "1 5 1.2 123.321";
    let interner = TestDatabase::default();
    let span_source_id = interner.intern_str("test_end_is_consumed");

    let actual = lexer(span_source_id, &interner).parse(code).unwrap();
    let expected = vec![
        Token::Num(interner.intern_string("1".to_string())),
        Token::Num(interner.intern_string("5".to_string())),
        Token::Num(interner.intern_string("1.2".to_string())),
        Token::Num(interner.intern_string("123.321".to_string())),
    ];

    assert_vec(
        &actual.into_iter().map(|i| i.0).collect::<Vec<_>>(),
        &expected,
    );
}

#[test]
fn test_string_lexer() {
    let code = "\"Hello\" \"Hello World\"";
    let interner = TestDatabase::default();
    let span_source_id = interner.intern_str("test_end_is_consumed");

    let actual = lexer(span_source_id, &interner).parse(code).unwrap();
    let expected = vec![
        Token::String(interner.intern_string("Hello".to_string())),
        Token::String(interner.intern_string("Hello World".to_string())),
    ];

    assert_vec(
        &actual.into_iter().map(|i| i.0).collect::<Vec<_>>(),
        &expected,
    );
}

#[test]
fn test_keyword_lexer() {
    let code = "rule cond native event enum by partial struct getvar setvar val var fn type pub";
    let interner = TestDatabase::default();
    let span_source_id = interner.intern_str("test_end_is_consumed");

    let actual = lexer(span_source_id, &interner).parse(code).unwrap();
    let expected = vec![
        Token::Rule,
        Token::Cond,
        Token::Native,
        Token::Event,
        Token::Enum,
        Token::By,
        Token::Partial,
        Token::Struct,
        Token::GetVar,
        Token::SetVar,
        Token::Val,
        Token::Var,
        Token::Fn,
        Token::Type,
        Token::Pub,
    ];

    assert_vec(
        &actual.into_iter().map(|i| i.0).collect::<Vec<_>>(),
        &expected,
    );
}
