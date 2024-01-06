use anyhow::anyhow;
use chumsky::input::Stream;
use chumsky::prelude::{end, Input};
use chumsky::Parser;
use compiler::analysis::interner::Interner;
use compiler::assert_iterator;
use compiler::cst::{Call, DeclArg, Expr, Rule};
use compiler::language::lexer::{lexer, Token};
use compiler::language::parser::{
    chain, declared_arg, expression, r#enum, rule, ParserExtra, ParserInput,
};
use compiler::span::{CopyRange, Offset, Span, SpanSourceId, Spanned};
use serde::Deserialize;
use std::fmt::Debug;
use std::fs;
use std::path::PathBuf;
use test_common::TestDatabase;

#[derive(Debug)]
struct ResKey {
    directory: PathBuf,
    name: &'static str,
}

impl ResKey {
    fn new(
        base: impl Into<PathBuf>,
        directory: impl Into<PathBuf>,
        name: impl Into<&'static str>,
    ) -> ResKey {
        ResKey {
            directory: base.into().join(directory.into()),
            name: name.into(),
        }
    }

    fn whole_path(&self) -> PathBuf {
        self.directory.join(self.name).with_extension("yaml")
    }
}

#[derive(Debug, Deserialize)]
struct BaseTestData<T> {
    code: String,
    expected: Option<T>,
}

#[derive(Debug, Deserialize)]
struct EnumTestData {
    #[serde(default)]
    is_native: bool,
    #[serde(default)]
    constants: Vec<String>,
}

type VarIdent = VarLen<IdentTestData>;

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum VarLen<T> {
    Single(T),
    Many(Vec<T>),
}

impl<T> VarLen<T> {
    fn len(&self) -> usize {
        match self {
            VarLen::Single(_) => 1,
            VarLen::Many(many) => many.len(),
        }
    }
}

impl<T> IntoIterator for VarLen<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            VarLen::Single(single) => vec![single].into_iter(),
            VarLen::Many(many) => many.into_iter(),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum IdentTestData {
    Ident(String),
    IdentWithArgs { ident: String, args: Vec<VarIdent> },
}

#[derive(Default, Debug, Deserialize)]
struct CallChainTestData {
    idents: Vec<IdentTestData>,
}

#[derive(Debug, Deserialize)]
struct RuleTestData {
    name: String,
    event: String,

    #[serde(default)]
    args: Vec<VarIdent>,
}

#[derive(Debug, Deserialize)]
struct DeclArgsTestData {
    args: Vec<DeclArgTestData>,
}

#[derive(Debug, Deserialize)]
enum ExprTestData {
    Chain(CallChainTestData),
    Neg {
        value: Box<ExprTestData>,
    },
    And {
        lhs: Box<ExprTestData>,
        rhs: Box<ExprTestData>,
    },
    Or {
        lhs: Box<ExprTestData>,
        rhs: Box<ExprTestData>,
    },
}

#[derive(Debug, Deserialize)]
struct DeclArgTestData {
    name: String,
    r#type: VarLen<String>,
    default: Option<CallChainTestData>,
}

fn read_test_data<T: for<'a> serde::Deserialize<'a>>(
    path_buf: &PathBuf,
) -> anyhow::Result<Vec<BaseTestData<T>>> {
    let input = fs::read_to_string(path_buf)?;
    let vec = input
        .split("---")
        .enumerate()
        .filter(|(_, it)| !it.is_empty())
        .map(|(index, it)| serde_yaml::from_str(it).expect(&format!("{index}: Cannot read {it}")))
        .collect::<Vec<_>>();
    Ok(vec)
}

fn parse_code<'src, T>(
    span_source_id: SpanSourceId,
    interner: &dyn Interner,
    code: &str,
    parser: &impl Parser<'src, ParserInput, T, ParserExtra<'src>>,
) -> anyhow::Result<T> {
    let tokens: Vec<_> = lex_code(span_source_id, interner, code)?;
    let eoi = Span::new(
        span_source_id,
        Offset::from(CopyRange::from(tokens.len()..tokens.len() + 1)),
    );
    let stream = Stream::from_iter(tokens.into_iter()).spanned(eoi);

    parser
        .then_ignore(end())
        .parse(stream)
        .into_result()
        .map_err(|_| anyhow!("Cannot parse '{code}'"))
}

fn lex_code(
    span_source_id: SpanSourceId,
    interner: &dyn Interner,
    code: &str,
) -> anyhow::Result<Vec<(Token, Span)>> {
    lexer(span_source_id, interner)
        .parse(code)
        .into_result()
        .map_err(|errors| {
            let error_message = errors
                .into_iter()
                .map(|it| it.to_string())
                .collect::<Vec<String>>()
                .join("\n");
            anyhow!(error_message)
        })
}

fn test_parser_result<'src, Ex, Ac, F>(
    key: &ResKey,
    should_panic: bool,
    parser: impl Parser<'src, ParserInput, Ac, ParserExtra<'src>>,
    assertion: F,
    interner: &dyn Interner,
) where
    Ac: Debug,
    Ex: for<'a> serde::Deserialize<'a>,
    F: Fn(Ex, Ac),
{
    let path_buf = key.whole_path();
    let data = read_test_data(&path_buf).unwrap();
    let span_source_id = interner.intern_span_source(path_buf);

    let results = data
        .into_iter()
        .map(|test_data| {
            let code = parse_code::<Ac>(span_source_id, interner, &test_data.code, &parser);
            let code = match code {
                Ok(code) => code,
                Err(error) => return Err(error),
            };

            if let Some(expected) = test_data.expected {
                assertion(expected, code);
            }
            Ok(())
        })
        .collect::<Vec<_>>();

    let error_messages = results
        .iter()
        .map(|it| match it {
            Ok(_) => "Successful".to_string(),
            Err(error) => format!("Error: {:?}", error),
        })
        .collect::<Vec<_>>()
        .join("\n");

    println!("{}", error_messages);
    if should_panic {
        if results.iter().any(|it| it.is_ok()) {
            panic!("There were some successful tests, but were expected to fail");
        }
    } else {
        if results.iter().any(|it| it.is_err()) {
            panic!("There were some failed tests, but were expected to pass")
        }
    }
}

fn nothing<A, B>(_: A, _: B) {}

const TEST_DIR: &str = "../resources/test/parser";

fn assert_call_chain(
    expected: impl IntoIterator<Item = IdentTestData>,
    actual: impl IntoIterator<Item = Box<Call>>,
    db: &dyn Interner,
) {
    actual.into_iter()
        .zip(expected)
        .for_each(|(call, test_data)| {
            match *call {
                Call::IdentArgs { name: actual_ident, args: actual_args, .. } => {
                    match test_data {
                        IdentTestData::IdentWithArgs { ident: expected_ident, args: expected_args } => {
                            assert_eq!(expected_ident, actual_ident.value.name(db), "Check if idents are equal");
                            expected_args.into_iter()
                                .zip(actual_args)
                                .for_each(|(expected, actual)| {
                                    assert_call_chain(expected, actual.call_chain(), db);
                                })
                        }
                        IdentTestData::Ident(expected) => {
                            panic!("The actual ident was {actual_ident:?} with {actual_args:?}, but an ident {expected:?} was expected");
                        }
                    }
                }
                Call::Ident(actual_ident) => {
                    match test_data {
                        IdentTestData::Ident(expected_ident) => {
                            assert_eq!(expected_ident, actual_ident.value.name(db), "Check if idents are equal")
                        }
                        IdentTestData::IdentWithArgs { ident: expected_ident, args: expected_args } => {
                            panic!("The actual ident was {actual_ident:?} but expected was an ident {expected_ident:?} with args {expected_args:?}")
                        }
                    }
                }
                Call::String(actual_ident, _) | Call::Number(actual_ident, _) => {
                    match test_data {
                        IdentTestData::Ident(expected_ident) => {
                            assert_eq!(expected_ident, actual_ident.name(db), "Check if idents are equal")
                        }
                        IdentTestData::IdentWithArgs { ident: expected_ident, args: expected_args } => {
                            panic!("The actual ident was {actual_ident:?} but expected was an ident {expected_ident:?} with args {expected_args:?}")
                        }
                    }
                }
            }
        })
}

#[test]
fn test_valid_enum() {
    let key = ResKey::new(TEST_DIR, "enum", "valid");
    let db = TestDatabase::default();

    test_parser_result(
        &key,
        false,
        r#enum(),
        |expected: EnumTestData, actual| {
            assert_eq!(expected.is_native, actual.decl.is_native.is_some());
            assert_eq!(
                expected.constants,
                actual
                    .def
                    .constants
                    .into_iter()
                    .map(|it| it.value.name(&db))
                    .collect::<Vec<_>>()
            );
        },
        &db,
    )
}

#[test]
fn test_invalid_enum() {
    let key = ResKey::new(TEST_DIR, "enum", "invalid");
    let db = TestDatabase::default();

    test_parser_result(&key, true, r#enum(), nothing::<EnumTestData, _>, &db);
}

#[test]
fn test_valid_ident_chain() {
    let key = ResKey::new(TEST_DIR, "ident_chain", "valid");
    let db = TestDatabase::default();

    test_parser_result(
        &key,
        false,
        chain().ident_chain(),
        |expected: CallChainTestData, actual| {
            assert_call_chain(expected.idents, actual, &db);
        },
        &db,
    );
}

#[test]
fn test_invalid_ident_chain() {
    let key = ResKey::new(TEST_DIR, "ident_chain", "invalid");
    let db = TestDatabase::default();

    test_parser_result(
        &key,
        true,
        chain().ident_chain(),
        nothing::<CallChainTestData, _>,
        &db,
    );
}

#[test]
fn test_valid_rule() {
    let key = ResKey::new(TEST_DIR, "rule", "valid");
    let db = TestDatabase::default();

    test_parser_result(
        &key,
        false,
        rule(),
        |expected: RuleTestData, actual: Rule| {
            assert_eq!(expected.name, actual.name.value.name(&db));
            assert_eq!(expected.event, actual.event.value.name(&db));
            expected
                .args
                .into_iter()
                .zip(actual.args)
                .for_each(|(actual, expected)| {
                    assert_call_chain(actual, expected.call_chain(), &db)
                })
        },
        &db,
    );
}

#[test]
fn test_invalid_decl_args() {
    let key = ResKey::new(TEST_DIR, "decl_args", "invalid");
    let db = TestDatabase::default();

    test_parser_result(
        &key,
        true,
        declared_arg(),
        nothing::<DeclArgsTestData, _>,
        &db,
    )
}

#[test]
fn test_valid_expr() {
    let key = ResKey::new(TEST_DIR, "expr", "valid");
    let db = TestDatabase::default();

    fn assertion(expected: ExprTestData, actual: Expr, db: &dyn Interner) {
        match (expected, actual) {
            (ExprTestData::Chain(expected), Expr::Chain(actual)) => {
                assert_call_chain(expected.idents.into_iter(), actual, db);
            }
            (
                ExprTestData::And {
                    lhs: ex_lhs,
                    rhs: ex_rhs,
                },
                Expr::And(ac_lhs, ac_rhs),
            ) => {
                assertion(*ex_lhs, *ac_lhs, db);
                assertion(*ex_rhs, *ac_rhs, db);
            }
            (
                ExprTestData::Or {
                    lhs: ex_lhs,
                    rhs: ex_rhs,
                },
                Expr::Or(ac_lhs, ac_rhs),
            ) => {
                assertion(*ex_lhs, *ac_lhs, db);
                assertion(*ex_rhs, *ac_rhs, db);
            }
            (ExprTestData::Neg { value: ex }, Expr::Neg(ac)) => {
                assertion(*ex, *ac, db);
            }
            (expected, actual) => {
                assert!(
                    false,
                    "Actual {actual:#?} and expected {expected:#?} are different"
                )
            }
        }
    }

    test_parser_result(
        &key,
        false,
        expression(),
        |expected, actual| assertion(expected, actual, &db),
        &db,
    );
}

#[test]
fn test_valid_decl_args() {
    let key = ResKey::new(TEST_DIR, "decl_args", "valid");
    let db = TestDatabase::default();

    test_parser_result(
        &key,
        false,
        declared_arg(),
        |expected: DeclArgsTestData, actual: Spanned<Vec<DeclArg>>| {
            expected
                .args
                .into_iter()
                .zip(actual)
                .for_each(|(expected, actual)| {
                    assert_eq!(expected.name, actual.name.value.name(&db));

                    let actual_types = actual
                        .types
                        .into_iter()
                        .map(|it| it.ident.value.name(&db))
                        .collect::<Vec<_>>();

                    assert_iterator!(expected.r#type, actual_types);
                    if let (Some(expected), Some(actual)) = (expected.default, actual.default_value)
                    {
                        assert_call_chain(expected.idents, actual, &db)
                    }
                })
        },
        &db,
    )
}
