use crate::compiler::cst::*;
use crate::compiler::language::lexer::Token;
use crate::compiler::{AssignMod, Ident, Text, UseRestriction};
use chumsky::error::Error;
use chumsky::input::{SpannedInput, Stream};

use crate::compiler::span::{Span, Spanned, SpannedBool};
use chumsky::prelude::*;
use chumsky::util::Maybe;
use smallvec::SmallVec;

pub type ParserInput = SpannedInput<Token, Span, Stream<std::vec::IntoIter<(Token, Span)>>>;
pub type ParserExtra<'a> = extra::Err<Rich<'a, Token, Span>>;

fn ident<'src>() -> impl Parser<'src, ParserInput, Ident, ParserExtra<'src>> + Clone {
    let ident = |name| Token::Ident(Text::from(name));

    any().try_map(move |token, span| match token {
        Token::Ident(ident) => Ok(Ident { value: ident, span }),
        _ => Err(<Rich<_, _> as Error<ParserInput>>::expected_found(
            vec![Some(Maybe::from(ident("ident")))],
            Some(Maybe::from(token)),
            span,
        )),
    })
}

fn declared_arguments<'src>(
) -> impl Parser<'src, ParserInput, Spanned<Vec<DeclaredArgument>>, ParserExtra<'src>> {
    let types = ident()
        .separated_by(just(Token::Ctrl('|')))
        .collect::<Vec<_>>()
        .map_with_span(|types, span| Types {
            values: types.into(),
            span,
        });
    let default_value = just(Token::Ctrl('='))
        .ignore_then(chain().ident_chain())
        .or_not();

    ident()
        .then_ignore(just(Token::Ctrl(':')))
        .then(types)
        .then(default_value)
        .map_with_span(|((name, types), default_value), span| (name, types, default_value, span))
        .separated_by(just(Token::Ctrl(',')))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .map(|arg_tuples| {
            arg_tuples
                .into_iter()
                .enumerate()
                .map(
                    |(position, (name, types, default_value, span))| DeclaredArgument {
                        position,
                        name,
                        types,
                        default_value,
                        span,
                    },
                )
                .collect::<Vec<_>>()
        })
        .map_with_span(Spanned::new)
}

fn native_or_not<'src>() -> impl Parser<'src, ParserInput, SpannedBool, ParserExtra<'src>> {
    just(Token::Native)
        .or_not()
        .map_with_span(Spanned::ignore_value)
}

fn event<'src>() -> impl Parser<'src, ParserInput, Event, ParserExtra<'src>> {
    let by = just(Token::By)
        .ignore_then(ident())
        .then(chain().args())
        .or_not();

    native_or_not()
        .then_ignore(just(Token::Event))
        .then(ident())
        .map_with_span(|(is_native, name), span| EventDeclaration {
            is_native,
            name,
            span,
        })
        .then(declared_arguments())
        .then(by)
        .validate(|((event_decl, decl_args), by), span, emitter| {
            if event_decl.is_native.is_some() && by.is_some() {
                emitter.emit(Rich::custom(
                    span,
                    "native functions cannot have a by clause",
                ));
            }
            ((event_decl, decl_args), by)
        })
        .then(block())
        .map_with_span(|(((declaration, arguments), by), block), span| Event {
            declaration,
            definition: EventDefinition {
                actions: block.actions,
                conditions: block.conditions,
                by,
                arguments: arguments.inner_into(),
            },
            span,
        })
}

fn r#enum<'src>() -> impl Parser<'src, ParserInput, Enum, ParserExtra<'src>> {
    let constants = ident()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .collect::<Vec<_>>();

    native_or_not()
        .then_ignore(just(Token::Enum))
        .then(ident())
        .map_with_span(|(is_native, name), span| EnumDeclaration {
            is_native,
            name,
            span,
        })
        .then(constants.delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))))
        .map_with_span(|(declaration, constants), span| Enum {
            declaration,
            definition: EnumDefinition { constants },
            span,
        })
}

fn property<'src>() -> impl Parser<'src, ParserInput, PropertyDeclaration, ParserExtra<'src>> {
    let use_restriction_tokens = (
        just(Token::GetVar),
        just(Token::SetVar),
        just(Token::Val),
        just(Token::Var),
    );
    let use_restriction = choice(use_restriction_tokens).map_with_span(Spanned::new);

    native_or_not()
        .then(use_restriction)
        .then(ident())
        .then_ignore(just(Token::Ctrl(':')))
        .then(ident())
        .map(|(((is_native, property_type), name), r#type)| {
            let use_restriction = match property_type {
                // Write a test which tries to put other tokens here
                Spanned {
                    value: Token::GetVar,
                    span,
                } => Spanned::new(UseRestriction::GetVar, span),
                Spanned {
                    value: Token::SetVar,
                    span,
                } => Spanned::new(UseRestriction::SetVar, span),
                Spanned {
                    value: Token::Var,
                    span,
                } => Spanned::new(UseRestriction::Var, span),
                Spanned {
                    value: Token::Val,
                    span,
                } => Spanned::new(UseRestriction::Val, span),
                _ => panic!(
                    "Compiler Error: Unexpected token as property type {:?}",
                    property_type
                ),
            };
            PropertyDeclaration {
                name,
                is_native,
                use_restriction,
                r#type,
            }
        })
}

fn expression<'src>() -> impl Parser<'src, ParserInput, Expr, ParserExtra<'src>> {
    recursive(|expr| {
        let chain = chain().ident_chain().map(Expr::Chain);

        let atom = chain.or(expr.delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))));

        let op = |c| just(Token::Ctrl(c));

        const DUP: usize = 2;
        let dup_op = |c| op(c).repeated().exactly(DUP).collect_exactly::<[_; DUP]>();

        let neg = op('!')
            .repeated()
            .foldr(atom, |_, rhs| Expr::Neg(Box::new(rhs)));

        let and = neg.clone().foldl(
            dup_op('&')
                .to(Expr::And as fn(_, _) -> _)
                .then(neg)
                .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        let or = and.clone().foldl(
            dup_op('|')
                .to(Expr::Or as fn(_, _) -> _)
                .then(and)
                .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        or
    })
}

fn open_or_not<'src>() -> impl Parser<'src, ParserInput, SpannedBool, ParserExtra<'src>> {
    just(Token::Open)
        .or_not()
        .map_with_span(Spanned::ignore_value)
}

fn r#struct<'src>() -> impl Parser<'src, ParserInput, Struct, ParserExtra<'src>> {
    let member_function = native_or_not()
        .then_ignore(just(Token::Fn))
        .then(ident())
        .then(declared_arguments())
        .map(|((is_native, name), arguments)| FunctionDeclaration {
            name,
            is_native,
            arguments: arguments.inner_into(),
        });

    enum StructMember {
        Property(PropertyDeclaration),
        Function(FunctionDeclaration),
    }

    let property = property().map(StructMember::Property);
    let member_function = member_function.map(StructMember::Function);

    open_or_not()
        .then(native_or_not())
        .then_ignore(just(Token::Struct))
        .then(ident())
        .map_with_span(|((is_open, is_native), name), span| StructDeclaration {
            is_open,
            is_native,
            name,
            span,
        })
        .then(
            choice((property, member_function))
                .then_ignore(just(Token::Ctrl(';')))
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))),
        )
        .map_with_span(|(declaration, members), span| {
            let mut functions: FunctionDecls = SmallVec::new();
            let mut properties: PropertyDecls = SmallVec::new();
            for member in members {
                match member {
                    StructMember::Function(function) => functions.push(function),
                    StructMember::Property(property) => properties.push(property),
                };
            }

            Struct {
                declaration,
                definition: StructDefinition {
                    properties,
                    functions,
                },
                span,
            }
        })
}

struct IdentChainParserResult<'src, 'a> {
    ident_chain: Boxed<'src, 'a, ParserInput, CallChain, ParserExtra<'src>>,
    args: Boxed<'src, 'a, ParserInput, CallArguments, ParserExtra<'src>>,
}

impl<'src, 'a> IdentChainParserResult<'src, 'a> {
    fn ident_chain(self) -> Boxed<'src, 'a, ParserInput, CallChain, ParserExtra<'src>> {
        self.ident_chain
    }

    fn args(self) -> Boxed<'src, 'a, ParserInput, CallArguments, ParserExtra<'src>> {
        self.args
    }
}

fn chain<'src: 'a, 'a>() -> IdentChainParserResult<'src, 'a> {
    let mut ident_chain = Recursive::declare();

    let arg_name_or_not = ident().then_ignore(just(Token::Ctrl('='))).or_not();
    let args = arg_name_or_not
        .then(ident_chain.clone())
        .map_with_span(|(named, call_chain), span| match named {
            Some(name) => CallArgument::Named(name, call_chain, span),
            None => CallArgument::Pos(call_chain),
        })
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .map_with_span(CallArguments::new);

    let literal = any().try_map(|token, span| match token {
        Token::String(string) => Ok(Box::new(Call::String(string, span))),
        Token::Num(number) => Ok(Box::new(Call::Number(number, span))),
        _ => {
            let literal = |func: fn(_) -> _, string| Some(Maybe::from(func(Text::from(string))));
            let expected = vec![literal(Token::String, "string"), literal(Token::Num, "num")];
            let found = Some(Maybe::from(token));

            let rich = <Rich<_, _> as Error<ParserInput>>::expected_found(expected, found, span);
            Err(rich)
        }
    });

    ident_chain.define(
        ident()
            .then(args.clone().or_not())
            .map_with_span(|(ident, arguments), span| {
                let call = match arguments {
                    Some(arguments) => Call::IdentArguments {
                        name: ident,
                        args: arguments,
                        span,
                    },
                    None => Call::Ident(ident),
                };
                Box::new(call)
            })
            .or(literal)
            .separated_by(just(Token::Ctrl('.')))
            .at_least(1)
            .collect()
            .map_with_span(CallChain::new),
    );

    IdentChainParserResult {
        ident_chain: ident_chain.boxed(),
        args: args.boxed(),
    }
}

fn assigment<'src>() -> impl Parser<'src, ParserInput, Action, ParserExtra<'src>> {
    let ident_chain = || chain().ident_chain();

    let assign_mods = (
        just(Token::Ctrl('+')),
        just(Token::Ctrl('-')),
        just(Token::Ctrl('*')),
        just(Token::Ctrl('/')),
    );
    let assign_mod = choice(assign_mods)
        .map(|token| match token {
            Token::Ctrl('+') => AssignMod::Add,
            Token::Ctrl('-') => AssignMod::Sub,
            Token::Ctrl('*') => AssignMod::Mul,
            Token::Ctrl('/') => AssignMod::Div,
            _ => unreachable!(),
        })
        .or_not();

    ident_chain()
        .then(assign_mod)
        .then_ignore(just(Token::Ctrl('=')))
        .then(ident_chain())
        .map(|((left, assign_mod), right)| Action::Assignment(left, right, assign_mod))
}

fn block<'src>() -> impl Parser<'src, ParserInput, Block, ParserExtra<'src>> {
    let cond = just(Token::Cond).ignore_then(expression());

    let action = choice((
        assigment(),
        chain().ident_chain().map(Action::CallChain),
        property().map(Action::Property),
    ));

    let conditions = cond
        .then_ignore(just(Token::Ctrl(';')))
        .repeated()
        .collect::<Vec<_>>();
    let actions = action
        .then_ignore(just(Token::Ctrl(';')))
        .repeated()
        .collect::<Vec<_>>();

    conditions
        .then(actions)
        .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
        .map_with_span(|(conditions, actions), span| Block {
            actions: actions.into(),
            conditions: conditions.into(),
            span,
        })
}

fn rule<'src>() -> impl Parser<'src, ParserInput, Rule, ParserExtra<'src>> {
    let rule_name = any().try_map(|token, span| match token {
        Token::String(string) => Ok(Spanned::new(string, span)),
        _ => {
            let expected = vec![Some(Maybe::from(Token::String(Text::from("rule name"))))];
            let found = Some(Maybe::from(token));

            let error = <Rich<_, _> as Error<ParserInput>>::expected_found(expected, found, span);
            Err(error)
        }
    });

    just(Token::Rule)
        .ignore_then(rule_name)
        .then(ident())
        .then(chain().args())
        .then(block())
        .map_with_span(|(((rule_name, ident), arguments), block), _span| Rule {
            conditions: block.conditions,
            actions: block.actions,
            name: rule_name,
            event: ident,
            arguments,
        })
}

pub fn parser<'src>() -> impl Parser<'src, ParserInput, Ast, ParserExtra<'src>> {
    let rule_parser = rule().map(Root::Rule);
    let event_parser = event().map(Root::Event);
    let enum_parser = r#enum().map(Root::Enum);
    let struct_parser = r#struct().map(Root::Struct);

    choice((rule_parser, event_parser, enum_parser, struct_parser))
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .map(Ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::assert_iterator;
    use crate::compiler::cst::{Call, DeclaredArgument, Rule};
    use crate::compiler::database::test::TestDatabase;
    use crate::compiler::language::lexer::{lexer, Token};
    use crate::compiler::span::{CopyRange, SpanInterner};
    use crate::compiler::span::{Span, SpanLocation, SpanSourceId, Spanned};
    use anyhow::anyhow;
    use chumsky::prelude::end;
    use chumsky::Parser;
    use serde::Deserialize;
    use std::fmt::Debug;
    use std::fs;
    use std::path::PathBuf;

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
            .map(|(index, it)| {
                serde_yaml::from_str(it).expect(&format!("{index}: Cannot read {it}"))
            })
            .collect::<Vec<_>>();
        Ok(vec)
    }

    fn parse_code<'src, T>(
        span_source_id: SpanSourceId,
        code: &str,
        parser: &impl Parser<'src, ParserInput, T, ParserExtra<'src>>,
    ) -> anyhow::Result<T> {
        let tokens: Vec<_> = lex_code(span_source_id, code)?;
        let eoi = Span::new(
            span_source_id,
            SpanLocation::from(CopyRange::from(tokens.len()..tokens.len() + 1)),
        );
        let stream = Stream::from_iter(tokens.into_iter()).spanned(eoi);

        parser
            .then_ignore(end())
            .parse(stream)
            .into_result()
            .map_err(|_| anyhow!("Cannot parse '{code}'"))
    }

    fn lex_code(span_source_id: SpanSourceId, code: &str) -> anyhow::Result<Vec<(Token, Span)>> {
        lexer(span_source_id)
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
    ) where
        Ac: Debug,
        Ex: for<'a> serde::Deserialize<'a>,
        F: Fn(Ex, Ac),
    {
        let interner = TestDatabase::default();
        let path_buf = key.whole_path();
        let data = read_test_data(&path_buf).unwrap();
        let span_source_id = interner.intern_span_source(path_buf.to_string_lossy().into());

        let results = data
            .into_iter()
            .map(|test_data| {
                let code = parse_code::<Ac>(span_source_id, &test_data.code, &parser);
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

    const TEST_DIR: &str = "res/test";

    fn assert_call_chain(
        expected: impl IntoIterator<Item = IdentTestData>,
        actual: impl IntoIterator<Item = Box<Call>>,
    ) {
        actual.into_iter()
            .zip(expected)
            .for_each(|(call, test_data)| {
                match *call {
                    Call::IdentArguments { name: actual_ident, args: actual_args, .. } => {
                        match test_data {
                            IdentTestData::IdentWithArgs { ident: expected_ident, args: expected_args } => {
                                assert_eq!(expected_ident, actual_ident.value, "Check if idents are equal");
                                expected_args.into_iter()
                                    .zip(actual_args)
                                    .for_each(|(expected, actual)| {
                                        assert_call_chain(expected, actual.call_chain());
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
                                assert_eq!(expected_ident, actual_ident.value, "Check if idents are equal")
                            }
                            IdentTestData::IdentWithArgs { ident: expected_ident, args: expected_args } => {
                                panic!("The actual ident was {actual_ident:?} but expected was an ident {expected_ident:?} with args {expected_args:?}")
                            }
                        }
                    }
                    Call::String(actual_ident, _) | Call::Number(actual_ident, _) => {
                        match test_data {
                            IdentTestData::Ident(expected_ident) => {
                                assert_eq!(expected_ident, actual_ident, "Check if idents are equal")
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

        test_parser_result(&key, false, r#enum(), |expected: EnumTestData, actual| {
            assert_eq!(expected.is_native, actual.declaration.is_native.is_some());
            assert_eq!(
                expected.constants,
                actual
                    .definition
                    .constants
                    .into_iter()
                    .map(|it| it.value)
                    .collect::<Vec<_>>()
            );
        })
    }

    #[test]
    fn test_invalid_enum() {
        let key = ResKey::new(TEST_DIR, "enum", "invalid");

        test_parser_result(&key, true, r#enum(), nothing::<EnumTestData, _>);
    }

    #[test]
    fn test_valid_ident_chain() {
        let key = ResKey::new(TEST_DIR, "ident_chain", "valid");

        test_parser_result(
            &key,
            false,
            chain().ident_chain(),
            |expected: CallChainTestData, actual| {
                assert_call_chain(expected.idents, actual);
            },
        );
    }

    #[test]
    fn test_invalid_ident_chain() {
        let key = ResKey::new(TEST_DIR, "ident_chain", "invalid");

        test_parser_result(
            &key,
            true,
            chain().ident_chain(),
            nothing::<CallChainTestData, _>,
        );
    }

    #[test]
    fn test_valid_rule() {
        let key = ResKey::new(TEST_DIR, "rule", "valid");

        test_parser_result(
            &key,
            false,
            rule(),
            |expected: RuleTestData, actual: Rule| {
                assert_eq!(expected.name, actual.name.value);
                assert_eq!(expected.event, actual.event.value);
                expected
                    .args
                    .into_iter()
                    .zip(actual.arguments)
                    .for_each(|(actual, expected)| assert_call_chain(actual, expected.call_chain()))
            },
        );
    }

    #[test]
    fn test_invalid_decl_args() {
        let key = ResKey::new(TEST_DIR, "decl_args", "invalid");

        test_parser_result(
            &key,
            true,
            declared_arguments(),
            nothing::<DeclArgsTestData, _>,
        )
    }

    #[test]
    fn test_valid_expr() {
        let key = ResKey::new(TEST_DIR, "expr", "valid");

        fn assertion(expected: ExprTestData, actual: Expr) {
            match (expected, actual) {
                (ExprTestData::Chain(expected), Expr::Chain(actual)) => {
                    assert_call_chain(expected.idents.into_iter(), actual);
                }
                (
                    ExprTestData::And {
                        lhs: ex_lhs,
                        rhs: ex_rhs,
                    },
                    Expr::And(ac_lhs, ac_rhs),
                ) => {
                    assertion(*ex_lhs, *ac_lhs);
                    assertion(*ex_rhs, *ac_rhs);
                }
                (
                    ExprTestData::Or {
                        lhs: ex_lhs,
                        rhs: ex_rhs,
                    },
                    Expr::Or(ac_lhs, ac_rhs),
                ) => {
                    assertion(*ex_lhs, *ac_lhs);
                    assertion(*ex_rhs, *ac_rhs);
                }
                (ExprTestData::Neg { value: ex }, Expr::Neg(ac)) => {
                    assertion(*ex, *ac);
                }
                (expected, actual) => {
                    assert!(
                        false,
                        "Actual {actual:#?} and expected {expected:#?} are different"
                    )
                }
            }
        }

        test_parser_result(&key, false, expression(), assertion);
    }

    #[test]
    fn test_valid_decl_args() {
        let key = ResKey::new(TEST_DIR, "decl_args", "valid");

        test_parser_result(
            &key,
            false,
            declared_arguments(),
            |expected: DeclArgsTestData, actual: Spanned<Vec<DeclaredArgument>>| {
                expected
                    .args
                    .into_iter()
                    .zip(actual)
                    .for_each(|(expected, actual)| {
                        assert_eq!(expected.name, actual.name.value);

                        let actual_types = actual
                            .types
                            .into_iter()
                            .map(|it| it.value)
                            .collect::<Vec<_>>();

                        assert_iterator!(expected.r#type, actual_types);
                        if let (Some(expected), Some(actual)) =
                            (expected.default, actual.default_value)
                        {
                            assert_call_chain(expected.idents, actual)
                        }
                    })
            },
        )
    }
}
