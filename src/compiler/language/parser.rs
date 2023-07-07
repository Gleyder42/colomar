extern crate core;

use crate::compiler::cst::*;
use crate::compiler::language::lexer::Token;
use crate::compiler::{
    HierarchicalSpan, Ident, Span, SpanInterner, SpanSourceId, Spanned, SpannedBool, Text,
    UseRestriction,
};

use crate::compiler::span::{HierarchicalLocation, Segment};
use chumsky::prelude::*;
use smallvec::SmallVec;

pub type ParserError = Simple<Token, Span>;

fn ident(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
) -> impl Parser<Token, Ident, Error = ParserError> + Clone + '_ {
    static IDENT_SPACER: Segment = Segment::spacer("ident");

    filter_map(move |span: Span, token| match token {
        Token::Ident(ident) => Ok(Ident {
            value: ident.clone(),
            span: HierarchicalSpan::new(
                interner,
                span.source,
                parent.clone(),
                &IDENT_SPACER,
                Segment::new(ident),
            ),
        }),
        _ => Err(ParserError::expected_input_found(
            span,
            Vec::new(),
            Some(token),
        )),
    })
}

fn swap_params<A, B, C>(input: impl Fn(A, B) -> C) -> impl Fn(B, A) -> C {
    move |b, a| input(a, b)
}

fn map_with_location<T, U, In>(
    mut parent: HierarchicalLocation,
    segment: Segment,
    in_func: In,
) -> impl Fn(T, Span) -> U
where
    In: Fn(T, Span, HierarchicalLocation) -> U,
{
    parent.push(segment);
    let parent_cloner = move || parent.clone();

    move |value: T, span: Span| in_func(value, span, parent_cloner())
}

fn create_ignored_value<T>(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
    segment: Segment,
) -> impl Fn(Option<T>, Span) -> SpannedBool + '_ {
    let parent_cloner = move || parent.clone();
    let segment_cloner = move || segment.clone();

    move |value, span| {
        Spanned::ignore_value(
            value,
            HierarchicalSpan::overarching_from(span, interner, parent_cloner(), segment_cloner()),
        )
    }
}

fn create_spanned<T>(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
    segment: Segment,
) -> impl Fn(T, Span) -> Spanned<T> + Clone + '_ {
    let parent_cloner = move || parent.clone();
    let segment_cloner = move || segment.clone();

    move |value, span| Spanned {
        value,
        span: HierarchicalSpan::overarching_from(span, interner, parent_cloner(), segment_cloner()),
    }
}

fn declared_arguments(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
) -> impl Parser<Token, Spanned<Vec<DeclaredArgument>>, Error = ParserError> + '_ {
    static SPACER: Segment = Segment::spacer("decl_arg");
    static TYPE_SPACER: Segment = Segment::spacer("type");
    static DEFAULT_ARG_SPACER: Segment = Segment::spacer("default_arg");
    let parent = parent.append_spacer(&SPACER);

    ident(interner, parent.clone())
        .then_ignore(just(Token::Ctrl(':')))
        .then(
            ident(interner, parent.clone().append_spacer(&DEFAULT_ARG_SPACER))
                .separated_by(just(Token::Ctrl('|')))
                .map_with_span(map_with_location(
                    parent.clone(),
                    Segment::new("name"),
                    |types: Vec<Ident>, span, loc| Types {
                        values: types.into(),
                        span: HierarchicalSpan::from_location(interner, span, loc),
                    },
                )),
        )
        .then(
            just(Token::Ctrl('='))
                .ignore_then(
                    chain(interner, parent.clone().append_spacer(&DEFAULT_ARG_SPACER))
                        .ident_chain(),
                )
                .or_not(),
        )
        .map_with_span(|((name, types), default_value), span| (name, types, default_value, span))
        .separated_by(just(Token::Ctrl(',')))
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .map(|it| {
            it.into_iter()
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
        .map_with_span(create_spanned(
            interner,
            parent.clone(),
            Segment::new("all"),
        ))
}

fn native_or_not(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
) -> impl Parser<Token, SpannedBool, Error = ParserError> + '_ {
    just(Token::Native)
        .or_not()
        .map_with_span(create_ignored_value(
            interner,
            parent,
            Segment::new("native"),
        ))
}

fn event(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
) -> impl Parser<Token, Event, Error = ParserError> + '_ {
    static SPACER: Segment = Segment::spacer("event");
    static BY_SPACER: Segment = Segment::spacer("by");
    let parent = parent.append_spacer(&SPACER);

    native_or_not(interner, parent.clone())
        .then_ignore(just(Token::Event))
        .then(ident(interner, parent.clone()))
        .map_with_span(|(is_native, name), span| EventDeclaration {
            is_native,
            name,
            span,
        })
        .then(declared_arguments(interner, parent.clone()))
        .then(
            just(Token::By)
                .ignore_then(ident(interner, parent.clone().append_spacer(&BY_SPACER)))
                .then(chain(interner, parent.clone().append_spacer(&BY_SPACER)).args())
                .or_not(),
        )
        .validate(|((a, b), c), span, emit| {
            if a.is_native.is_some() && c.is_some() {
                emit(Simple::custom(
                    span,
                    "native functions cannot have a by clause",
                ));
            }
            ((a, b), c)
        })
        .then(block(interner, parent.clone()))
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

fn r#enum(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
) -> impl Parser<Token, Enum, Error = ParserError> + '_ {
    static SPACER: Segment = Segment::spacer("enum");
    static CONSTANTS_SPACER: Segment = Segment::spacer("constants");
    let parent = parent.append_spacer(&SPACER);

    let constants = ident(interner, parent.clone().append_spacer(&CONSTANTS_SPACER))
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .padded_by(newline_repeated());

    native_or_not(interner, parent.clone())
        .then_ignore(just(Token::Enum))
        .then(ident(interner, parent.clone()))
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

fn property(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
) -> impl Parser<Token, PropertyDeclaration, Error = ParserError> + '_ {
    static SPACER: Segment = Segment::spacer("property");
    static TYPE_SPACER: Segment = Segment::spacer("return_type");
    let parent = parent.append_spacer(&SPACER);

    native_or_not(interner, parent.clone())
        .then(
            choice((just(Token::GetVar), just(Token::Val))).map_with_span(create_spanned(
                interner,
                parent.clone(),
                Segment::new("use_restriction"),
            )),
        )
        .then(ident(interner, parent.clone()))
        .then_ignore(just(Token::Ctrl(':')))
        .then(ident(interner, parent.clone().append_spacer(&TYPE_SPACER)))
        .map(|(((is_native, property_type), name), r#type)| {
            let use_restriction = match property_type {
                // Write a test which tries to put other tokens here
                Spanned {
                    value: Token::GetVar,
                    span,
                } => Spanned::new(UseRestriction::GetVar, span),
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

fn r#struct(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
) -> impl Parser<Token, Struct, Error = ParserError> + '_ {
    static SPACER: Segment = Segment::spacer("struct");
    static MEMBER_SPACER: Segment = Segment::spacer("member");
    static PROPERTY_SPACER: Segment = Segment::spacer("property");
    let parent = parent.append_spacer(&SPACER);

    let member_function = native_or_not(interner, parent.clone())
        .then_ignore(just(Token::Fn))
        .then(ident(
            interner,
            parent.clone().append_spacer(&MEMBER_SPACER),
        ))
        .then(declared_arguments(
            interner,
            parent.clone().append_spacer(&MEMBER_SPACER),
        ))
        .map(|((is_native, name), arguments)| FunctionDeclaration {
            name,
            is_native,
            arguments: arguments.inner_into(),
        });

    enum StructMember {
        Property(PropertyDeclaration),
        Function(FunctionDeclaration),
    }

    let open_or_not = just(Token::Open)
        .or_not()
        .map_with_span(create_ignored_value(
            interner,
            parent.clone(),
            Segment::new("open"),
        ));

    open_or_not
        .then(native_or_not(interner, parent.clone()))
        .then_ignore(just(Token::Struct))
        .then(ident(interner, parent.clone()))
        .map_with_span(|((is_open, is_native), name), span| StructDeclaration {
            is_open,
            is_native,
            name,
            span,
        })
        .then(
            property(interner, parent.clone().append_spacer(&PROPERTY_SPACER))
                .map(StructMember::Property)
                .or(member_function.map(StructMember::Function))
                .padded_by(just(Token::NewLine).repeated())
                .repeated()
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

fn newline_repeated() -> impl Parser<Token, (), Error = ParserError> + Clone {
    just(Token::NewLine).repeated().ignored()
}

struct IdentChainParserResult<'a> {
    ident_chain: BoxedParser<'a, Token, CallChain, ParserError>,
    args: BoxedParser<'a, Token, CallArguments, ParserError>,
}

impl<'a> IdentChainParserResult<'a> {
    fn ident_chain(self) -> BoxedParser<'a, Token, CallChain, ParserError> {
        self.ident_chain
    }

    fn args(self) -> BoxedParser<'a, Token, CallArguments, ParserError> {
        self.args
    }
}

fn chain<'a>(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
) -> IdentChainParserResult<'a> {
    static CHAIN_SPACER: Segment = Segment::spacer("chain");
    static ARG_SPACER: Segment = Segment::spacer("args");

    let mut ident_chain = Recursive::<_, CallChain, _>::declare();

    let args_parent = parent.clone().append_spacer(&ARG_SPACER);
    let args = ident(interner, args_parent.clone())
        .then_ignore(just(Token::Ctrl('=')))
        .or_not()
        .then(ident_chain.clone())
        .map_with_span(|(named, call_chain), span| match named {
            Some(name) => CallArgument::Named(name, call_chain, span),
            None => CallArgument::Pos(call_chain),
        })
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .map_with_span(create_spanned(
            interner,
            args_parent.clone(),
            Segment::new("all"),
        ));

    let literal = filter_map(|span, token| match token {
        Token::String(string) => Ok(Box::new(Call::String(string, span))),
        Token::Num(number) => Ok(Box::new(Call::Number(number, span))),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    let chain_parent = parent.clone().append_spacer(&CHAIN_SPACER);
    ident_chain.define(
        ident(interner, chain_parent.clone())
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
            .map_with_span(create_spanned(
                interner,
                chain_parent.clone(),
                Segment::new("all"),
            )),
    );

    IdentChainParserResult {
        ident_chain: ident_chain.boxed(),
        args: args.boxed(),
    }
}

fn block(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
) -> impl Parser<Token, Block, Error = ParserError> + '_ {
    static SPACER: Segment = Segment::spacer("block");
    static COND_SPACER: Segment = Segment::spacer("cond");
    static ACTION_SPACER: Segment = Segment::spacer("action");
    static PROPERTY_SPACER: Segment = Segment::spacer("property");
    let parent = parent.append_spacer(&SPACER);

    let cond = just(Token::Cond)
        .ignore_then(chain(interner, parent.clone().append_spacer(&COND_SPACER)).ident_chain())
        .map(|it| it as Condition);

    let action = chain(interner, parent.clone().append_spacer(&ACTION_SPACER))
        .ident_chain()
        .map(Action::CallChain)
        .or(
            property(interner, parent.clone().append_spacer(&PROPERTY_SPACER))
                .map(Action::Property),
        );

    cond.then_ignore(at_least_newlines())
        .repeated()
        .then(action.then_ignore(at_least_newlines()).repeated())
        .delimited_by(
            just(Token::Ctrl('{')).padded_by(newline_repeated()),
            just(Token::Ctrl('}')).padded_by(newline_repeated()),
        )
        .map_with_span(|(conditions, actions), span| Block {
            actions: actions.into(),
            conditions: conditions.into(),
            span,
        })
}

fn at_least_newlines() -> impl Parser<Token, (), Error = ParserError> {
    just(Token::NewLine).repeated().at_least(1).map(|_| ())
}

fn rule(
    interner: &dyn SpanInterner,
    parent: HierarchicalLocation,
) -> impl Parser<Token, Rule, Error = ParserError> + '_ {
    static SPACER: Segment = Segment::spacer("rule");
    static RULE_SPACER: Segment = Segment::spacer("rule");
    let parent = parent.append_spacer(&SPACER);

    let rule_name = {
        let filter_rule_map = |token: Token, span: Span, loc: HierarchicalLocation| match token {
            Token::String(string) => Ok(Spanned::new(
                string,
                HierarchicalSpan::from_location(interner, span, loc),
            )),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        };
        let map = map_with_location(parent.clone(), Segment::new("name"), filter_rule_map);
        let swapped = swap_params(map);
        filter_map(swapped)
    };

    just(Token::Rule)
        .ignore_then(rule_name)
        .then(ident(interner, parent.clone()))
        .then(chain(interner, parent.clone()).args())
        .then(block(interner, parent.clone()))
        .map_with_span(|(((rule_name, ident), arguments), block), _span| Rule {
            conditions: block.conditions,
            actions: block.actions,
            name: rule_name,
            event: ident,
            arguments,
        })
}

pub fn parser(interner: &dyn SpanInterner) -> impl Parser<Token, Ast, Error = ParserError> + '_ {
    let root = HierarchicalLocation::new();

    let rule_parser = rule(interner, root.clone()).map(Root::Rule);
    let event_parser = event(interner, root.clone()).map(Root::Event);
    let enum_parser = r#enum(interner, root.clone()).map(Root::Enum);
    let struct_parser = r#struct(interner, root.clone()).map(Root::Struct);

    choice((rule_parser, event_parser, enum_parser, struct_parser))
        .padded_by(newline_repeated())
        .repeated()
        .then_ignore(end())
        .map(Ast)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::compiler::cst::{Call, DeclaredArgument, Rule};
    use crate::compiler::database::test::TestDatabase;
    use crate::compiler::language::lexer::{lexer, Token};
    use crate::compiler::language::parser::ParserError;
    use crate::compiler::{SpanInterner, SpanLocation};
    use crate::compiler::{SpanSourceId, Spanned};
    use crate::{assert_iterator, Span};
    use anyhow::anyhow;
    use chumsky::prelude::end;
    use chumsky::{Parser, Stream};
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

    fn parse_code<T>(
        span_source_id: SpanSourceId,
        code: &str,
        parser: &impl Parser<Token, T, Error = ParserError>,
    ) -> anyhow::Result<T> {
        let tokens: Vec<_> = lex_code(span_source_id, code)?;
        let eoi = Span::new(
            span_source_id,
            SpanLocation::from(tokens.len()..tokens.len() + 1),
        );
        let stream = Stream::from_iter(eoi, tokens.into_iter());

        parser
            .then_ignore(end())
            .parse(stream)
            .map_err(|_| anyhow!("Cannot parse '{code}'"))
    }

    fn lex_code(span_source_id: SpanSourceId, code: &str) -> anyhow::Result<Vec<(Token, Span)>> {
        lexer(span_source_id).parse(code).map_err(|errors| {
            let error_message = errors
                .into_iter()
                .map(|it| it.to_string())
                .collect::<Vec<String>>()
                .join("\n");
            anyhow!(error_message)
        })
    }

    fn test_parser_result<Ex, Ac, F>(
        key: &ResKey,
        should_panic: bool,
        parser: impl Parser<Token, Ac, Error = ParserError>,
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

    const TEST_DIR: &'static str = "resources/test";

    fn assert_call_chain(
        expected: impl IntoIterator<Item = IdentTestData>,
        actual: impl IntoIterator<Item = Box<Call>>,
    ) {
        actual.into_iter()
            .zip(expected.into_iter())
            .for_each(|(call, test_data)| {
                match *call {
                    Call::IdentArguments { name: actual_ident, args: actual_args, .. } => {
                        match test_data {
                            IdentTestData::IdentWithArgs { ident: expected_ident, args: expected_args } => {
                                assert_eq!(expected_ident, actual_ident.value, "Check if idents are equal");
                                expected_args.into_iter()
                                    .zip(actual_args.into_iter())
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
                    .zip(actual.arguments.into_iter())
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
                    .zip(actual.into_iter())
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
