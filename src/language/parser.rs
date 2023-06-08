extern crate core;

use crate::language::ast::*;
use crate::language::lexer::Token;
use crate::language::{Ident, Spanned};
use chumsky::prelude::*;
use smallvec::SmallVec;

fn ident() -> impl Parser<Token, Ident, Error=Simple<Token>> {
    filter_map(|span, token| match token {
        Token::Ident(ident) => Ok(Ident { value: ident, span }),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    })
}

fn declared_arguments() -> impl Parser<Token, Spanned<Vec<DeclaredArgument>>, Error=Simple<Token>>
{
    ident()
        .then_ignore(just(Token::Ctrl(':')))
        .then(
            ident()
                .separated_by(just(Token::Ctrl('|')))
                .map_with_span(|types, span| Types {
                    values: types.into(),
                    span,
                }),
        )
        .then(
            just(Token::Ctrl('='))
                .ignore_then(chain().ident_chain())
                .or_not(),
        )
        .map_with_span(|((name, types), default_value), span| DeclaredArgument {
            name,
            types,
            default_value,
            span,
        })
        .separated_by(just(Token::Ctrl(',')))
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .map_with_span(Spanned::new)
}

fn workshop_or_not() -> impl Parser<Token, SpannedBool, Error=Simple<Token>> {
    just(Token::Workshop)
        .or_not()
        .map_with_span(Spanned::ignore_value)
}

fn event() -> impl Parser<Token, Event, Error=Simple<Token>> {
    just(Token::Workshop)
        .or_not()
        .map_with_span(Spanned::ignore_value)
        .then_ignore(just(Token::Event))
        .then(ident())
        .map_with_span(|(is_workshop, name), span| EventDeclaration {
            is_workshop,
            name,
            span,
        })
        .then(declared_arguments())
        .then(
            just(Token::By)
                .ignore_then(ident())
                .then(chain().args())
                .or_not(),
        )
        .validate(|((a, b), c), span, emit| {
            if a.is_workshop.is_some() && c.is_some() {
                emit(Simple::custom(
                    span,
                    "Workshop functions cannot have a by clause",
                ));
            }
            ((a, b), c)
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

fn r#enum() -> impl Parser<Token, Enum, Error=Simple<Token>> {
    let constants = ident()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .padded_by(newline_repeated());

    workshop_or_not()
        .then_ignore(just(Token::Enum))
        .then(ident())
        .map_with_span(|(is_workshop, name), span| EnumDeclaration {
            is_workshop,
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

fn property() -> impl Parser<Token, PropertyDeclaration, Error=Simple<Token>> {
    workshop_or_not()
        .then(choice((just(Token::GetVal), just(Token::Val))).map_with_span(Spanned::new))
        .then(ident())
        .then_ignore(just(Token::Ctrl(':')))
        .then(ident())
        .map(|(((is_workshop, property_type), name), r#type)| {
            let use_restriction = match property_type {
                // Write a test which tries to put other tokens here
                Spanned {
                    value: Token::GetVal,
                    span,
                } => Spanned::new(UseRestriction::GetVal, span),
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
                is_workshop,
                use_restriction,
                r#type,
            }
        })
}

fn r#struct() -> impl Parser<Token, Struct, Error=Simple<Token>> {
    let member_function = workshop_or_not()
        .then_ignore(just(Token::Fn))
        .then(ident())
        .then(declared_arguments())
        .map(|((is_workshop, name), arguments)| FunctionDeclaration {
            name,
            is_workshop,
            arguments: arguments.inner_into(),
        });

    enum StructMember {
        Property(PropertyDeclaration),
        Function(FunctionDeclaration),
    }

    let open_keyword = just(Token::Open)
        .or_not()
        .map_with_span(Spanned::ignore_value);

    open_keyword
        .then(workshop_or_not())
        .then_ignore(just(Token::Struct))
        .then(ident())
        .map_with_span(|((is_open, is_workshop), name), span| StructDeclaration {
            is_open,
            is_workshop,
            name,
            span,
        })
        .then(
            property()
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

fn newline_repeated() -> impl Parser<Token, (), Error=Simple<Token>> + Clone {
    just(Token::NewLine).repeated().ignored()
}

struct IdentChainParserResult<'a> {
    ident_chain: BoxedParser<'a, Token, CallChain, Simple<Token>>,
    args: BoxedParser<'a, Token, CallArguments, Simple<Token>>,
}

impl<'a> IdentChainParserResult<'a> {
    fn ident_chain(self) -> BoxedParser<'a, Token, CallChain, Simple<Token>> {
        self.ident_chain
    }

    fn args(self) -> BoxedParser<'a, Token, CallArguments, Simple<Token>> {
        self.args
    }
}

fn chain<'a>() -> IdentChainParserResult<'a> {
    let mut ident_chain = Recursive::<_, CallChain, _>::declare();
    let args = ident_chain
        .clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .map_with_span(CallArguments::new)
        .labelled("args");

    let literal = filter_map(|span, token| match token {
        Token::String(string) => Ok(Box::new(Call::String(string, span))),
        Token::Num(number) => Ok(Box::new(Call::Number(number, span))),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
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
            .map_with_span(CallChain::new),
    );

    IdentChainParserResult {
        ident_chain: ident_chain.boxed(),
        args: args.boxed(),
    }
}

fn block() -> impl Parser<Token, Block, Error=Simple<Token>> {
    let cond = just(Token::Cond)
        .ignore_then(chain().ident_chain())
        .map(|it| it as Condition);

    let action = chain()
        .ident_chain()
        .map(Action::CallChain)
        .or(property().map(Action::Property));

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

fn at_least_newlines() -> impl Parser<Token, (), Error=Simple<Token>> {
    just(Token::NewLine).repeated().at_least(1).map(|_| ())
}

fn rule() -> impl Parser<Token, Rule, Error=Simple<Token>> {
    let rule_name = filter_map(|span, token| match token {
        Token::String(string) => Ok(Spanned::new(string, span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
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

pub fn parser() -> impl Parser<Token, Ast, Error=Simple<Token>> {
    let rule_parser = rule().map(Root::Rule);
    let event_parser = event().map(Root::Event);
    let enum_parser = r#enum().map(Root::Enum);
    let struct_parser = r#struct().map(Root::Struct);

    choice((rule_parser, event_parser, enum_parser, struct_parser))
        .separated_by(just(Token::NewLine).repeated())
        .then_ignore(end())
        .map(Ast)
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::fs;
    use std::path::{Path, PathBuf};
    use chumsky::error::Simple;
    use serde::{Deserialize};
    use crate::language::lexer::{lexer, Token};
    use crate::language::parser::{chain, r#enum};
    use crate::Span;
    use chumsky::{Parser, Stream};
    use crate::language::ast::CallChain;

    #[derive(Debug, Deserialize)]
    struct BaseTestData<T> {
        code: String,

        expected: Option<T>,
    }

    #[derive(Debug, Deserialize)]
    struct EnumTestData {
        #[serde(default)]
        is_workshop: bool,
        #[serde(default)]
        constants: Vec<String>,
    }


    #[derive(Debug, Deserialize)]
    #[serde(untagged)]
    enum IdentTestData {
        Ident(String),
        IdentWithArgs{ ident: String, args: Vec<IdentTestData> }
    }

    #[derive(Debug, Deserialize)]
    struct CallChainTestData {
        idents: Vec<IdentTestData>,
    }

    fn read_test_data<T: for<'a> serde::Deserialize<'a>>(path: impl AsRef<Path>, directory: &str) -> Vec<BaseTestData<T>> {
        let mut path_buf: PathBuf = PathBuf::from(&format!("resources/test/{directory}"));
        path_buf.push(path);
        path_buf.set_extension("yaml");

        let input = fs::read_to_string(&path_buf).expect(&format!("Cannot read file {path_buf:?}"));
        input.split("---")
            .filter(|it| !it.is_empty())
            .map(|it| serde_yaml::from_str(it).expect(&format!("Cannot read {it}")))
            .collect::<Vec<_>>()
    }

    fn parse_code<T>(code: &str, parser: &impl Parser<Token, T, Error=Simple<Token>>) -> T {
        let tokens: Vec<_> = get_tokens(code);
        let stream = Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter());

        parser.parse(stream)
            .map_err(|_| format!("Cannot parse '{code}'"))
            .unwrap()
    }

    fn get_tokens(code: &str) -> Vec<(Token, Span)> {
        lexer().parse(code).expect(&format!("Cannot lex {code}"))
    }

    fn test_parser_result<Ex, Ac, F>(
        path: impl AsRef<Path>,
        directory: &str,
        parser: impl Parser<Token, Ac, Error=Simple<Token>>,
        func: F,
    )
        where Ex: for<'a> serde::Deserialize<'a>,
              F: Fn(Ex, Ac),
    {
        let data = read_test_data(path, directory);

        for test_data in data {
            let code = parse_code(&test_data.code, &parser);

            if let Some(expected) = test_data.expected {
                func(expected, code)
            }
        }
    }

    fn nothing<A, B>(_: A, _: B) {}

    #[test]
    fn test_valid_enum() {
        test_parser_result("valid", "enum", r#enum(), |expected: EnumTestData, actual| {
            assert_eq!(expected.is_workshop, actual.declaration.is_workshop.is_some());
            assert_eq!(expected.constants, actual.definition.constants.into_iter().map(|it| it.value).collect::<Vec<_>>());
        });
    }

    #[test]
    #[should_panic(expected = "Cannot parse")]
    fn test_invalid_enum() {
        test_parser_result::<EnumTestData, _, _>("invalid", "enum", r#enum(), nothing);
    }

    #[test]
    fn test_valid_ident_chain() {
        test_parser_result("valid", "ident_chain", chain().ident_chain(), |expected: CallChainTestData, actual: CallChain| {
            assert_eq!(expected.idents, actual.value)
        })
    }
}