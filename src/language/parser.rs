extern crate core;

use chumsky::prelude::*;
use crate::language::lexer::Token;
use crate::language::ast::*;
use crate::language::{Ident};

type IdentParser = impl Parser<Token, Ident, Error=Simple<Token>> + Clone;
type IdentChainParser = impl Parser<Token, CallChain, Error=Simple<Token>> + Clone;
type ArgsParser = impl Parser<Token, Vec<CallChain>, Error=Simple<Token>> + Clone;
type EventParser = impl Parser<Token, Event, Error=Simple<Token>> + Clone;
type EnumParser = impl Parser<Token, Enum, Error=Simple<Token>> + Clone;
type BlockParser = impl Parser<Token, Block, Error=Simple<Token>> + Clone;
type RuleParser = impl Parser<Token, Rule, Error=Simple<Token>> + Clone;
type StructParser = impl Parser<Token, Struct, Error=Simple<Token>> + Clone;
type DeclaredArgumentParser = impl Parser<Token, Vec<DeclaredArgument>, Error=Simple<Token>> + Clone;
type PropertyParser = impl Parser<Token, Property, Error=Simple<Token>> + Clone;

pub fn ident_parser() -> IdentParser {
    filter_map(|span, token| match token {
        Token::Ident(ident) => Ok(Ident { value: ident.clone(), span }),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    })
}

fn declare_arguments_parser(
    ident: IdentParser,
    ident_chain: IdentChainParser,
) -> DeclaredArgumentParser {
    ident.clone()
        .then_ignore(just(Token::Ctrl(':')))
        .then(ident.clone().separated_by(just(Token::Ctrl('|'))))
        .then(just(Token::Ctrl('=')).ignore_then(ident_chain).or_not())
        .map_with_span(|((name, types), default_value), span|
            DeclaredArgument { name, types, default_value, span }
        )
        .separated_by(just(Token::Ctrl(',')))
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
}

fn workshop_keyword() -> impl Parser<Token, Spanned<bool>, Error=Simple<Token>> + Clone {
    just(Token::Workshop)
        .or_not()
        .map_with_span(|it, span| Spanned(it.is_some(), span))
}

fn event_parser(
    block: BlockParser,
    declare_args: DeclaredArgumentParser,
    ident: IdentParser,
    args: ArgsParser
) -> EventParser {
    just(Token::Workshop)
        .or_not()
        .map(|o| o.is_some())
        .then_ignore(just(Token::Event))
        .then(ident.clone())
        .then(declare_args)
        .then(just(Token::By).ignore_then(ident).then(args).or_not())
        .then(block.clone())
        .validate(|((((is_workshop, name), decl_args), by), block), span, emit| {
            if is_workshop && by.is_some() {
                emit(Simple::custom(span, "Workshop functions cannot have a by clause"));
            }
            ((((is_workshop, name), decl_args), by), block)
        })
        .map_with_span(|((((_is_workshop, name), decl_args), by), block), span| Event {
            name,
            by,
            args: decl_args,
            conditions: block.conditions,
            actions: block.actions,
            span,
        })
}

fn enum_parser(
    ident: IdentParser
) -> EnumParser {
    let constants = ident.clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .padded_by(newlines());

    workshop_keyword()
        .then_ignore(just(Token::Enum))
        .then(ident.clone())
        .then(constants.delimited_by(
            just(Token::Ctrl('{')),
            just(Token::Ctrl('}')),
        ))
        .map_with_span(|((is_workshop, name), constants), span| Enum { name, is_workshop, constants, span })
}

fn property_parser(ident: IdentParser) -> PropertyParser {
    workshop_keyword()
        .then(choice((just(Token::GetVal), just(Token::Val))))
        .then(ident.clone())
        .then_ignore(just(Token::Ctrl(':')))
        .then(ident.clone())
        .map(|(((is_workshop, property_type), name), r#type)| {
            let desc = match property_type {
                // Write a test which tries to put other tokens here
                Token::GetVal => PropertyDesc::GetVal,
                Token::Val => PropertyDesc::Val,
                _ => panic!("Compiler Error: Unexpected token as property type {}", property_type)
            };
            Property { name, is_workshop, desc, r#type }
        })
}

fn struct_parser(
    ident: IdentParser,
    declared_args: DeclaredArgumentParser,
    property: PropertyParser,
) -> StructParser {
    let member_function = workshop_keyword()
        .then_ignore(just(Token::Fn))
        .then(ident.clone())
        .then(declared_args.clone())
        .map(|((is_workshop, name), arguments)| {
            Function { name, is_workshop, arguments }
        });

    enum StructMember {
        Property(Property),
        Function(Function),
    }

    let struct_parser = just(Token::Open)
        .or_not().map_with_span(|it, span| Spanned(it.is_some(), span))
        .then(just(Token::Workshop).or_not().map_with_span(|it, span| Spanned(it.is_some(), span)))
        .then_ignore(just(Token::Struct))
        .then(ident.clone())
        .then(
            property.map(StructMember::Property)
                .or(member_function.map(StructMember::Function))
                .separated_by(just(Token::NewLine).repeated().at_least(1))
                .padded_by(just(Token::NewLine).repeated())
                .delimited_by(
                    just(Token::Ctrl('{')), just(Token::Ctrl('}')),
                )
        )
        .map_with_span(|(((is_open, is_workshop), name), members), span| {
            let mut functions = Vec::new();
            let mut properties = Vec::new();
            for member in members {
                match member {
                    StructMember::Function(function) => functions.push(function),
                    StructMember::Property(property) => properties.push(property)
                };
            }

            Struct { name, is_open, is_workshop, span, properties, functions }
        });

    struct_parser
}

fn newlines() -> impl Parser<Token, (), Error=Simple<Token>> + Clone {
    just(Token::NewLine)
        .repeated()
        .map(|_| ())
}

fn ident_chain_parser(
    ident: IdentParser
) -> (
    IdentChainParser,
    ArgsParser
) {
    let mut ident_chain = Recursive::<_, CallChain, _>::declare();
    let args = ident_chain
        .clone()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .map(|it| it as CallArguments)
        .labelled("function args");

    let literal = filter_map(|span, token| match token {
        Token::String(string) => Ok(Box::new(Call::String(string))),
        Token::Num(number) => Ok(Box::new(Call::Number(number))),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    ident_chain.define(
        ident
            .then(args.clone().or_not())
            .map_with_span(|(ident, arguments), span| {
                let call = match arguments {
                    Some(arguments) => Call::ArgumentsIdent { name: ident, args: arguments, span },
                    None => Call::Ident(ident)
                };
                Box::new(call)
            })
            .or(literal)
            .separated_by(just(Token::Ctrl('.')))
            .at_least(1));

    (ident_chain, args)
}

fn block_parser(
    ident_chain: IdentChainParser,
    property: PropertyParser,
) -> BlockParser {
    let cond = just(Token::Cond)
        .ignore_then(ident_chain.clone())
        .then_ignore(just(Token::NewLine))
        .map(|it| it as Condition);

    let action = ident_chain.map(Action::CallChain)
        .or(property.map(Action::Property));

    just(Token::Ctrl('{'))
        .ignore_then(cond.repeated().padded_by(just(Token::NewLine).repeated()))
        .then(action
            .then_ignore(just(Token::NewLine))
            .repeated().padded_by(just(Token::NewLine).repeated())
        )
        .then_ignore(just(Token::Ctrl('}')))
        .map_with_span(|(conditions, actions), span| Block { actions, conditions, span })
}

pub fn rule_parser(
    ident: IdentParser,
    block: BlockParser,
    args: ArgsParser,
) -> RuleParser {
    let rule_name = filter_map(|span, token| match token {
        Token::String(string) => Ok(Spanned(string.clone(), span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    just(Token::Rule)
        .ignore_then(rule_name)
        .then(ident)
        .then(args.clone())
        .then(block)
        .map_with_span(|(((rule_name, ident), args), block), span| Rule {
            conditions: block.conditions,
            actions: block.actions,
            name: rule_name,
            event: ident,
            args,
            span,
        })
}

pub fn parser() -> impl Parser<Token, Ast, Error=Simple<Token>> {
    let ident = ident_parser();
    let (ident_chain, args) = ident_chain_parser(ident.clone());
    let property = property_parser(ident.clone());
    let block = block_parser(ident_chain.clone(), property.clone());
    let declared_argument = declare_arguments_parser(ident.clone(), ident_chain.clone());

    let rule_parser = rule_parser(ident.clone(), block.clone(), args.clone())
        .map(Root::Rule);
    let event_parser = event_parser(block.clone(), declared_argument.clone(), ident.clone(), args)
        .map(Root::Event);
    let enum_parser = enum_parser(ident.clone())
        .map(Root::Enum);
    let struct_parser = struct_parser(ident.clone(), declared_argument.clone(), property.clone())
        .map(Root::Struct);

    choice((rule_parser, event_parser, enum_parser, struct_parser))
        .separated_by(just(Token::NewLine).repeated())
        .then_ignore(end())
        .map(|ast| ast)
}

#[cfg(test)]
mod tests {
    use chumsky::{Parser, Stream};
    use std::fs::read_to_string;
    use once_cell::sync::Lazy;
    use crate::language::ast::Spanned;
    use crate::language::Ident;
    use crate::language::lexer::lexer;
    use crate::language::parser::{Ast, Call, DeclaredArgument, Enum, Event, parser, Root, Rule};
    use crate::test_assert::assert_vec;

    static RULE_HEADER: Lazy<String> = Lazy::new(|| read_to_string("resources/tests/snippets/rule_header.colo").unwrap());
    static EVENT_DECL_HEADER: Lazy<String> = Lazy::new(|| read_to_string("resources/tests/snippets/rule_decl.colo").unwrap());
    static ENUM: Lazy<String> = Lazy::new(|| read_to_string("resources/tests/snippets/enum.colo").unwrap());

    fn read(file: &Lazy<String>) -> Ast {
        let tokens = lexer().parse(file.as_str()).unwrap();
        let stream = Stream::from_iter(tokens.len()..tokens.len() + 1, tokens.into_iter());
        parser().parse(stream).unwrap()
    }

    #[test]
    fn test_enum() {
        let actual_enums: Vec<_> = read(&ENUM).into_iter()
            .filter_map(|decl| match decl {
                Root::Enum(my_enum) => Some(my_enum),
                _ => None
            }).collect();

        let expected: Vec<Enum> = vec![
            Enum {
                is_workshop: Spanned(true, 0..8),
                name: Ident::new("Hero".to_string(), 14..18),
                constants: vec![
                    Ident::new("Reaper".to_string(), 26..32),
                    Ident::new("Tracer".to_string(), 39..45),
                    Ident::new("Mercy".to_string(), 52..57),
                ],
                span: 0..60,
            },
            Enum {
                is_workshop: Spanned(false, 64..68),
                name: Ident::new("MyEnum".to_string(), 69..75),
                constants: vec![
                    Ident::new("First".to_string(), 83..88),
                    Ident::new("Second".to_string(), 90..96),
                ],
                span: 64..99,
            },
        ];

        actual_enums.into_iter()
            .zip(expected)
            .for_each(|(actual, expected)| {
                assert_eq!(actual.name, expected.name);
                assert_eq!(actual.is_workshop, expected.is_workshop);
                assert_eq!(actual.span, expected.span);
                assert_vec(&actual.constants, &expected.constants);
            })
    }

    #[test]
    fn test_event_decl() {
        let actual_events: Vec<_> = read(&EVENT_DECL_HEADER).into_iter()
            .filter_map(|o| match o {
                Root::Event(event) => Some(event),
                _ => None,
            })
            .collect();

        let expected: Vec<Event> = vec![
            Event {
                name: Ident::new("OngoingEachPlayer".to_string(), 15..32),
                conditions: Vec::new(),
                by: None,
                span: 0..71,
                args: vec![
                    DeclaredArgument {
                        name: Ident::new("team".to_string(), 33..37),
                        types: vec![Ident::new("Team".to_string(), 39..43)],
                        default_value: None,
                        span: 33..43,
                    },
                    DeclaredArgument {
                        name: Ident::new(
                            "heroSlot".to_string(),
                            45..53,
                        ),
                        types: vec![Ident::new(
                            "Hero".to_string(),
                            55..59,
                        ), Ident::new(
                            "Slot".to_string(),
                            62..66,
                        )],
                        default_value: None,
                        span: 45..66,
                    },
                ],
            }
        ];

        assert_eq!(actual_events.len(), expected.len(),
                   "Test if actual rules length is equal to expected length");

        actual_events.into_iter()
            .zip(expected)
            .for_each(|(actual, expected)| {
                assert_eq!(actual.name, expected.name);
                assert_eq!(actual.by, expected.by);
                assert_eq!(actual.span, expected.span);
                assert_vec(&actual.args, &expected.args);
            })
    }

    #[test]
    fn test_rule_header() {
        let actual_rules = read(&RULE_HEADER);

        let expected: Vec<Rule> = vec![
            Rule {
                name: Spanned("Heal on Kill".to_string(), 5..19),
                event: Ident::new("OngoingPlayer".to_string(), 20..33),
                args: Vec::new(),
                conditions: Vec::new(),
                actions: Vec::new(),
                span: 0..39,
            },
            Rule {
                name: Spanned("Test".to_string(), 48..54),
                event: Ident::new("MyEvent".to_string(), 55..62),
                args: vec![
                    Call::new_var(Ident::new("Hello".to_string(), 63..68)),
                    Call::new_var(Ident::new("World".to_string(), 70..75)),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
                span: 43..80,
            },
            Rule {
                name: Spanned("Heal on Kill".to_string(), 89..103),
                event: Ident::new("PlayerDealtFinalBlow".to_string(), 104..124),
                args: vec![
                    Call::new_var(Ident::new("Team1".to_string(), 125..130)),
                    Call::new_var(Ident::new("Slot1".to_string(), 132..137)),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
                span: 84..142,
            },
            Rule {
                name: Spanned("Heal on Kill".to_string(), 151..165),
                event: Ident::new("PlayerDealtFinalBlow".to_string(), 166..186),
                args: vec![
                    Call::new_fn(Ident::new("Team1".to_string(), 187..192), 187..194),
                    Call::new_fn(Ident::new("Slot1".to_string(), 196..201), 196..203),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
                span: 146..208,
            },
            Rule {
                name: Spanned("Do something".to_string(), 217..231),
                span: 212..271,
                event: Ident::new("HelloWorld".to_string(), 232..242),
                args: vec![
                    Call::new_fn_args(
                        Ident::new("test".to_string(), 243..247),
                        243..254,
                        vec![Call::new_var(Ident::new("Team1".to_string(), 248..253))],
                    ),
                    Call::new_fn_args(
                        Ident::new("foo".to_string(), 256..259),
                        256..266,
                        vec![Call::new_var(Ident::new("Slot1".to_string(), 260..265))],
                    ),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
            },
            Rule {
                name: Spanned("Complex".to_string(), 280..289),
                span: 275..345,
                event: Ident::new("HelloWorld".to_string(), 290..300),
                args: vec![
                    Call::new_var_next(
                        Ident::new("foo".to_string(), 301..304),
                        Call::new_fn_args_next(
                            Ident::new("bar".to_string(), 305..308),
                            301..320,
                            vec![Call::new_var(Ident::new("hello".to_string(), 309..314))],
                            Call::new_var(Ident::new("nice".to_string(), 316..320)),
                        ),
                    ),
                    Call::new_fn_args_next(
                        Ident::new("fn".to_string(), 322..324),
                        322..340,
                        vec![
                            Call::new_var(Ident::new("e".to_string(), 325..326)),
                            Call::new_var(Ident::new("o".to_string(), 328..329)),
                        ],
                        Call::new_fn_args(
                            Ident::new("foo".to_string(), 331..334),
                            322..340,
                            vec![
                                Call::new_var(Ident::new("x".to_string(), 335..336)),
                                Call::new_var(Ident::new("p".to_string(), 338..339)),
                            ],
                        ),
                    ),
                ],
                conditions: Vec::new(),
                actions: Vec::new(),
            },
        ];

        assert_eq!(actual_rules.len(), expected.len(),
                   "Test if actual rules length is equal to expected length");

        actual_rules.into_iter()
            .zip(expected)
            .for_each(|(actual, expected)| {
                match actual {
                    Root::Rule(actual) => {
                        assert_eq!(actual.name, expected.name,
                                   "Test if {:?} is equal to {:?}", actual.name, expected.name);
                        assert_eq!(actual.event, expected.event,
                                   "Test if {:?} is equal to {:?}", actual.event, expected.event);
                        assert_eq!(actual.span, expected.span);
                        assert_vec(&actual.args, &expected.args);
                        assert_vec(&actual.conditions, &expected.conditions);
                        assert_vec(&actual.actions, &expected.actions);
                    }
                    _ => assert!(false, "{:?} and {:?} do not have the same type", actual, expected)
                }
            })
    }
}