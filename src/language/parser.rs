extern crate core;

use chumsky::prelude::*;
use crate::language::lexer::Token;
use crate::language::ast::*;
use crate::language::{Ident, Spanned};

type IdentParser = impl Parser<Token, Ident, Error=Simple<Token>> + Clone;
type IdentChainParser = impl Parser<Token, CallChain, Error=Simple<Token>> + Clone;
type ArgsParser = impl Parser<Token, CallArguments, Error=Simple<Token>> + Clone;
type EventParser = impl Parser<Token, Event, Error=Simple<Token>> + Clone;
type EnumParser = impl Parser<Token, Enum, Error=Simple<Token>> + Clone;
type BlockParser = impl Parser<Token, Block, Error=Simple<Token>> + Clone;
type RuleParser = impl Parser<Token, Rule, Error=Simple<Token>> + Clone;
type StructParser = impl Parser<Token, Struct, Error=Simple<Token>> + Clone;
type DeclaredArgumentParser = impl Parser<Token, Spanned<Vec<DeclaredArgument>>, Error=Simple<Token>> + Clone;
type PropertyParser = impl Parser<Token, PropertyDeclaration, Error=Simple<Token>> + Clone;

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
        .then(ident.clone().separated_by(just(Token::Ctrl('|'))).map_with_span(|types, span| Types { values: types, span }))
        .then(just(Token::Ctrl('=')).ignore_then(ident_chain).or_not())
        .map_with_span(|((name, types), default_value), span|
            DeclaredArgument { name, types, default_value, span }
        )
        .separated_by(just(Token::Ctrl(',')))
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .map_with_span(|it, span| Spanned::new(it, span))
}

fn workshop_keyword() -> impl Parser<Token, SpannedBool, Error=Simple<Token>> + Clone {
    just(Token::Workshop)
        .or_not()
        .map_with_span(|it, span| Spanned::ignore_value(it, span))
}

fn event_parser(
    block: BlockParser,
    declare_args: DeclaredArgumentParser,
    ident: IdentParser,
    args: ArgsParser
) -> EventParser {
    just(Token::Workshop).or_not().map_with_span(|o, span| Spanned::ignore_value(o, span))
        .then_ignore(just(Token::Event))
        .then(ident.clone())
        .map_with_span(|(is_workshop, name), span| EventDeclaration { is_workshop, name, span })
        .then(declare_args)
        .then(just(Token::By).ignore_then(ident).then(args).or_not())
        .validate(|((a, b), c), span, emit| {
            if a.is_workshop.is_some() && c.is_some() {
                emit(Simple::custom(span, "Workshop functions cannot have a by clause"));
            }
            ((a, b), c)
        })
        .then(block.clone())
        .map_with_span(|(((declaration, arguments), by), block), span| {
            Event {
                declaration,
                definition: EventDefinition {
                    actions: block.actions,
                    conditions: block.conditions,
                    by,
                    arguments
                },
                span
            }
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
        .map_with_span(|(is_workshop, name), span| {
            EnumDeclaration { is_workshop, name, span }
        })
        .then(constants.delimited_by(
            just(Token::Ctrl('{')),
            just(Token::Ctrl('}')),
        ))
        .map_with_span(|(declaration, constants), span| {
            Enum {
                declaration,
                definition: EnumDefinition { constants },
                span
            }
        })
}

fn property_parser(ident: IdentParser) -> PropertyParser {
    workshop_keyword()
        .then(choice((just(Token::GetVal), just(Token::Val))).map_with_span(|it, span| Spanned::new(it, span)))
        .then(ident.clone())
        .then_ignore(just(Token::Ctrl(':')))
        .then(ident.clone())
        .map(|(((is_workshop, property_type), name), r#type)| {
            let use_restriction = match property_type {
                // Write a test which tries to put other tokens here
                Spanned { value: Token::GetVal, span} => Spanned::new(UseRestriction::GetVal, span),
                Spanned { value: Token::Val, span } => Spanned::new(UseRestriction::Val, span),
                _ => panic!("Compiler Error: Unexpected token as property type {:?}", property_type)
            };
            PropertyDeclaration { name, is_workshop, use_restriction, r#type }
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
            FunctionDeclaration { name, is_workshop, arguments }
        });

    enum StructMember {
        Property(PropertyDeclaration),
        Function(FunctionDeclaration),
    }

    let open_keyword = just(Token::Open)
        .or_not()
        .map_with_span(|it, span| Spanned::ignore_value(it, span));

    let struct_parser = open_keyword
        .then(workshop_keyword())
        .then_ignore(just(Token::Struct))
        .then(ident.clone())
        .map_with_span(|((is_open, is_workshop), name), span| {
            StructDeclaration { is_open, is_workshop, name, span }
        })
        .then(
            property.map(StructMember::Property)
                .or(member_function.map(StructMember::Function))
                .padded_by(just(Token::NewLine).repeated())
                .repeated()
                .delimited_by(
                    just(Token::Ctrl('{')), just(Token::Ctrl('}')),
                )
        )
        .map_with_span(|(declaration, members), span| {
            let mut functions = Vec::new();
            let mut properties = Vec::new();
            for member in members {
                match member {
                    StructMember::Function(function) => functions.push(function),
                    StructMember::Property(property) => properties.push(property)
                };
            }

            Struct {
                declaration,
                definition: StructDefinition { properties, functions },
                span,
            }
        });

    struct_parser
}

fn newlines() -> impl Parser<Token, (), Error=Simple<Token>> + Clone {
    just(Token::NewLine)
        .repeated()
        .ignored()
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
        .map_with_span(|it, span| CallArguments::new(it, span))
        .labelled("function args");

    let literal = filter_map(|span, token| match token {
        Token::String(string) => Ok(Box::new(Call::String(string, span))),
        Token::Num(number) => Ok(Box::new(Call::Number(number, span))),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    ident_chain.define(
        ident
            .then(args.clone().or_not())
            .map_with_span(|(ident, arguments), span| {
                let call = match arguments {
                    Some(arguments) => Call::IdentArguments { name: ident, args: arguments, span },
                    None => Call::Ident(ident)
                };
                Box::new(call)
            })
            .or(literal)
            .separated_by(just(Token::Ctrl('.')))
            .at_least(1)
            .map_with_span(|it, span| CallChain::new(it, span)));

    (ident_chain, args)
}

fn block_parser(
    ident_chain: IdentChainParser,
    property: PropertyParser,
) -> BlockParser {
    let cond = just(Token::Cond)
        .ignore_then(ident_chain.clone())
        .map(|it| it as Condition);

    let action = ident_chain.map(Action::CallChain)
            .or(property.map(Action::Property));

    cond.then_ignore(at_least_newlines()).repeated()
        .then(action.then_ignore(at_least_newlines()).repeated())
        .delimited_by(
            just(Token::Ctrl('{')).padded_by(newlines()),
            just(Token::Ctrl('}')).padded_by(newlines())
        )
        .map_with_span(|(conditions, actions), span| Block { actions, conditions, span })
}

fn at_least_newlines() -> impl Parser<Token, (), Error=Simple<Token>> + Clone {
    just(Token::NewLine)
        .repeated()
        .at_least(1)
        .map(|_| ())
}

pub fn rule_parser(
    ident: IdentParser,
    block: BlockParser,
    args: ArgsParser,
) -> RuleParser {
    let rule_name = filter_map(|span, token| match token {
        Token::String(string) => Ok(Spanned::new(string.clone(), span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    });

    just(Token::Rule)
        .ignore_then(rule_name)
        .then(ident)
        .then(args.clone())
        .then(block)
        .map_with_span(|(((rule_name, ident), arguments), block), _span| Rule {
            conditions: block.conditions,
            actions: block.actions,
            name: rule_name,
            event: ident,
            arguments,
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
        .map(|root_vec| Ast(root_vec))
}