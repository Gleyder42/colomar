use super::super::cst::*;
use super::super::language::lexer::Token;
use super::super::{AssignMod, Ident, UseRestriction};
use chumsky::input::{SpannedInput, Stream};

use super::super::span::{Span, Spanned, SpannedBool};
use crate::parser_alias;
use chumsky::prelude::*;
use smallvec::SmallVec;

pub type ParserInput = SpannedInput<Token, Span, Stream<std::vec::IntoIter<(Token, Span)>>>;
pub type ParserExtra<'a> = extra::Err<Rich<'a, Token, Span>>;

parser_alias!(PParser, ParserInput, ParserExtra<'a>);

pub fn ident<'src>() -> impl PParser<'src, Ident> + Clone {
    select! {
        Token::Ident(ident) = span => Ident { value: ident, span }
    }
}

pub fn decl_generics<'src>() -> impl PParser<'src, DeclGenerics> {
    ident()
        .separated_by(just(Token::Ctrl(',')))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::Ctrl('<')), just(Token::Ctrl('>')))
        .or_not()
        .map(|generics| {
            generics
                .map(DeclGenerics::from_vec)
                .unwrap_or_else(|| DeclGenerics::new())
        })
}

pub fn bound_generics<'src>() -> impl PParser<'src, Vec<BoundGeneric>> {
    recursive(|generics| {
        ident()
            .then(
                generics
                    .or_not()
                    .map(|generics| generics.unwrap_or_else(Vec::new)),
            )
            .map(|(ident, bound_generics)| BoundGeneric {
                ident,
                bound_generics,
            })
            .separated_by(just(Token::Ctrl(',')))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::Ctrl('<')), just(Token::Ctrl('>')))
    })
}

pub fn r#type<'src>() -> impl PParser<'src, Type> {
    let with_generics = ident()
        .then(bound_generics())
        .map(|(ident, generics)| Type { ident, generics });
    let without_generics = ident().map(|ident| Type {
        ident,
        generics: Vec::new(),
    });
    with_generics.or(without_generics)
}

pub fn types<'src>() -> impl PParser<'src, Types> {
    r#type()
        .separated_by(just(Token::Ctrl('|')))
        .collect::<Vec<_>>()
        .map_with_span(|types, span| Types {
            values: types.into(),
            span,
        })
}

pub fn default_value<'src>() -> impl PParser<'src, Option<CallChain>> {
    just(Token::Ctrl('='))
        .ignore_then(chain().ident_chain())
        .or_not()
}

pub fn declared_arg<'src>() -> impl PParser<'src, Spanned<Vec<DeclArg>>> {
    vararg()
        .then(ident())
        .then_ignore(just(Token::Ctrl(':')))
        .then(types())
        .then(default_value())
        .map_with_span(|(((is_vararg, name), types), default_value), span| {
            (is_vararg, name, types, default_value, span)
        })
        .separated_by(just(Token::Ctrl(',')))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .map(|arg_tuples| {
            arg_tuples
                .into_iter()
                .enumerate()
                .map(
                    |(position, (is_vararg, name, types, default_value, span))| DeclArg {
                        is_vararg,
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

pub fn native<'src>() -> impl PParser<'src, SpannedBool> {
    just(Token::Native)
        .or_not()
        .map_with_span(Spanned::ignore_value)
}

pub fn event<'src>() -> impl PParser<'src, Event> {
    let by = just(Token::By)
        .ignore_then(ident())
        .then(chain().args())
        .or_not();

    visibility()
        .then(native())
        .then_ignore(just(Token::Event))
        .then(ident())
        .map_with_span(|((visibility, is_native), name), span| EventDecl {
            visibility,
            is_native,
            name,
            span,
        })
        .then(declared_arg())
        .then(by)
        .validate(|((event_decl, decl_args), by), span, emitter| {
            if event_decl.is_native.is_some() && by.is_some() {
                emitter.emit(Rich::custom(span, "native events cannot have a by clause"));
            }
            ((event_decl, decl_args), by)
        })
        .then(block())
        .map_with_span(|(((decl, args), by), block), span| Event {
            decl,
            def: EventDef {
                actions: block.actions,
                conditions: block.conditions,
                by,
                args: args.inner_into(),
            },
            span,
        })
}

pub fn visibility<'src>() -> impl PParser<'src, Visibility> {
    just(Token::Pub)
        .or_not()
        .map(|token| {
            match token {
                Some(Token::Pub) => Visibility::Public,
                None => Visibility::Private,
                Some(token) => panic!("Error while parsing visibility. Expected is `pub` or nothing, but somehow it was {token}.")
            }
        })
}

pub fn r#enum<'src>() -> impl PParser<'src, Enum> {
    let constants = ident()
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .collect::<Vec<_>>();

    visibility()
        .then(native())
        .then_ignore(just(Token::Enum))
        .then(ident())
        .map_with_span(|((visibility, is_native), name), span| EnumDecl {
            visibility,
            is_native,
            name,
            span,
        })
        .then(constants.delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))))
        .map_with_span(|(decl, constants), span| Enum {
            decl,
            def: EnumDef { constants },
            span,
        })
}

pub fn spanned_bool<'src>(token: Token) -> impl PParser<'src, SpannedBool> {
    just(token)
        .or_not()
        .map_with_span(|it, span| it.map(|_| Spanned::new((), span)))
}

pub fn r#static<'src>() -> impl PParser<'src, SpannedBool> {
    spanned_bool(Token::Static)
}

pub fn vararg<'src>() -> impl PParser<'src, SpannedBool> {
    spanned_bool(Token::Vararg)
}

pub fn property<'src>() -> impl PParser<'src, PropertyDecl> {
    let use_restriction_tokens = (
        just(Token::GetVar),
        just(Token::SetVar),
        just(Token::Val),
        just(Token::Var),
    );
    let use_restriction = choice(use_restriction_tokens).map_with_span(Spanned::new);

    native()
        .then(use_restriction)
        .then(ident())
        .then_ignore(just(Token::Ctrl(':')))
        .then(r#type())
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
            PropertyDecl {
                name,
                is_native,
                use_restriction,
                r#type,
            }
        })
}

macro_rules! dup_op {
    ($lit:literal) => {
        just(Token::Ctrl($lit))
            .repeated()
            .exactly(2)
            .collect_exactly::<[_; 2]>()
    };
}

pub fn expression<'src>() -> impl PParser<'src, Expr> {
    recursive(|expr| {
        let chain = chain().ident_chain().map(Expr::Chain);

        let atom = chain.or(expr.delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))));

        let op = |c| just(Token::Ctrl(c));

        let neg = op('!')
            .repeated()
            .foldr(atom, |_, rhs| Expr::Neg(Box::new(rhs)));

        let and = neg.clone().foldl(
            dup_op!('&')
                .to(Expr::And as fn(_, _) -> _)
                .then(neg)
                .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        let or = and.clone().foldl(
            dup_op!('|')
                .to(Expr::Or as fn(_, _) -> _)
                .then(and)
                .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        or
    })
}

pub fn partial<'src>() -> impl PParser<'src, SpannedBool> {
    spanned_bool(Token::Partial)
}

pub fn path<'src>() -> impl PParser<'src, Path> {
    ident()
        .map(|it| it.value)
        .separated_by(dup_op!(':'))
        .collect::<Vec<_>>()
        .map_with_span(|segments, span| Path {
            name: PathName { segments },
            span,
        })
}

pub fn import<'src>() -> impl PParser<'src, Import> {
    just(Token::Import)
        .ignore_then(path())
        .then_ignore(just(Token::Ctrl(';')))
        .map_with_span(|path, span| Import { path, span })
}

pub fn function_decl<'src>() -> impl PParser<'src, FunctionDecl> {
    let return_type = just(Token::Ctrl('-'))
        .then(just(Token::Ctrl('>')))
        .ignore_then(r#type())
        .or_not();

    native()
        .then(r#static())
        .then_ignore(just(Token::Fn))
        .then(ident())
        .then(declared_arg())
        .then(return_type)
        .map(
            |((((is_native, is_static), name), args), return_type)| FunctionDecl {
                name,
                is_native,
                is_static,
                args: args.inner_into(),
                return_type,
            },
        )
}

pub fn r#struct<'src>() -> impl PParser<'src, Struct> {
    let member_function = function_decl();
    enum StructMember {
        Property(PropertyDecl),
        Function(FunctionDecl),
    }

    let property = property().map(StructMember::Property);
    let member_function = member_function.map(StructMember::Function);

    let struct_start = visibility()
        .then(partial())
        .then(native())
        .then_ignore(just(Token::Struct))
        .or_not()
        .map_with_span(|it, span| match it {
            Some(it) => it,
            None => (
                (Visibility::Public, Some(Spanned::new((), span))),
                Some(Spanned::new((), span)),
            ),
        });

    struct_start
        .then(ident())
        .then(decl_generics())
        .map_with_span(
            |((((visibility, is_partial), is_native), name), generics), span| StructDecl {
                visibility,
                is_partial,
                is_native,
                name,
                span,
                generics,
            },
        )
        .then(
            choice((property, member_function))
                .then_ignore(just(Token::Ctrl(';')))
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))),
        )
        .map_with_span(|(decl, members), span| {
            let mut functions: FunctionDecls = SmallVec::new();
            let mut properties: PropertyDecls = SmallVec::new();
            for member in members {
                match member {
                    StructMember::Function(function) => functions.push(function),
                    StructMember::Property(property) => properties.push(property),
                };
            }

            Struct {
                decl,
                def: StructDef {
                    properties,
                    functions,
                },
                span,
            }
        })
}

pub struct IdentChainParserResult<'src, 'a> {
    ident_chain: BBoxed<'src, 'a, CallChain>,
    args: BBoxed<'src, 'a, CallArgs>,
}

impl<'src, 'a> IdentChainParserResult<'src, 'a> {
    pub fn ident_chain(self) -> BBoxed<'src, 'a, CallChain> {
        self.ident_chain
    }

    pub fn args(self) -> BBoxed<'src, 'a, CallArgs> {
        self.args
    }
}

pub fn chain<'src: 'a, 'a>() -> IdentChainParserResult<'src, 'a> {
    let mut ident_chain = Recursive::declare();

    let arg_name_or_not = ident().then_ignore(just(Token::Ctrl('='))).or_not();
    let args = arg_name_or_not
        .then(ident_chain.clone())
        .map_with_span(|(named, call_chain), span| match named {
            Some(name) => CallArg::Named(name, call_chain, span),
            None => CallArg::Pos(call_chain),
        })
        .separated_by(just(Token::Ctrl(',')))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        .map_with_span(CallArgs::new);

    let literal = select! {
        Token::String(string) = span => Box::new(Call::String(string, span)),
        Token::Num(number) = span => Box::new(Call::Number(number, span)),
    };

    ident_chain.define(
        ident()
            .then(args.clone().or_not())
            .map_with_span(|(ident, args), span| {
                let call = match args {
                    Some(args) => Call::IdentArgs {
                        name: ident,
                        args,
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

pub fn assigment<'src>() -> impl PParser<'src, Action> {
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

pub fn block<'src>() -> impl PParser<'src, Block> {
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

pub fn rule<'src>() -> impl PParser<'src, Rule> {
    let rule_name = select! {
        Token::String(string) = span => Spanned::new(string, span)
    };

    visibility()
        .then_ignore(just(Token::Rule))
        .then(rule_name)
        .then(ident())
        .then(chain().args())
        .then(block())
        .map_with_span(
            |((((visibility, rule_name), ident), args), block), _span| Rule {
                visibility,
                conditions: block.conditions,
                actions: block.actions,
                name: rule_name,
                event: ident,
                args,
            },
        )
}

pub fn parser<'src>() -> impl PParser<'src, Cst> {
    let rule_parser = rule().map(Root::Rule);
    let event_parser = event().map(Root::Event);
    let enum_parser = r#enum().map(Root::Enum);
    let struct_parser = r#struct().map(Root::Struct);
    let import_parser = import().map(Root::Import);

    choice((
        rule_parser,
        event_parser,
        enum_parser,
        struct_parser,
        import_parser,
    ))
    .repeated()
    .collect::<Vec<_>>()
    .then_ignore(end())
    .map(Cst)
}
