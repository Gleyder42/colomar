extern crate core;

use super::super::span::{Offset, Span, SpanSourceId, StringId};
use chumsky::prelude::*;

use crate::analysis::interner::Interner;
use crate::InternedName;
use std::fmt::{Debug, Display, Formatter};
use std::string::String;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Token {
    Rule,
    Event,
    Cond,
    Native,
    Enum,
    By,
    Partial,
    Struct,
    GetVar,
    SetVar,
    Val,
    Var,
    Fn,
    Type,
    Import,
    Pub,
    Static,
    Vararg,
    Ident(StringId),
    String(StringId),
    Num(StringId),
    Ctrl(char),
    Dctrl([char; 2]), // Delete this maybe?
}

impl InternedName for char {
    fn name(&self, _: &dyn Interner) -> String {
        self.to_string()
    }
}

impl InternedName for Token {
    fn name(&self, interner: &dyn Interner) -> String {
        match self {
            Token::Num(string) | Token::String(string) | Token::Ident(string) => {
                interner.lookup_intern_string(*string)
            }
            token => token.to_string(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Rule => write!(f, "rule"),
            Token::Event => write!(f, "event"),
            Token::Cond => write!(f, "cond"),
            Token::Native => write!(f, "native"),
            Token::Enum => write!(f, "enum"),
            Token::By => write!(f, "by"),
            Token::Struct => write!(f, "struct"),
            Token::Partial => write!(f, "partial"),
            Token::GetVar => write!(f, "getvar"),
            Token::SetVar => write!(f, "setvar"),
            Token::Fn => write!(f, "fn"),
            Token::Type => write!(f, "type"),
            Token::Val => write!(f, "val"),
            Token::Var => write!(f, "var"),
            Token::Pub => write!(f, "pub"),
            Token::Static => write!(f, "static"),
            Token::Vararg => write!(f, "vararg"),
            Token::Import => write!(f, "import"),
            Token::Ident(string) => write!(f, "{string:?}"),
            Token::String(string) => write!(f, "{string:?}"),
            Token::Num(string) => write!(f, "{string:?}"),
            Token::Ctrl(ctrl) => write!(f, "{ctrl}"),
            Token::Dctrl(ctrl) => write!(f, "{}{}", ctrl[0], ctrl[1]),
        }
    }
}

pub type LexerExtra<'a> = extra::Err<Rich<'a, char>>;

pub fn lexer(
    span_source_id: SpanSourceId,
    string_interner: &dyn Interner,
) -> impl Parser<&str, Vec<(Token, Span)>, LexerExtra> {
    let num = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .slice()
        .map(|it: &str| Token::Num(string_interner.intern_string(it.to_owned())));

    let string = just('"')
        .ignore_then(any().filter(|c| *c != '"').repeated().collect::<String>())
        .then_ignore(just('"'))
        .map(|it| Token::String(string_interner.intern_string(it)));

    let ctrl = one_of("<>(){},.:|=;+-/*&|!-").map(Token::Ctrl);

    let ident = text::ident().map(|ident: &str| match ident {
        "rule" => Token::Rule,
        "cond" => Token::Cond,
        "native" => Token::Native,
        "event" => Token::Event,
        "enum" => Token::Enum,
        "by" => Token::By,
        "partial" => Token::Partial,
        "struct" => Token::Struct,
        "getvar" => Token::GetVar,
        "setvar" => Token::SetVar,
        "pub" => Token::Pub,
        "fn" => Token::Fn,
        "type" => Token::Type,
        "val" => Token::Val,
        "var" => Token::Var,
        "import" => Token::Import,
        "vararg" => Token::Vararg,
        "static" => Token::Static,
        _ => Token::Ident(string_interner.intern_string(ident.to_owned())),
    });

    let token = choice((ident, num, string, ctrl));

    token
        .padded()
        .map_with_span(move |tok, span| {
            (
                tok,
                Span {
                    offset: Offset::from(span),
                    context: span_source_id,
                },
            )
        })
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {}
