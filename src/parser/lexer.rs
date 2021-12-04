use std::fs::{File, read};
use std::io::{BufRead, BufReader, Error, Read};
use std::iter::Peekable;
use core::slice::Iter;
use std::fmt::{Display, Formatter};
use std::process::id;
use crate::lexer::Token::Comma;
use strum_macros::Display;

pub const STRING_OPERATOR: char = '\"';
pub const OPEN_PARENTHESES: char = '(';
pub const CLOSED_PARENTHESES: char = ')';
pub const OPEN_BRACE: char = '{';
pub const CLOSED_BRACE: char = '}';
pub const COMMA: char = ',';
pub const DOT: char = '.';
pub const CONDITION: &'static str = "condition";
pub const WHITESPACE: char = ' ';
pub const LINE_BREAK: char = '\n';
pub const COLON: char = ':';

pub struct Lexer;

impl Lexer {
    pub fn new() -> Lexer {
        Lexer { }
    }

    pub fn lex(&mut self, reader: BufReader<File>) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars: Vec<char> = reader.lines()
            .flat_map(|line| Lexer::line_to_chars(line))
            .collect();

        let mut reader = chars.iter().peekable();
        loop {
            let peek = reader.peek();
            if peek.is_none() {
                break;
            }
            let peek = **peek.unwrap();

            match peek {
                '\n' => {
                    reader.next();
                    tokens.push(Token::LineBreak);
                }
                WHITESPACE => {
                    reader.next();
                    continue;
                }
                STRING_OPERATOR => {
                    reader.next();
                    let literal = self.read_string_literal(&mut reader);
                    reader.next();
                    tokens.push(Token::StringLiteral(literal));
                }
                OPEN_PARENTHESES => {
                    reader.next();
                    tokens.push(Token::OpenParentheses);
                }
                CLOSED_PARENTHESES => {
                    reader.next();
                    tokens.push(Token::ClosedParentheses);
                }
                OPEN_BRACE => {
                    reader.next();
                    tokens.push(Token::OpenBrace);
                }
                CLOSED_BRACE => {
                    reader.next();
                    tokens.push(Token::ClosedBrace);
                }
                DOT => {
                    reader.next();
                    tokens.push(Token::Dot);
                }
                COMMA => {
                    reader.next();
                    tokens.push(Token::Comma);
                }
                COLON => {
                    reader.next();
                    tokens.push(Token::Colon)
                }
                _ => {
                    let ident = self.read_ident(&mut reader);
                    let token = match ident.as_str() {
                        "struct" => Token::Struct,
                        "rule" => Token::Rule,
                        "workshop" => Token::Workshop,
                        "func" => Token::Func,
                        "condition" => Token::Condition,
                        _ => Token::Ident(ident)
                    };
                    tokens.push(token);
                }
            }
        }
        tokens
    }

    fn read_string_literal(&self, reader: &mut Peekable<Iter<char>>) -> String {
        self.read_text(reader, |char| char == STRING_OPERATOR)
    }

    fn read_ident(&self, reader: &mut Peekable<Iter<char>>) -> String {
        self.read_text(reader, |char| self.has_ident_reached_end(char))
    }

    fn read_text<F>(&self, reader: &mut Peekable<Iter<char>>, has_reached_end: F) -> String
        where F: Fn(char) -> bool
    {
        let mut string = String::new();
        loop {
            let option = reader.peek();
            if let Some(char) = option {
                let char = **char;
                if has_reached_end(char) {
                    break;
                } else {
                    reader.next();
                    string.push(char);
                }
            } else {
                break;
            }
        }
        string
    }

    fn has_ident_reached_end(&self, char: char) -> bool {
        char == WHITESPACE
            || char == COMMA
            || char == OPEN_PARENTHESES
            || char == CLOSED_PARENTHESES
            || char == DOT
            || char == STRING_OPERATOR
            || char == LINE_BREAK
            || char == COLON
    }

    fn line_to_chars(line: Result<String, Error>) -> Vec<char> {
        let mut vec = line.expect("lines failed")
            .chars()
            .collect::<Vec<char>>();
        vec.push('\n');
        vec
    }
}

#[derive(Debug, PartialEq, Display)]
pub enum Token {
    Ident(String),
    StringLiteral(String),
    NumberLiteral(f64),
    Condition,
    LineBreak,
    OpenParentheses,
    ClosedParentheses,
    OpenBrace,
    ClosedBrace,
    Dot,
    Comma,
    Colon,
    Struct,
    Workshop,
    Rule,
    Func,
}

impl Token {
    pub fn display(&self) -> String {
        match self {
            Token::Ident(name) => name.clone(),
            Token::StringLiteral(name) => name.clone(),
            Token::NumberLiteral(number) => number.to_string(),
            Token::LineBreak => String::from("\n"),
            Token::OpenParentheses => String::from("("),
            Token::ClosedParentheses => String::from(")"),
            Token::OpenBrace => String::from("{"),
            Token::ClosedBrace => String::from("}"),
            Token::Dot => String::from("."),
            Token::Comma => String::from(","),
            Token::Colon => String::from(":"),
            Token::Struct => String::from("struct"),
            Token::Workshop => String::from("workshop"),
            Token::Rule => String::from("rule"),
            Token::Func => String::from("func"),
            Token::Condition => String::from("condition")
        }
    }
}