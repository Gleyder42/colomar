use std::fs::{File, read};
use std::io::{BufRead, BufReader, Error, Read};
use std::iter::Peekable;
use core::slice::Iter;
use std::process::id;
use crate::lexer::Token::Comma;

const STRING_OPERATOR: char = '\"';
const OPEN_PARENTHESES: char = '(';
const CLOSED_PARENTHESES: char = ')';
const OPEN_BRACE: char = '{';
const CLOSED_BRACE: char = '}';
const COMMA: char = ',';
const DOT: char = '.';
const CONDITION: &'static str = "condition";
const WHITESPACE: char = ' ';
const LINE_BREAK: char = '\n';

pub struct Lexer {
    state: State,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer { state: State::RootLevel }
    }

    pub fn lex(&mut self, reader: &mut BufReader<File>) -> Vec<Token> {
        let mut tokens = Vec::new();

        let mut char_reader: Vec<char> = reader.lines()
            .flat_map(|line|
                {
                    Self::line_to_char(line)
                }
            )
            .collect();

        let mut reader = char_reader.iter().peekable();
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
                _ => {
                    let ident = self.read_ident(&mut reader);
                    tokens.push(Token::Ident(ident));
                }
            }
        }
        tokens
    }

    fn line_to_char(line: Result<String, Error>) -> Vec<char> {
        let mut vec = line.expect("lines failed")
            .chars()
            .collect::<Vec<char>>();
        vec.push('\n');
        vec
    }

    fn read_string_literal(&self, reader: &mut Peekable<Iter<char>>) -> String {
        self.read_text(reader, |char| char == STRING_OPERATOR)
    }

    fn read_ident(&self, reader: &mut Peekable<Iter<char>>) -> String {
        self.read_text(reader, |char| self.has_ident_reached_end(char))
    }

    fn read_text<F>(&self, reader: &mut Peekable<Iter<char>>, func: F) -> String
        where F: Fn(char) -> bool
    {
        let mut string = String::new();
        loop {
            let option = reader.peek();
            if let Some(char) = option {
                let char = **char;
                if func(char) {
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
    }
}

#[derive(Debug)]
pub enum Token {
    Ident(String),
    StringLiteral(String),
    LineBreak,
    OpenParentheses,
    ClosedParentheses,
    OpenBrace,
    ClosedBrace,
    Dot,
    Comma,
}

#[derive(Eq, PartialEq)]
enum State {
    RootLevel,
    StringLiteral,
    NumberLiteral,
    RuleType,
    RuleBlock,
}

enum TokenType {
    Keyword,
    Identifier,
    Literal,
    Syntax,
}