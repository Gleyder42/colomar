use std::fs::read;
use std::iter::Peekable;
use std::slice::Iter;
use crate::lexer::{CLOSED_BRACE, OPEN_BRACE, Token};
use crate::WorkshopTree;
use anyhow::{Error, Result};
use crate::parser::reader::Reader;
use strum_macros::Display;

macro_rules! error {
    ($($arg:tt)*) => {{
        let res = format!($($arg)*);
        Err(anyhow::Error::msg(res))
    }}
}

pub fn parse<'a>(reader: &'a Reader<Token>) -> Result<ParseTreeNode<'a>> {
    let mut nodes = Vec::new();

    loop {
        let token = reader.peek();
        if token.is_none() {
            break
        }
        let token = token.unwrap();

        match token {
            Token::Workshop => {
                if let Token::Struct = reader.peek_nth(1).unwrap() {
                    let result = parse_struct(&reader);
                    if let Ok(tree) = result {
                        nodes.push(Box::new(tree));
                    } else {
                        return error!("Could not parse struct");
                    }
                } else {
                    return error!("Unexpected token {}. Only struct is valid", token);
                }
            }
            _ => {
                return error!("Unexpected token {}. Only workshop is valid", token);
            }
        };
    };

    Ok(ParseTreeNode::Node(nodes))
}


fn parse_struct<'a>(reader: &'a Reader<'a, Token>) -> Result<ParseTreeNode<'a>> {
    let mut nodes = Vec::new();
    nodes.push(ParseTreeNode::create_leaf(reader.consume().unwrap()));
    nodes.push(ParseTreeNode::create_leaf(reader.consume().unwrap()));

    let option = reader.consume().unwrap();
    if let Token::Ident(name) = option {
        nodes.push(ParseTreeNode::create_leaf(option));
        reader.consume();
        loop {
            let token = reader.consume().unwrap();
            if let Token::ClosedBrace = token {
                reader.consume();
                reader.consume();
                break;
            }
            let result = parse_func(reader);
            if result.is_ok() {
                nodes.push(Box::new(result.unwrap()));
            }
        }
    } else {
        return error!("Unexpected token {}. Only ident is valid", option);
    }

    Ok(ParseTreeNode::Node(nodes))
}

fn parse_func<'a>(reader: &'a Reader<'a, Token>) -> Result<ParseTreeNode<'a>> {
    let mut nodes = Vec::new();
    loop {
        let token = reader.consume().unwrap();
        if let Token::LineBreak = token {
            break;
        }
        nodes.push(ParseTreeNode::create_leaf(token));
    }
    Ok(ParseTreeNode::Node(nodes))
}

#[derive(Display, Debug)]
pub enum ParseTreeNode<'a> {
    Node(Vec<Box<ParseTreeNode<'a>>>),
    Leaf(&'a Token),
}

impl<'a> ParseTreeNode<'a> {
    fn create_leaf(token: &Token) -> Box<ParseTreeNode> {
        Box::from(ParseTreeNode::Leaf(token))
    }
}

