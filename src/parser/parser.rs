use std::fmt;
use std::fmt::format;
use std::fs::read;
use std::io::Read;
use std::iter::Peekable;
use std::slice::Iter;
use std::vec::IntoIter;
use crate::lexer::{CLOSED_BRACE, OPEN_BRACE, Token};
use crate::WorkshopTree;
use anyhow::{Error, Result};
use peekmore::{PeekMore, PeekMoreIterator};
use strum_macros::Display;
use crate::parser::error_handler::ErrorRecovery;

macro_rules! error {
    ($($arg:tt)*) => {{
        let res = format!($($arg)*);
        Err(anyhow::Error::msg(res))
    }}
}

pub type TokenIter = PeekMoreIterator<IntoIter<Token>>;

struct Parser {
    pub tokens: TokenIter,
    pub error_handler: ErrorRecovery,
}

macro_rules! check_token {
        ($func_name:ident, $expression:expr) => {
            fn $func_name(&mut self, current_tokens: &String) -> Result<(), ParseError> {
                let token = self.tokens.next();
                if token.is_none() {
                    return Err(ParseError::new(format!(
                        "Expected {} token but got nothing near {}", $expression, current_tokens
                    )));
                }
                let token = token.unwrap();
                if $expression != token {
                    return Err(ParseError::new(format!(
                        "Expected {} token but got {} near {}", $expression, token, current_tokens
                    )));
                }
                return Ok(())
            }
        };

        ($func_name:ident, $pat:pat_param, $id:ident, $tp:ty) => {
            fn $func_name(&mut self, current_tokens: &String) -> Result<$tp, ParseError> {
                let token = self.tokens.next();
                if token.is_none() {
                    return Err(ParseError::new(format!(
                        "Expected {} token but got nothing near {}", "lol", current_tokens
                    )));
                }
                let token = token.unwrap();
                if let $pat = token {
                    Ok($id)
                } else {
                    Err(ParseError::new(format!(
                        "Expected {} token but got {} near {}", "lol", token, current_tokens
                    )))
                }
            }
        };
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens.into_iter().peekmore(),
            error_handler: ErrorRecovery::new(),
        }
    }

    pub fn parse(mut self) -> Result<ParseTreeNode, ParseError> {
        let mut root_node = ParseTreeNode::new(NodeType::Root);

        loop {
            let token = self.tokens.peek();
            if token.is_some() {
                break;
            }
        }
        Err(ParseError::new("Hello World".to_string()))
    }

    fn parse_struct_header(&mut self) -> Result<Box<ParseTreeNode>, ParseError> {
        let mut nodes = Vec::new();
        let current_tokens = self.peek_next();
        self.check_workshop_token(&current_tokens)?;
        self.check_struct_token(&current_tokens)?;
        let name = self.parse_ident_token(&current_tokens)?;

        nodes.push(ParseTreeNode::new_leaf(Token::Workshop));
        nodes.push(ParseTreeNode::new_leaf(Token::Struct));
        nodes.push(ParseTreeNode::new_leaf(name));

        Ok(ParseTreeNode::new_node(nodes, NodeType::StructDef))
    }

    check_token!(check_comma_token, Token::Comma);
    check_token!(check_open_brace_token, Token::OpenBrace);

    check_token!(check_ident, Token::Ident(value), value, String);

    fn parse_ident_token(&mut self, current_tokens: &String) -> Result<Token, ParseError> {
        let name_token = self.tokens.next();
        if let None = name_token {
            return Err(ParseError::new(format!(
                "Expected name token but got nothing near {}", current_tokens)));
        }
        let name_token = name_token.unwrap();
        if let Token::Ident(_) = name_token {
            Ok(name_token)
        } else {
            Err(ParseError::new(format!(
                "Expected ident token but got {} near {}", name_token, current_tokens
            )))
        }
    }

    fn parse_struct_function(&mut self) -> Result<Box<ParseTreeNode>, ParseError> {
        let mut nodes = Vec::new();
        let current_tokens = self.peek_next();
        self.check_workshop_token(&current_tokens)?;
        self.check_func_token(&current_tokens)?;
        let name = self.parse_ident_token(&current_tokens)?;

        nodes.push(ParseTreeNode::new_leaf(Token::Workshop));
        nodes.push(ParseTreeNode::new_leaf(Token::Func));
        nodes.push(ParseTreeNode::new_leaf(name));
        Ok(ParseTreeNode::new_node(nodes, NodeType::FuncDef))
    }

    fn check_func_token(&mut self, current_tokens: &String) -> Result<(), ParseError> {
        let func_token = self.tokens.next();
        if func_token.is_none() {
            return Err(ParseError::new(format!(
                "Expected func token but got nothing near {}", current_tokens
            )));
        }
        let func_token = func_token.unwrap();
        if Token::Struct != func_token {
            return Err(ParseError::new(format!(
                "Expected func token but got {} near {}", func_token, current_tokens
            )));
        }
        return Ok(())
    }

    fn check_struct_token(&mut self, current_tokens: &String) -> Result<(), ParseError> {
        let struct_token = self.tokens.next();
        if struct_token.is_none() {
            return Err(ParseError::new(format!(
                "Expected struct token but got nothing near {}", current_tokens
            )));
        }
        let struct_token = struct_token.unwrap();
        if Token::Struct != struct_token {
            return Err(ParseError::new(format!(
                "Expected struct token but got {} near {}", struct_token, current_tokens
            )));
        }
        return Ok(())
    }

    fn check_workshop_token(&mut self, current_tokens: &String) -> Result<(), ParseError> {
        let workshop_token = self.tokens.next();
        if workshop_token.is_none() {
            return Err(ParseError::new(format!(
                "Expected workshop token but got nothing near {}", current_tokens
            )));
        }
        let workshop_token = workshop_token.unwrap();
        if Token::Workshop != workshop_token {
            return Err(ParseError::new(format!(
                "Expected workshop token but got {} near {}", workshop_token, current_tokens
            )));
        }
        return Ok(())
    }

    fn peek_next(&mut self) -> String {
        self.tokens.peek_amount(6).iter()
            .filter_map(|token| {
                if let Option::Some(token) = token {
                    Some(token.display())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    error: String,
}

impl ParseError {
    fn new(msg: String) -> ParseError {
        ParseError { error: msg }
    }
}

#[derive(Debug)]
pub enum ParseTreeNode {
    Node(Vec<Box<ParseTreeNode>>, NodeType),
    Leaf(Token),
}

impl ParseTreeNode {
    fn new(node_type: NodeType) -> ParseTreeNode {
        ParseTreeNode::Node(Vec::new(), node_type)
    }

    fn new_node(nodes: Vec<Box<ParseTreeNode>>, token_type: NodeType) -> Box<ParseTreeNode> {
        Box::new(ParseTreeNode::Node(nodes, token_type))
    }

    fn new_leaf(token: Token) -> Box<ParseTreeNode> {
        Box::from(ParseTreeNode::Leaf(token))
    }
}

#[derive(Debug)]
pub enum NodeType {
    FuncDef,
    Root,
    Term,
    Expr,
    StructDef,
}

