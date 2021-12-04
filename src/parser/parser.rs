use std::fmt;
use std::fmt::{Display, format, Formatter, write};
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

macro_rules! parse_error {
    ($($arg:tt)*) => {{
        Err(ParseError::new(format!($($arg)*)))
    }}
}

macro_rules! impl_parse_token_fn {
            ($func_name:ident, $pat:pat_param) => {
            fn $func_name(&mut self, current_tokens: &String) -> Result<Token, ParseError> {
                let token = self.tokens.next();
                if token.is_none() {
                    return Err(ParseError::new(format!(
                        "Expected \"{}\" token but got nothing near \"{}\"", stringify!($func_name), current_tokens
                    )));
                }
                let token = token.unwrap();
                if let $pat = token {
                    Ok(token)
                } else {
                    Err(ParseError::new(format!(
                        "Expected \"{}\" token but got \"{}\" near \"{}\"", stringify!($func_name), token, current_tokens
                    )))
                }
            }
        };
}

macro_rules! impl_check_token_fn {
        ($func_name:ident, $expression:expr) => {
            fn $func_name(&mut self, current_tokens: &String) -> Result<(), ParseError> {
                let token = self.tokens.next();
                if token.is_none() {
                    return Err(ParseError::new(format!(
                        "Expected \"{}\" token but got nothing near \"{}\"", $expression, current_tokens
                    )));
                }
                let token = token.unwrap();
                if $expression != token {
                    return Err(ParseError::new(format!(
                        "Expected \"{}\" token but got \"{}\" near \"{}\"", $expression, token, current_tokens
                    )));
                }
                return Ok(())
            }
        };
}

pub type TokenIter = PeekMoreIterator<IntoIter<Token>>;

pub struct Parser {
    pub tokens: TokenIter,
    pub error_handler: ErrorRecovery,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    error: String,
}

#[derive(Debug, Display)]
pub enum ParseTreeNode {
    Node(NodeType, Vec<Box<ParseTreeNode>>),
    Leaf(Token),
}

#[derive(Debug, Display)]
pub enum NodeType {
    FuncDef,
    Root,
    Term,
    Expr,
    StructDef,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens.into_iter().peekmore(),
            error_handler: ErrorRecovery::new(),
        }
    }

    pub fn parse(mut self) -> Result<ParseTreeNode, ParseError> {
        let mut nodes = Vec::new();

        loop {
            let token = self.tokens.peek();
            if token.is_none() {
                break;
            }
            let token = token.unwrap();

            match token {
                Token::Workshop => {
                    let result = self.parse_struct_header()?;
                    nodes.push(result);
                }
                _ => return parse_error!("Expected workshop token but got {}", token)
            }
        }
        Ok(ParseTreeNode::new(nodes, NodeType::Root))
    }

    fn parse_struct_header(&mut self) -> Result<Box<ParseTreeNode>, ParseError> {
        let mut nodes = Vec::new();
        let current_tokens = self.peek_context();
        let workshop_token = self.parse_workshop_token(&current_tokens)?;
        let struct_token = self.parse_struct_token(&current_tokens)?;
        let name = self.parse_ident_token(&current_tokens)?;
        let open_brace_token = self.parse_open_brace_token(&current_tokens)?;

        nodes.push(ParseTreeNode::new_leaf(workshop_token));
        nodes.push(ParseTreeNode::new_leaf(struct_token));
        nodes.push(ParseTreeNode::new_leaf(name));
        nodes.push(ParseTreeNode::new_leaf(open_brace_token));

        self.dismiss_line_break_tokens();

        loop {
            let next = self.tokens.peek();
            if let Some(next) = next {
                match next {
                    Token::Workshop | Token::Condition => {
                        let function_node = self.parse_struct_function()?;
                        nodes.push(function_node);
                    }
                    Token::ClosedBrace => break,
                    _ => return parse_error!(
                        "Expected workshop or condition token, but got \"{}\" near \"{}\"",
                        next, current_tokens
                    )
                }
            } else {
                println!("{}", next.is_none());
                return parse_error!("Token expected but none was supplied near \"{}\"", current_tokens);
            }
        }

        let closed_brace_token = self.parse_closed_brace_token(&current_tokens)?;
        nodes.push(ParseTreeNode::new_leaf(closed_brace_token));

        self.dismiss_line_break_tokens();

        Ok(ParseTreeNode::new_node(nodes, NodeType::StructDef))
    }

    fn parse_struct_function(&mut self) -> Result<Box<ParseTreeNode>, ParseError> {
        let mut nodes = Vec::new();
        let current_tokens = self.peek_context();
        let workshop_token = self.parse_workshop_token(&current_tokens)?;

        let func_token = self.parse_func_token(&current_tokens)?;

        let func_name = self.parse_ident_token(&current_tokens)?;
        let open_parentheses_token = self.parse_open_parentheses_token(&current_tokens)?;
        let close_parentheses_token = self.parse_closed_parentheses_token(&current_tokens)?;

        nodes.push(ParseTreeNode::new_leaf(workshop_token));
        nodes.push(ParseTreeNode::new_leaf(func_token));
        nodes.push(ParseTreeNode::new_leaf(func_name));
        nodes.push(ParseTreeNode::new_leaf(open_parentheses_token));
        nodes.push(ParseTreeNode::new_leaf(close_parentheses_token));

        self.dismiss_line_break_tokens();

        Ok(ParseTreeNode::new_node(nodes, NodeType::FuncDef))
    }

    impl_parse_token_fn!(parse_comma_token, Token::Comma);
    impl_parse_token_fn!(parse_open_brace_token, Token::OpenBrace);
    impl_parse_token_fn!(parse_closed_brace_token, Token::ClosedBrace);
    impl_parse_token_fn!(parse_func_token, Token::Func);
    impl_parse_token_fn!(parse_struct_token, Token::Struct);
    impl_parse_token_fn!(parse_workshop_token, Token::Workshop);
    impl_parse_token_fn!(parse_open_parentheses_token, Token::OpenParentheses);
    impl_parse_token_fn!(parse_closed_parentheses_token, Token::ClosedParentheses);

    impl_parse_token_fn!(parse_ident_token, Token::Ident(_));

    fn dismiss_line_break_tokens(&mut self) {
        loop {
            let token = self.tokens.peek();
            if let Option::Some(token) = token {
                if let Token::LineBreak = token {
                    self.tokens.next();
                } else {
                    break;
                };
            } else {
                break
            }
        }
    }

    fn peek_context(&mut self) -> String {
        self.tokens.peek_amount(5).iter()
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

impl ParseError {
    fn new(msg: String) -> ParseError {
        ParseError { error: msg }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.error)
    }
}

impl ParseTreeNode {
    fn new(nodes: Vec<Box<ParseTreeNode>>, node_type: NodeType) -> ParseTreeNode {
        ParseTreeNode::Node(node_type, nodes)
    }

    fn new_node(nodes: Vec<Box<ParseTreeNode>>, token_type: NodeType) -> Box<ParseTreeNode> {
        Box::new(ParseTreeNode::Node(token_type, nodes))
    }

    fn new_leaf(token: Token) -> Box<ParseTreeNode> {
        Box::from(ParseTreeNode::Leaf(token))
    }
}
