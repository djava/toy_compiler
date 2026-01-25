use crate::ast;
use peg::error::ParseError;
use peg::str::LineCol;

mod tokenizer;
pub mod parse_tree;
pub mod to_ast;

pub use tokenizer::{Token, tokenize};

#[derive(Debug, Clone)]
pub enum ParserError<'a> {
    Tokenizer(ParseError<LineCol>, char),
    ParseTree(ParseError<usize>, Token<'a>),
}

pub fn parse<'a>(input: &'a str) -> Result<ast::Module, ParserError<'a>> {
    let tokens = tokenizer::tokenize(input)?;
    let parse_tree = parse_tree::parse_tokens(&tokens)?;
    let ast = to_ast::to_ast(parse_tree);

    Ok(ast)
}
