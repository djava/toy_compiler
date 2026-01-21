use crate::parser::{ParserError, tokenizer::*};
use peg::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Int(i64),
    Id(&'a str),
    Unary(Operator, Box<Expr<'a>>),
    Parens(Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, Operator, Box<Expr<'a>>),
    Call(&'a str, Vec<Expr<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Expr(Expr<'a>),
    Assign(&'a str, Expr<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'t> {
    pub statements: Vec<Statement<'t>>,
}

parser! {
    // `'t` is the lifetime of the tokens specifically, not the lifetime
    // of the input (which is the slice of tokens, and only lives for as
    // long as whoever sliced them). The TOKENS themself have the same
    // lifetime as the source string, since their references are to the
    // names of the variables in the source
    grammar parse_tree<'t>() for [Token<'t>] {
        rule eof() = [Token::Newline]* ![_]

        rule operator() -> Operator =
            op:[Token::Minus | Token::Plus] {
                match op {
                    Token::Minus => Operator::Minus,
                    Token::Plus  => Operator::Plus,
                    _ => unreachable!()
                }
            }

        rule expr() -> Expr<'t> = precedence!{
            // Lowest Precendence: Binary Operators, left-associative
            l:(@) op:operator() r:@ { Expr::Binary(Box::new(l), op, Box::new(r)) }
            --
            // Middle: Unary operators
            op:operator() val:@ { Expr::Unary(op, Box::new(val)) }
            --
            // Highest: Atoms
            [Token::Identifier(id)][Token::OpenParen] args:(expr() ** [Token::Comma]) [Token::CloseParen] { Expr::Call(id, args) }
            [Token::Identifier(id)] { Expr::Id(id) }
            [Token::Int(val)] { Expr::Int(val) }
            [Token::OpenParen] e:expr() [Token::CloseParen] { Expr::Parens(Box::new(e)) }
        }

        pub rule assign() -> Statement<'t> =
            [Token::Identifier(id)] [Token::Equals] e:expr() { Statement::Assign(id, e) }

        pub rule statement() -> Statement<'t> =
            assign() / (e:expr() { Statement::Expr(e) })

        pub rule module() -> Module<'t> = s:(statement() ++ [Token::Newline]) eof() { Module { statements: s } }
    }
}

pub fn parse_tokens<'t>(tokens: &[Token<'t>]) -> Result<Module<'t>, ParserError<'t>> {
    parse_tree::module(tokens).map_err(|e| {
        let got = (tokens.get(e.location).unwrap_or(&Token::Identifier("===EOF==="))).clone();
        ParserError::ParseTree(e, got)
    })
}
