use crate::parser::{ParserError, tokenizer::*};
use peg::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    And,
    Or,
    Equals,
    NotEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    Not,
    Is,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Int(i64),
    Bool(bool),
    Id(&'a str),
    Unary(Operator, Box<Expr<'a>>),
    Parens(Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, Operator, Box<Expr<'a>>),
    Call(&'a str, Vec<Expr<'a>>),
    Ternary(Box<Expr<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),
    Tuple(Vec<Expr<'a>>),
    Subscript(Box<Expr<'a>>, i64)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Expr(Expr<'a>),
    Assign(&'a str, Expr<'a>),
    SubscriptAssign(&'a str, i64, Expr<'a>),
    If(Expr<'a>, Vec<Statement<'a>>),
    ElseIf(Expr<'a>, Vec<Statement<'a>>),
    Else(Vec<Statement<'a>>),
    While(Expr<'a>, Vec<Statement<'a>>),
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
            op:[Token::Minus | Token::Plus | Token::And | Token::Or | Token::Not |
                Token::DoubleEquals | Token::NotEquals | Token::Greater |
                Token::GreaterEquals | Token::Less | Token::LessEquals | Token::Is] {
                match op {
                    Token::Minus         => Operator::Minus,
                    Token::Plus          => Operator::Plus,
                    Token::And           => Operator::And,
                    Token::Or            => Operator::Or,
                    Token::DoubleEquals  => Operator::Equals,
                    Token::NotEquals     => Operator::NotEquals,
                    Token::Greater       => Operator::Greater,
                    Token::GreaterEquals => Operator::GreaterEquals,
                    Token::Less          => Operator::Less,
                    Token::LessEquals    => Operator::LessEquals,
                    Token::Not           => Operator::Not,
                    Token::Is            => Operator::Is,
                    _ => unreachable!()
                }
            }

        // Trailing comma is mandatory for one elem but optional for multiple
        rule tuple_elements() -> Vec<Expr<'t>> =
            elems:((s:(expr() **<2,50> [Token::Comma]) [Token::Comma]? { s }) / (e:expr() [Token::Comma] { vec![e] }))  { elems }

        rule tuple() -> Expr<'t> =
            [Token::OpenParen] args:tuple_elements() [Token::CloseParen] { Expr::Tuple(args) }

        rule expr() -> Expr<'t> =
            // Lowest-precendence, right-associative ternary expression
            cond:precedence_expr() [Token::QuestionMark] pos:expr() [Token::Colon] neg:expr() {
                Expr::Ternary(Box::new(cond), Box::new(pos), Box::new(neg))
            }
            / tuple()
            / precedence_expr()

        rule precedence_expr() -> Expr<'t> = precedence!{
            // Lowest Precendence: Infix Operators, left-associative
            l:(@) op:operator() r:@ { Expr::Binary(Box::new(l), op, Box::new(r)) }
            --
            // Postfix operators
            e:@ [Token::OpenBracket] [Token::Int(idx)] [Token::CloseBracket] { Expr::Subscript(Box::new(e), idx) }
            --
            // Prefix operators
            op:operator() val:@ { Expr::Unary(op, Box::new(val)) }
            --
            // Highest: Atoms
            [Token::Identifier(id)][Token::OpenParen] args:(expr() ** [Token::Comma]) [Token::CloseParen] { Expr::Call(id, args) }
            [Token::Identifier(id)] { Expr::Id(id) }
            [Token::Int(val)] { Expr::Int(val) }
            [Token::Bool(val)] { Expr::Bool(val) }
            [Token::OpenParen] e:expr() [Token::CloseParen] { Expr::Parens(Box::new(e)) }
        }

        pub rule assign() -> Statement<'t> =
            [Token::Identifier(id)] [Token::Equals] e:expr() { Statement::Assign(id, e) }

        pub rule subscript_assign() -> Statement<'t> =
            [Token::Identifier(id)] [Token::OpenBracket] [Token::Int(idx)] [Token::CloseBracket] [Token::Equals] e:expr()
            { Statement::SubscriptAssign(id, idx, e) }

        pub rule statement_body() -> Vec<Statement<'t>> =
            [Token::OpenCurly] [Token::Newline]*
            ss:(if_chain() / (s:simple_statement() { vec![s] })) ** ([Token::Newline]+)
            [Token::Newline]* [Token::CloseCurly] {
                ss.into_iter().flatten().collect()
            }

        pub rule if_statement() -> Statement<'t> =
            [Token::If] cond:expr() body:statement_body() {
                Statement::If(cond, body)
            }

        pub rule else_if_statement() -> Statement<'t> =
            [Token::Else] [Token::If] cond:expr() body:statement_body() {
                Statement::ElseIf(cond, body)
            }

        pub rule else_statement() -> Statement<'t> =
            [Token::Else] body:statement_body() {
                Statement::Else(body)
            }

        /// An if-chain: if { } [else if { }]* [else { }]?
        /// No newlines required between parts
        pub rule if_chain() -> Vec<Statement<'t>> =
        head:if_statement() rest:([Token::Newline]* s:(else_if_statement() / else_statement()) { s })* {
            let mut v = vec![head];
            v.extend(rest);
            v
        }

        pub rule while_statement() -> Statement<'t> =
            [ Token::While ] cond:expr() body:statement_body() {
                Statement::While(cond, body)
            }

        /// Simple statements (not if-chains)
        pub rule simple_statement() -> Statement<'t> =
            assign() / subscript_assign() / (e:expr() { Statement::Expr(e) }) / while_statement()

        /// For use inside statement bodies
        pub rule statement() -> Statement<'t> =
            if_statement() / else_if_statement() / else_statement() / simple_statement()

        pub rule module() -> Module<'t> =
            ss:(if_chain() / (s:simple_statement() { vec![s] })) ** ([Token::Newline]+) eof() {
                Module { statements: ss.into_iter().flatten().collect() }
            }
    }
}

pub fn parse_tokens<'t>(tokens: &[Token<'t>]) -> Result<Module<'t>, ParserError<'t>> {
    parse_tree::module(tokens).map_err(|e| {
        let got = (tokens
            .get(e.location)
            .unwrap_or(&Token::Identifier("===EOF===")))
        .clone();
        ParserError::ParseTree(e, got)
    })
}
