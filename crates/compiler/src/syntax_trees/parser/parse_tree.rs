use crate::syntax_trees::ValueType;

use super::{ParserError, tokenizer::*};
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
    Asterisk,
    LeftShift,
    RightShift,
    Divide,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Int(i64),
    Bool(bool),
    Id(&'a str),
    Unary(Operator, Box<Expr<'a>>),
    Parens(Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, Operator, Box<Expr<'a>>),
    Call(Box<Expr<'a>>, Vec<Expr<'a>>),
    Ternary(Box<Expr<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),
    Tuple(Vec<Expr<'a>>),
    Array(Vec<Expr<'a>>),
    Subscript(Box<Expr<'a>>, Box<Expr<'a>>),
    Lambda(Vec<&'a str>, Vec<Statement<'a>>),
    StringLiteral(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Expr(Expr<'a>),
    Assign(&'a str, Expr<'a>, Option<ValueType>),
    SubscriptAssign(Expr<'a>, Expr<'a>, Expr<'a>),
    If(Expr<'a>, Vec<Statement<'a>>),
    ElseIf(Expr<'a>, Vec<Statement<'a>>),
    Else(Vec<Statement<'a>>),
    While(Expr<'a>, Vec<Statement<'a>>),
    Return(Option<Expr<'a>>),
    For(Box<Statement<'a>>, Expr<'a>, Box<Statement<'a>>, Vec<Statement<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'a> {
    pub name: &'a str,
    pub params: Vec<(&'a str, ValueType)>,
    pub return_type: ValueType,
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'a> {
    pub functions: Vec<Function<'a>>,
}

parser! {
    // `'t` is the lifetime of the tokens specifically, not the lifetime
    // of the input (which is the slice of tokens, and only lives for as
    // long as whoever sliced them). The TOKENS themself have the same
    // lifetime as the source string, since their references are to the
    // names of the variables in the source
    grammar parse_tree<'t>() for [TokenValue<'t>] {
        rule eof() = [TokenValue::Newline]* ![_]

        rule shift_operator() -> Operator =
            ([TokenValue::Less] [TokenValue::Less] { Operator::LeftShift }) /
            ([TokenValue::Greater] [TokenValue::Greater] { Operator::RightShift })

        rule operator() -> Operator =
            shift_operator() /
            op:[TokenValue::Minus | TokenValue::Plus | TokenValue::And | TokenValue::Or | TokenValue::Not |
                TokenValue::DoubleEquals | TokenValue::NotEquals | TokenValue::Greater | TokenValue::GreaterEquals
                | TokenValue::Less | TokenValue::LessEquals | TokenValue::Is | TokenValue::Asterisk | TokenValue::Divide] {
                match op {
                    TokenValue::Minus         => Operator::Minus,
                    TokenValue::Plus          => Operator::Plus,
                    TokenValue::And           => Operator::And,
                    TokenValue::Or            => Operator::Or,
                    TokenValue::DoubleEquals  => Operator::Equals,
                    TokenValue::NotEquals     => Operator::NotEquals,
                    TokenValue::Greater       => Operator::Greater,
                    TokenValue::GreaterEquals => Operator::GreaterEquals,
                    TokenValue::Less          => Operator::Less,
                    TokenValue::LessEquals    => Operator::LessEquals,
                    TokenValue::Not           => Operator::Not,
                    TokenValue::Is            => Operator::Is,
                    TokenValue::Asterisk      => Operator::Asterisk,
                    TokenValue::Divide        => Operator::Divide,
                    _ => unreachable!()
                }
            }

        rule int_type() -> ValueType = [TokenValue::IntType] { ValueType::IntType }
        rule bool_type() -> ValueType = [TokenValue::BoolType] { ValueType::BoolType }
        rule primitive_type() -> ValueType = int_type() / bool_type()

        rule tuple_type() -> ValueType =
            [TokenValue::TupleType] [TokenValue::Less] types:(_type() ++ [TokenValue::Comma]) [TokenValue::Greater]
            { ValueType::TupleType(types) }

        rule array_type() -> ValueType =
            [TokenValue::ArrayType] [TokenValue::Less] typ:_type() [TokenValue::Greater]
            { ValueType::ArrayType(Box::new(typ)) }

        rule callable_type() -> ValueType =
            [TokenValue::CallableType] [TokenValue::Less]
                [TokenValue::OpenBracket] args:(_type() ** [TokenValue::Comma]) [TokenValue::CloseBracket]
                return_type:([TokenValue::Comma] ret:_type() {ret})?
            [TokenValue::Greater]
            { ValueType::FunctionType(args, Box::new(return_type.unwrap_or(ValueType::NoneType)))}

        rule none_type() -> ValueType = [TokenValue::NoneType] { ValueType::NoneType }

        rule string_type() -> ValueType = [TokenValue::StringType] { ValueType::ArrayType(Box::new(ValueType::CharType)) }

        rule _type() -> ValueType = array_type() / tuple_type() / primitive_type() / callable_type() / none_type() / string_type()

        // Trailing comma is mandatory for one elem but optional for multiple
        rule tuple_elements() -> Vec<Expr<'t>> =
            elems:((s:(expr() **<2,50> [TokenValue::Comma]) [TokenValue::Comma]? { s }) / (e:expr() [TokenValue::Comma] { vec![e] }))
            { elems }

        rule tuple() -> Expr<'t> =
            [TokenValue::OpenParen] elems:tuple_elements() [TokenValue::CloseParen] { Expr::Tuple(elems) }

        rule array_elements() -> Vec<Expr<'t>> =
            s:(expr() ** [TokenValue::Comma]) [TokenValue::Comma]? { s }

        rule array() -> Expr<'t> =
            [TokenValue::OpenBracket] elems:array_elements() [TokenValue::CloseBracket] { Expr::Array(elems) }

        rule lambda_oneliner_body() -> Vec<Statement<'t>> = e:expr() {
            vec![Statement::Return(Some(e))]
        }

        rule lambda() -> Expr<'t> =
            [TokenValue::Lambda] args:(([TokenValue::Identifier(id)] { id }) ** [TokenValue::Comma])
            [TokenValue::Colon]
            body:(lambda_oneliner_body() / statement_body())
            { Expr::Lambda(args, body) }

        rule ternary() -> Expr<'t> =
            cond:precedence_expr() [TokenValue::QuestionMark] pos:expr() [TokenValue::Colon] neg:expr() {
                Expr::Ternary(Box::new(cond), Box::new(pos), Box::new(neg))
            }

        rule expr() -> Expr<'t> =
              ternary()
            / tuple()
            / array()
            / lambda()
            / precedence_expr()

        rule precedence_expr() -> Expr<'t> = precedence!{
            // Lowest Precendence: Infix Operators, left-associative
            l:(@) op:operator() r:@ { Expr::Binary(Box::new(l), op, Box::new(r)) }
            --
            // Prefix operators
            op:operator() val:@ { Expr::Unary(op, Box::new(val)) }
            --
            // Postfix operators
            e:(@) [TokenValue::OpenBracket] idx:expr() [TokenValue::CloseBracket] { Expr::Subscript(Box::new(e), Box::new(idx)) }
            func:@ [TokenValue::OpenParen] args:(expr() ** [TokenValue::Comma]) [TokenValue::CloseParen] { Expr::Call(Box::new(func), args) }
            --
            // Highest: Atoms
            [TokenValue::Identifier(id)] { Expr::Id(id) }
            [TokenValue::Int(val)] { Expr::Int(val) }
            [TokenValue::Bool(val)] { Expr::Bool(val) }
            [TokenValue::OpenParen] e:expr() [TokenValue::CloseParen] { Expr::Parens(Box::new(e)) }
            [TokenValue::StringLiteral(s)] { Expr::StringLiteral(s) }
        }

        pub rule assign_type_hint() -> Option<ValueType> = ([TokenValue::Colon] t:_type() { t })?

        pub rule assign() -> Statement<'t> =
            [TokenValue::Identifier(id)] typ:assign_type_hint() [TokenValue::Equals] e:expr() { Statement::Assign(id, e, typ) }

        pub rule subscript_assign() -> Statement<'t> =
            [TokenValue::Identifier(container)] [TokenValue::OpenBracket] idx:expr() [TokenValue::CloseBracket] [TokenValue::Equals] e:expr()
            { Statement::SubscriptAssign(Expr::Id(container), idx, e) }

        pub rule statement_body() -> Vec<Statement<'t>> =
            [TokenValue::OpenCurly] [TokenValue::Newline]*
            ss:(if_chain() / (s:(while_statement() / for_statement() / simple_statement()) { vec![s] })) ** ([TokenValue::Newline]+)
            [TokenValue::Newline]* [TokenValue::CloseCurly] {
                ss.into_iter().flatten().collect()
            }

        pub rule if_statement() -> Statement<'t> =
            [TokenValue::If] cond:expr() body:statement_body() {
                Statement::If(cond, body)
            }

        pub rule else_if_statement() -> Statement<'t> =
            [TokenValue::Else] [TokenValue::If] cond:expr() body:statement_body() {
                Statement::ElseIf(cond, body)
            }

        pub rule else_statement() -> Statement<'t> =
            [TokenValue::Else] body:statement_body() {
                Statement::Else(body)
            }

        /// An if-chain: if { } [else if { }]* [else { }]?
        /// No newlines required between parts
        pub rule if_chain() -> Vec<Statement<'t>> =
            head:if_statement() rest:([TokenValue::Newline]* s:(else_if_statement() / else_statement()) { s })* {
            let mut v = vec![head];
                v.extend(rest);
                v
            }

        pub rule while_statement() -> Statement<'t> =
            [TokenValue::While] cond:expr() body:statement_body() { Statement::While(cond, body) }

        pub rule for_statement() -> Statement<'t> =
            [TokenValue::For] [TokenValue::OpenParen]
                init:simple_statement() [TokenValue::Semicolon]
                cond:expr() [TokenValue::Semicolon]
                incr:simple_statement()
            [TokenValue::CloseParen] body:statement_body() {
                Statement::For(Box::new(init), cond, Box::new(incr), body)
            }

        pub rule return_statement() -> Statement<'t> =
            [TokenValue::Return] val:expr()? { Statement::Return(val) }

        /// Simple statements (not if-chains)
        pub rule simple_statement() -> Statement<'t> =
            assign() / subscript_assign() / return_statement() / (e:expr() { Statement::Expr(e) })

        /// A param is a name with a type specifier
        pub rule param() -> (&'t str, ValueType) =
            [TokenValue::Identifier(name)] [TokenValue::Colon] t:_type() { (name, t) }

        pub rule param_list() -> Vec<(&'t str, ValueType)> =
            [TokenValue::OpenParen] params:(param() ** [TokenValue::Comma]) [TokenValue::CloseParen] { params }

        pub rule return_type() -> ValueType =
            [TokenValue::RightArrow] t:_type() { t }

        pub rule function() -> Function<'t> =
            [TokenValue::Fn] [TokenValue::Identifier(name)] params:param_list() ret:(return_type()?) body:statement_body()
                { Function { name, params, return_type: ret.unwrap_or(ValueType::NoneType), statements: body } }

        pub rule module() -> Module<'t> =
            [TokenValue::Newline]* functions:(function() ** ([TokenValue::Newline]+)) eof() { Module { functions } }
    }
}

pub fn parse_tokens<'t>(tokens: &[TokenValue<'t>]) -> Result<Module<'t>, ParserError<'t>> {
    parse_tree::module(tokens).map_err(|e| {
        let got = (tokens
            .get(e.location)
            .unwrap_or(&TokenValue::Identifier("===EOF===")))
        .clone();
        ParserError::ParseTree(e, got)
    })
}
