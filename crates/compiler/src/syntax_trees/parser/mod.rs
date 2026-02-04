use super::ast;
use peg::error::ParseError;
use peg::str::LineCol;

pub mod parse_tree;
pub mod to_ast;
mod tokenizer;

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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use test_support::compiler::syntax_trees::{ast, parser::*, shared::*};

    use parse_tree as pt;

    pub struct ParserTestCase<'a> {
        pub input_str: &'a str,
        pub expected_tokens: Vec<Token<'a>>,
        pub expected_parse_tree: parse_tree::Module<'a>,
        pub expected_ast: ast::Module,
    }

    impl ParserTestCase<'_> {
        pub fn run(self) {
            let tokens = tokenize(self.input_str).unwrap();
            assert_eq!(tokens, self.expected_tokens);

            let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
            assert_eq!(parse_tree, self.expected_parse_tree);

            let ast = to_ast::to_ast(parse_tree);
            assert_eq!(ast, self.expected_ast);
        }
    }

    #[test]
    fn test_simple_constant() {
        let tc = ParserTestCase {
            input_str: r"1",
            expected_tokens: vec![Token::Int(1)],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Int(1))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                    1,
                )))],
                types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_neg_constant() {
        let tc = ParserTestCase {
            input_str: r"-100",
            expected_tokens: vec![Token::Int(-100)],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Int(-100))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                    -100,
                )))],
                types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_binop() {
        let tc = ParserTestCase {
            input_str: r"1 + 2",
            expected_tokens: vec![Token::Int(1), Token::Plus, Token::Int(2)],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                    Box::new(pt::Expr::Int(1)),
                    pt::Operator::Plus,
                    Box::new(pt::Expr::Int(2)),
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                    Box::new(ast::Expr::Constant(Value::I64(1))),
                    BinaryOperator::Add,
                    Box::new(ast::Expr::Constant(Value::I64(2))),
                ))],
                types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_parens() {
        let tc = ParserTestCase {
            input_str: r"1 + (2 + 3)",
            expected_tokens: vec![
                Token::Int(1),
                Token::Plus,
                Token::OpenParen,
                Token::Int(2),
                Token::Plus,
                Token::Int(3),
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                    Box::new(pt::Expr::Int(1)),
                    pt::Operator::Plus,
                    Box::new(pt::Expr::Parens(Box::new(pt::Expr::Binary(
                        Box::new(pt::Expr::Int(2)),
                        pt::Operator::Plus,
                        Box::new(pt::Expr::Int(3)),
                    )))),
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                    Box::new(ast::Expr::Constant(Value::I64(1))),
                    BinaryOperator::Add,
                    Box::new(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(Value::I64(2))),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::Constant(Value::I64(3))),
                    )),
                ))],
                types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_unaryop() {
        let tc = ParserTestCase {
            input_str: r"-(1)",
            expected_tokens: vec![
                Token::Minus,
                Token::OpenParen,
                Token::Int(1),
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Unary(
                    pt::Operator::Minus,
                    Box::new(pt::Expr::Parens(Box::new(pt::Expr::Int(1)))),
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::UnaryOp(
                    UnaryOperator::Minus,
                    Box::new(ast::Expr::Constant(Value::I64(1))),
                ))],
                types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_assign() {
        let tc = ParserTestCase {
            input_str: r"x = 1000 + -21912983",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::Equals,
                Token::Int(1000),
                Token::Plus,
                Token::Int(-21912983),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Assign(
                    "x",
                    pt::Expr::Binary(
                        Box::new(pt::Expr::Int(1000)),
                        pt::Operator::Plus,
                        Box::new(pt::Expr::Int(-21912983)),
                    ),
                )],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Assign(
                    AssignDest::Id(Identifier::from("x")),
                    ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(Value::I64(1000))),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::Constant(Value::I64(-21912983))),
                    ),
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_simple_arg() {
        let tc = ParserTestCase {
            input_str: r"print(x)",
            expected_tokens: vec![
                Token::Identifier("print"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Call(
                    "print",
                    vec![pt::Expr::Id("x")],
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Call(
                    Identifier::from("print"),
                    vec![ast::Expr::Id(Identifier::from("x"))],
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_no_arg() {
        let tc = ParserTestCase {
            input_str: r"oogabooga()",
            expected_tokens: vec![
                Token::Identifier("oogabooga"),
                Token::OpenParen,
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Call("oogabooga", vec![]))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Call(
                    Identifier::from("oogabooga"),
                    vec![],
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_complex_arg() {
        let tc = ParserTestCase {
            input_str: r"print((1 + (2 + 4)) - read_int())",
            expected_tokens: vec![
                Token::Identifier("print"),
                Token::OpenParen,
                Token::OpenParen,
                Token::Int(1),
                Token::Plus,
                Token::OpenParen,
                Token::Int(2),
                Token::Plus,
                Token::Int(4),
                Token::CloseParen,
                Token::CloseParen,
                Token::Minus,
                Token::Identifier("read_int"),
                Token::OpenParen,
                Token::CloseParen,
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Call(
                    "print",
                    vec![pt::Expr::Binary(
                        Box::new(pt::Expr::Parens(Box::new(pt::Expr::Binary(
                            Box::new(pt::Expr::Int(1)),
                            pt::Operator::Plus,
                            Box::new(pt::Expr::Parens(Box::new(pt::Expr::Binary(
                                Box::new(pt::Expr::Int(2)),
                                pt::Operator::Plus,
                                Box::new(pt::Expr::Int(4)),
                            )))),
                        )))),
                        pt::Operator::Minus,
                        Box::new(pt::Expr::Call("read_int", vec![])),
                    )],
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Call(
                    Identifier::from("print"),
                    vec![ast::Expr::BinaryOp(
                        Box::new(ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Constant(Value::I64(1))),
                            BinaryOperator::Add,
                            Box::new(ast::Expr::BinaryOp(
                                Box::new(ast::Expr::Constant(Value::I64(2))),
                                BinaryOperator::Add,
                                Box::new(ast::Expr::Constant(Value::I64(4))),
                            )),
                        )),
                        BinaryOperator::Subtract,
                        Box::new(ast::Expr::Call(Identifier::from("read_int"), vec![])),
                    )],
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_multi_arg() {
        let tc = ParserTestCase {
            input_str: r"print(x, y, 1, 3, 1000)",
            expected_tokens: vec![
                Token::Identifier("print"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Comma,
                Token::Identifier("y"),
                Token::Comma,
                Token::Int(1),
                Token::Comma,
                Token::Int(3),
                Token::Comma,
                Token::Int(1000),
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Call(
                    "print",
                    vec![
                        pt::Expr::Id("x"),
                        pt::Expr::Id("y"),
                        pt::Expr::Int(1),
                        pt::Expr::Int(3),
                        pt::Expr::Int(1000),
                    ],
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Call(
                    Identifier::from("print"),
                    vec![
                        ast::Expr::Id(Identifier::from("x")),
                        ast::Expr::Id(Identifier::from("y")),
                        ast::Expr::Constant(Value::I64(1)),
                        ast::Expr::Constant(Value::I64(3)),
                        ast::Expr::Constant(Value::I64(1000)),
                    ],
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_assign_to_call() {
        let tc = ParserTestCase {
            input_str: "x = Fffoo(1)",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::Equals,
                Token::Identifier("Fffoo"),
                Token::OpenParen,
                Token::Int(1),
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Assign(
                    "x",
                    pt::Expr::Call("Fffoo", vec![pt::Expr::Int(1)]),
                )],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Assign(
                    AssignDest::Id(Identifier::from("x")),
                    ast::Expr::Call(
                        Identifier::from("Fffoo"),
                        vec![ast::Expr::Constant(Value::I64(1))],
                    ),
                )],
                types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_multiline() {
        let tc = ParserTestCase {
            input_str: r"x = 100
            print(1000)
            whatevn = x
            101010 + 1001010
        ",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::Equals,
                Token::Int(100),
                Token::Newline,
                Token::Identifier("print"),
                Token::OpenParen,
                Token::Int(1000),
                Token::CloseParen,
                Token::Newline,
                Token::Identifier("whatevn"),
                Token::Equals,
                Token::Identifier("x"),
                Token::Newline,
                Token::Int(101010),
                Token::Plus,
                Token::Int(1001010),
                Token::Newline,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![
                    pt::Statement::Assign("x", pt::Expr::Int(100)),
                    pt::Statement::Expr(pt::Expr::Call("print", vec![pt::Expr::Int(1000)])),
                    pt::Statement::Assign("whatevn", pt::Expr::Id("x")),
                    pt::Statement::Expr(pt::Expr::Binary(
                        Box::new(pt::Expr::Int(101010)),
                        pt::Operator::Plus,
                        Box::new(pt::Expr::Int(1001010)),
                    )),
                ],
            },
            expected_ast: ast::Module {
                body: vec![
                    ast::Statement::Assign(
                        AssignDest::Id(Identifier::from("x")),
                        ast::Expr::Constant(Value::I64(100)),
                    ),
                    ast::Statement::Expr(ast::Expr::Call(
                        Identifier::from("print"),
                        vec![ast::Expr::Constant(Value::I64(1000))],
                    )),
                    ast::Statement::Assign(
                        AssignDest::Id(Identifier::from("whatevn")),
                        ast::Expr::Id(Identifier::from("x")),
                    ),
                    ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(Value::I64(101010))),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::Constant(Value::I64(1001010))),
                    )),
                ],
                types: HashMap::new(),
            },
        };

        tc.run()
    }

    #[test]
    fn test_space_delimited_unary() {
        let tc = ParserTestCase {
            input_str: r"2 + - 5",
            expected_tokens: vec![Token::Int(2), Token::Plus, Token::Minus, Token::Int(5)],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                    Box::new(pt::Expr::Int(2)),
                    pt::Operator::Plus,
                    Box::new(pt::Expr::Unary(
                        pt::Operator::Minus,
                        Box::new(pt::Expr::Int(5)),
                    )),
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                    Box::new(ast::Expr::Constant(Value::I64(2))),
                    BinaryOperator::Add,
                    Box::new(ast::Expr::UnaryOp(
                        UnaryOperator::Minus,
                        Box::new(ast::Expr::Constant(Value::I64(5))),
                    )),
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_bool_op_simple() {
        let tc = ParserTestCase {
            input_str: r"false && true",
            expected_tokens: vec![Token::Bool(false), Token::And, Token::Bool(true)],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                    Box::new(pt::Expr::Bool(false)),
                    pt::Operator::And,
                    Box::new(pt::Expr::Bool(true)),
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                    Box::new(ast::Expr::Constant(Value::Bool(false))),
                    BinaryOperator::And,
                    Box::new(ast::Expr::Constant(Value::Bool(true))),
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_bool_op_complex() {
        let tc = ParserTestCase {
            input_str: r"truefoofalse(!(false && true) || (false == true) >= (1 - false))",
            expected_tokens: vec![
                Token::Identifier("truefoofalse"),
                Token::OpenParen,
                Token::Not,
                Token::OpenParen,
                Token::Bool(false),
                Token::And,
                Token::Bool(true),
                Token::CloseParen,
                Token::Or,
                Token::OpenParen,
                Token::Bool(false),
                Token::DoubleEquals,
                Token::Bool(true),
                Token::CloseParen,
                Token::GreaterEquals,
                Token::OpenParen,
                Token::Int(1),
                Token::Minus,
                Token::Bool(false),
                Token::CloseParen,
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Call(
                    "truefoofalse",
                    vec![pt::Expr::Binary(
                        Box::new(pt::Expr::Binary(
                            Box::new(pt::Expr::Unary(
                                pt::Operator::Not,
                                Box::new(pt::Expr::Parens(Box::new(pt::Expr::Binary(
                                    Box::new(pt::Expr::Bool(false)),
                                    pt::Operator::And,
                                    Box::new(pt::Expr::Bool(true)),
                                )))),
                            )),
                            pt::Operator::Or,
                            Box::new(pt::Expr::Parens(Box::new(pt::Expr::Binary(
                                Box::new(pt::Expr::Bool(false)),
                                pt::Operator::Equals,
                                Box::new(pt::Expr::Bool(true)),
                            )))),
                        )),
                        pt::Operator::GreaterEquals,
                        Box::new(pt::Expr::Parens(Box::new(pt::Expr::Binary(
                            Box::new(pt::Expr::Int(1)),
                            pt::Operator::Minus,
                            Box::new(pt::Expr::Bool(false)),
                        )))),
                    )],
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Call(
                    Identifier::from("truefoofalse"),
                    vec![ast::Expr::BinaryOp(
                        Box::new(ast::Expr::BinaryOp(
                            Box::new(ast::Expr::UnaryOp(
                                UnaryOperator::Not,
                                Box::new(ast::Expr::BinaryOp(
                                    Box::new(ast::Expr::Constant(Value::Bool(false))),
                                    BinaryOperator::And,
                                    Box::new(ast::Expr::Constant(Value::Bool(true))),
                                )),
                            )),
                            BinaryOperator::Or,
                            Box::new(ast::Expr::BinaryOp(
                                Box::new(ast::Expr::Constant(Value::Bool(false))),
                                BinaryOperator::Equals,
                                Box::new(ast::Expr::Constant(Value::Bool(true))),
                            )),
                        )),
                        BinaryOperator::GreaterEquals,
                        Box::new(ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Constant(Value::I64(1))),
                            BinaryOperator::Subtract,
                            Box::new(ast::Expr::Constant(Value::Bool(false))),
                        )),
                    )],
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_ternary_simple() {
        let tc = ParserTestCase {
            input_str: r"true ? 1 : 2",
            expected_tokens: vec![
                Token::Bool(true),
                Token::QuestionMark,
                Token::Int(1),
                Token::Colon,
                Token::Int(2),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Ternary(
                    Box::new(pt::Expr::Bool(true)),
                    Box::new(pt::Expr::Int(1)),
                    Box::new(pt::Expr::Int(2)),
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Ternary(
                    Box::new(ast::Expr::Constant(Value::Bool(true))),
                    Box::new(ast::Expr::Constant(Value::I64(1))),
                    Box::new(ast::Expr::Constant(Value::I64(2))),
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_ternary_nested() {
        // Right-associative: a ? b : c ? d : e  =>  a ? b : (c ? d : e)
        let tc = ParserTestCase {
            input_str: r"true ? 1 : false ? 2 : 3",
            expected_tokens: vec![
                Token::Bool(true),
                Token::QuestionMark,
                Token::Int(1),
                Token::Colon,
                Token::Bool(false),
                Token::QuestionMark,
                Token::Int(2),
                Token::Colon,
                Token::Int(3),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Ternary(
                    Box::new(pt::Expr::Bool(true)),
                    Box::new(pt::Expr::Int(1)),
                    Box::new(pt::Expr::Ternary(
                        Box::new(pt::Expr::Bool(false)),
                        Box::new(pt::Expr::Int(2)),
                        Box::new(pt::Expr::Int(3)),
                    )),
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Ternary(
                    Box::new(ast::Expr::Constant(Value::Bool(true))),
                    Box::new(ast::Expr::Constant(Value::I64(1))),
                    Box::new(ast::Expr::Ternary(
                        Box::new(ast::Expr::Constant(Value::Bool(false))),
                        Box::new(ast::Expr::Constant(Value::I64(2))),
                        Box::new(ast::Expr::Constant(Value::I64(3))),
                    )),
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_ternary_with_binop() {
        // Ternary has lower precedence than binary ops
        let tc = ParserTestCase {
            input_str: r"1 + 2 ? 3 : 4",
            expected_tokens: vec![
                Token::Int(1),
                Token::Plus,
                Token::Int(2),
                Token::QuestionMark,
                Token::Int(3),
                Token::Colon,
                Token::Int(4),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Ternary(
                    Box::new(pt::Expr::Binary(
                        Box::new(pt::Expr::Int(1)),
                        pt::Operator::Plus,
                        Box::new(pt::Expr::Int(2)),
                    )),
                    Box::new(pt::Expr::Int(3)),
                    Box::new(pt::Expr::Int(4)),
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Ternary(
                    Box::new(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::Constant(Value::I64(2))),
                    )),
                    Box::new(ast::Expr::Constant(Value::I64(3))),
                    Box::new(ast::Expr::Constant(Value::I64(4))),
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_simple() {
        let tc = ParserTestCase {
            input_str: r"if true { 1 }",
            expected_tokens: vec![
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::Int(1),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::If(
                    pt::Expr::Bool(true),
                    vec![pt::Statement::Expr(pt::Expr::Int(1))],
                )],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Conditional(
                    ast::Expr::Constant(Value::Bool(true)),
                    vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                        1,
                    )))],
                    vec![],
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_else() {
        let tc = ParserTestCase {
            input_str: r"if true { 1 }
else { 2 }",
            expected_tokens: vec![
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::Int(1),
                Token::CloseCurly,
                Token::Newline,
                Token::Else,
                Token::OpenCurly,
                Token::Int(2),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![
                    pt::Statement::If(
                        pt::Expr::Bool(true),
                        vec![pt::Statement::Expr(pt::Expr::Int(1))],
                    ),
                    pt::Statement::Else(vec![pt::Statement::Expr(pt::Expr::Int(2))]),
                ],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Conditional(
                    ast::Expr::Constant(Value::Bool(true)),
                    vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                        1,
                    )))],
                    vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                        2,
                    )))],
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_else_if_else() {
        let tc = ParserTestCase {
            input_str: r"if true { 1 }
else if false { 2 }
else { 3 }",
            expected_tokens: vec![
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::Int(1),
                Token::CloseCurly,
                Token::Newline,
                Token::Else,
                Token::If,
                Token::Bool(false),
                Token::OpenCurly,
                Token::Int(2),
                Token::CloseCurly,
                Token::Newline,
                Token::Else,
                Token::OpenCurly,
                Token::Int(3),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![
                    pt::Statement::If(
                        pt::Expr::Bool(true),
                        vec![pt::Statement::Expr(pt::Expr::Int(1))],
                    ),
                    pt::Statement::ElseIf(
                        pt::Expr::Bool(false),
                        vec![pt::Statement::Expr(pt::Expr::Int(2))],
                    ),
                    pt::Statement::Else(vec![pt::Statement::Expr(pt::Expr::Int(3))]),
                ],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Conditional(
                    ast::Expr::Constant(Value::Bool(true)),
                    vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                        1,
                    )))],
                    vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(false)),
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                            2,
                        )))],
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                            3,
                        )))],
                    )],
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_with_complex_condition() {
        let tc = ParserTestCase {
            input_str: r"if x == 1 { print(x) }",
            expected_tokens: vec![
                Token::If,
                Token::Identifier("x"),
                Token::DoubleEquals,
                Token::Int(1),
                Token::OpenCurly,
                Token::Identifier("print"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::If(
                    pt::Expr::Binary(
                        Box::new(pt::Expr::Id("x")),
                        pt::Operator::Equals,
                        Box::new(pt::Expr::Int(1)),
                    ),
                    vec![pt::Statement::Expr(pt::Expr::Call(
                        "print",
                        vec![pt::Expr::Id("x")],
                    ))],
                )],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Conditional(
                    ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Id(Identifier::from("x"))),
                        BinaryOperator::Equals,
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                    ),
                    vec![ast::Statement::Expr(ast::Expr::Call(
                        Identifier::from("print"),
                        vec![ast::Expr::Id(Identifier::from("x"))],
                    ))],
                    vec![],
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_multiline_body() {
        let tc = ParserTestCase {
            input_str: r"if true { x = 1


    print(x) }",
            expected_tokens: vec![
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::Equals,
                Token::Int(1),
                Token::Newline,
                Token::Newline,
                Token::Newline,
                Token::Identifier("print"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::If(
                    pt::Expr::Bool(true),
                    vec![
                        pt::Statement::Assign("x", pt::Expr::Int(1)),
                        pt::Statement::Expr(pt::Expr::Call("print", vec![pt::Expr::Id("x")])),
                    ],
                )],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Conditional(
                    ast::Expr::Constant(Value::Bool(true)),
                    vec![
                        ast::Statement::Assign(
                            AssignDest::Id(Identifier::from("x")),
                            ast::Expr::Constant(Value::I64(1)),
                        ),
                        ast::Statement::Expr(ast::Expr::Call(
                            Identifier::from("print"),
                            vec![ast::Expr::Id(Identifier::from("x"))],
                        )),
                    ],
                    vec![],
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_empty_body() {
        let tc = ParserTestCase {
            input_str: r"if true { }",
            expected_tokens: vec![
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::If(pt::Expr::Bool(true), vec![])],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Conditional(
                    ast::Expr::Constant(Value::Bool(true)),
                    vec![],
                    vec![],
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_chained_else_ifs() {
        let tc = ParserTestCase {
            input_str: r"if x == 1 { 1 }
else if x == 2 { 2 }
else if x == 3 { 3 }
else if x == 4 { 4 }
else { 5 }",
            expected_tokens: vec![
                Token::If,
                Token::Identifier("x"),
                Token::DoubleEquals,
                Token::Int(1),
                Token::OpenCurly,
                Token::Int(1),
                Token::CloseCurly,
                Token::Newline,
                Token::Else,
                Token::If,
                Token::Identifier("x"),
                Token::DoubleEquals,
                Token::Int(2),
                Token::OpenCurly,
                Token::Int(2),
                Token::CloseCurly,
                Token::Newline,
                Token::Else,
                Token::If,
                Token::Identifier("x"),
                Token::DoubleEquals,
                Token::Int(3),
                Token::OpenCurly,
                Token::Int(3),
                Token::CloseCurly,
                Token::Newline,
                Token::Else,
                Token::If,
                Token::Identifier("x"),
                Token::DoubleEquals,
                Token::Int(4),
                Token::OpenCurly,
                Token::Int(4),
                Token::CloseCurly,
                Token::Newline,
                Token::Else,
                Token::OpenCurly,
                Token::Int(5),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![
                    pt::Statement::If(
                        pt::Expr::Binary(
                            Box::new(pt::Expr::Id("x")),
                            pt::Operator::Equals,
                            Box::new(pt::Expr::Int(1)),
                        ),
                        vec![pt::Statement::Expr(pt::Expr::Int(1))],
                    ),
                    pt::Statement::ElseIf(
                        pt::Expr::Binary(
                            Box::new(pt::Expr::Id("x")),
                            pt::Operator::Equals,
                            Box::new(pt::Expr::Int(2)),
                        ),
                        vec![pt::Statement::Expr(pt::Expr::Int(2))],
                    ),
                    pt::Statement::ElseIf(
                        pt::Expr::Binary(
                            Box::new(pt::Expr::Id("x")),
                            pt::Operator::Equals,
                            Box::new(pt::Expr::Int(3)),
                        ),
                        vec![pt::Statement::Expr(pt::Expr::Int(3))],
                    ),
                    pt::Statement::ElseIf(
                        pt::Expr::Binary(
                            Box::new(pt::Expr::Id("x")),
                            pt::Operator::Equals,
                            Box::new(pt::Expr::Int(4)),
                        ),
                        vec![pt::Statement::Expr(pt::Expr::Int(4))],
                    ),
                    pt::Statement::Else(vec![pt::Statement::Expr(pt::Expr::Int(5))]),
                ],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Conditional(
                    ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Id(Identifier::from("x"))),
                        BinaryOperator::Equals,
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                    ),
                    vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                        1,
                    )))],
                    vec![ast::Statement::Conditional(
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Id(Identifier::from("x"))),
                            BinaryOperator::Equals,
                            Box::new(ast::Expr::Constant(Value::I64(2))),
                        ),
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                            2,
                        )))],
                        vec![ast::Statement::Conditional(
                            ast::Expr::BinaryOp(
                                Box::new(ast::Expr::Id(Identifier::from("x"))),
                                BinaryOperator::Equals,
                                Box::new(ast::Expr::Constant(Value::I64(3))),
                            ),
                            vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                                3,
                            )))],
                            vec![ast::Statement::Conditional(
                                ast::Expr::BinaryOp(
                                    Box::new(ast::Expr::Id(Identifier::from("x"))),
                                    BinaryOperator::Equals,
                                    Box::new(ast::Expr::Constant(Value::I64(4))),
                                ),
                                vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                                    4,
                                )))],
                                vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                                    5,
                                )))],
                            )],
                        )],
                    )],
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_else_single_line() {
        // No newline required between } and else
        let tc = ParserTestCase {
            input_str: r"if true { 1 } else { 2 }",
            expected_tokens: vec![
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::Int(1),
                Token::CloseCurly,
                Token::Else,
                Token::OpenCurly,
                Token::Int(2),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![
                    pt::Statement::If(
                        pt::Expr::Bool(true),
                        vec![pt::Statement::Expr(pt::Expr::Int(1))],
                    ),
                    pt::Statement::Else(vec![pt::Statement::Expr(pt::Expr::Int(2))]),
                ],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Conditional(
                    ast::Expr::Constant(Value::Bool(true)),
                    vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                        1,
                    )))],
                    vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                        2,
                    )))],
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_else_if_else_single_line() {
        let tc = ParserTestCase {
            input_str: r"if true { 1 } else if false { 2 } else { 3 }",
            expected_tokens: vec![
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::Int(1),
                Token::CloseCurly,
                Token::Else,
                Token::If,
                Token::Bool(false),
                Token::OpenCurly,
                Token::Int(2),
                Token::CloseCurly,
                Token::Else,
                Token::OpenCurly,
                Token::Int(3),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![
                    pt::Statement::If(
                        pt::Expr::Bool(true),
                        vec![pt::Statement::Expr(pt::Expr::Int(1))],
                    ),
                    pt::Statement::ElseIf(
                        pt::Expr::Bool(false),
                        vec![pt::Statement::Expr(pt::Expr::Int(2))],
                    ),
                    pt::Statement::Else(vec![pt::Statement::Expr(pt::Expr::Int(3))]),
                ],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Conditional(
                    ast::Expr::Constant(Value::Bool(true)),
                    vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                        1,
                    )))],
                    vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(false)),
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                            2,
                        )))],
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(
                            3,
                        )))],
                    )],
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_pair() {
        let tc = ParserTestCase {
            input_str: r"(1, 2)",
            expected_tokens: vec![
                Token::OpenParen,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Tuple(vec![
                    pt::Expr::Int(1),
                    pt::Expr::Int(2),
                ]))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                    ast::Expr::Constant(Value::I64(1)),
                    ast::Expr::Constant(Value::I64(2)),
                ]))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_single_trailing_comma() {
        let tc = ParserTestCase {
            input_str: r"(42,)",
            expected_tokens: vec![
                Token::OpenParen,
                Token::Int(42),
                Token::Comma,
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Tuple(vec![pt::Expr::Int(
                    42,
                )]))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                    ast::Expr::Constant(Value::I64(42)),
                ]))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_assign() {
        let tc = ParserTestCase {
            input_str: r"x = (1 + 2, read_int(), true)",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::Equals,
                Token::OpenParen,
                Token::Int(1),
                Token::Plus,
                Token::Int(2),
                Token::Comma,
                Token::Identifier("read_int"),
                Token::OpenParen,
                Token::CloseParen,
                Token::Comma,
                Token::Bool(true),
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Assign(
                    "x",
                    pt::Expr::Tuple(vec![
                        pt::Expr::Binary(
                            Box::new(pt::Expr::Int(1)),
                            pt::Operator::Plus,
                            Box::new(pt::Expr::Int(2)),
                        ),
                        pt::Expr::Call("read_int", vec![]),
                        pt::Expr::Bool(true),
                    ]),
                )],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Assign(
                    AssignDest::Id(Identifier::from("x")),
                    ast::Expr::Tuple(vec![
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Constant(Value::I64(1))),
                            BinaryOperator::Add,
                            Box::new(ast::Expr::Constant(Value::I64(2))),
                        ),
                        ast::Expr::Call(Identifier::from("read_int"), vec![]),
                        ast::Expr::Constant(Value::Bool(true)),
                    ]),
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_nested() {
        let tc = ParserTestCase {
            input_str: r"((1, 2), (3, 4))",
            expected_tokens: vec![
                Token::OpenParen,
                Token::OpenParen,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::CloseParen,
                Token::Comma,
                Token::OpenParen,
                Token::Int(3),
                Token::Comma,
                Token::Int(4),
                Token::CloseParen,
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Tuple(vec![
                    pt::Expr::Tuple(vec![pt::Expr::Int(1), pt::Expr::Int(2)]),
                    pt::Expr::Tuple(vec![pt::Expr::Int(3), pt::Expr::Int(4)]),
                ]))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                    ast::Expr::Tuple(vec![
                        ast::Expr::Constant(Value::I64(1)),
                        ast::Expr::Constant(Value::I64(2)),
                    ]),
                    ast::Expr::Tuple(vec![
                        ast::Expr::Constant(Value::I64(3)),
                        ast::Expr::Constant(Value::I64(4)),
                    ]),
                ]))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_trailing_comma_multi() {
        let tc = ParserTestCase {
            input_str: r"(1, 2,)",
            expected_tokens: vec![
                Token::OpenParen,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::Comma,
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Tuple(vec![
                    pt::Expr::Int(1),
                    pt::Expr::Int(2),
                ]))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                    ast::Expr::Constant(Value::I64(1)),
                    ast::Expr::Constant(Value::I64(2)),
                ]))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_in_call() {
        // print((1, 2)) is one tuple argument, not two int arguments
        let tc = ParserTestCase {
            input_str: r"print((1, 2))",
            expected_tokens: vec![
                Token::Identifier("print"),
                Token::OpenParen,
                Token::OpenParen,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::CloseParen,
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Call(
                    "print",
                    vec![pt::Expr::Tuple(vec![pt::Expr::Int(1), pt::Expr::Int(2)])],
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Call(
                    Identifier::from("print"),
                    vec![ast::Expr::Tuple(vec![
                        ast::Expr::Constant(Value::I64(1)),
                        ast::Expr::Constant(Value::I64(2)),
                    ])],
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    // ── Subscript expression tests ──────────────────────────────────────────

    #[test]
    fn test_subscript_simple() {
        let tc = ParserTestCase {
            input_str: r"x[0]",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                    Box::new(pt::Expr::Id("x")),
                    0,
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                    Box::new(ast::Expr::Id(Identifier::from("x"))),
                    0,
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_nonzero_index() {
        let tc = ParserTestCase {
            input_str: r"myvar[2]",
            expected_tokens: vec![
                Token::Identifier("myvar"),
                Token::OpenBracket,
                Token::Int(2),
                Token::CloseBracket,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                    Box::new(pt::Expr::Id("myvar")),
                    2,
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                    Box::new(ast::Expr::Id(Identifier::from("myvar"))),
                    2,
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_negative_index() {
        let tc = ParserTestCase {
            input_str: r"x[-1]",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(-1),
                Token::CloseBracket,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                    Box::new(pt::Expr::Id("x")),
                    -1,
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                    Box::new(ast::Expr::Id(Identifier::from("x"))),
                    -1,
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_chained() {
        // x[0][1] should be left-associative: Subscript(Subscript(x, 0), 1)
        let tc = ParserTestCase {
            input_str: r"x[0][1]",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::OpenBracket,
                Token::Int(1),
                Token::CloseBracket,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                    Box::new(pt::Expr::Subscript(Box::new(pt::Expr::Id("x")), 0)),
                    1,
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                    Box::new(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(Identifier::from("x"))),
                        0,
                    )),
                    1,
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_on_call() {
        // foo()[0] — subscript on a function call result
        let tc = ParserTestCase {
            input_str: r"foo()[0]",
            expected_tokens: vec![
                Token::Identifier("foo"),
                Token::OpenParen,
                Token::CloseParen,
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                    Box::new(pt::Expr::Call("foo", vec![])),
                    0,
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                    Box::new(ast::Expr::Call(Identifier::from("foo"), vec![])),
                    0,
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_in_binop() {
        // x[0] + 1 — subscript has higher precedence than binary ops
        let tc = ParserTestCase {
            input_str: r"x[0] + 1",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::Plus,
                Token::Int(1),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                    Box::new(pt::Expr::Subscript(Box::new(pt::Expr::Id("x")), 0)),
                    pt::Operator::Plus,
                    Box::new(pt::Expr::Int(1)),
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                    Box::new(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(Identifier::from("x"))),
                        0,
                    )),
                    BinaryOperator::Add,
                    Box::new(ast::Expr::Constant(Value::I64(1))),
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_on_parens_tuple() {
        // ((1, 2))[0] — subscript on a parenthesized tuple
        let tc = ParserTestCase {
            input_str: r"((1, 2))[0]",
            expected_tokens: vec![
                Token::OpenParen,
                Token::OpenParen,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::CloseParen,
                Token::CloseParen,
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                    Box::new(pt::Expr::Parens(Box::new(pt::Expr::Tuple(vec![
                        pt::Expr::Int(1),
                        pt::Expr::Int(2),
                    ])))),
                    0,
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                    Box::new(ast::Expr::Tuple(vec![
                        ast::Expr::Constant(Value::I64(1)),
                        ast::Expr::Constant(Value::I64(2)),
                    ])),
                    0,
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_in_ternary_condition() {
        // x[0] ? 1 : 2 — subscript in a ternary condition
        let tc = ParserTestCase {
            input_str: r"x[0] ? 1 : 2",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::QuestionMark,
                Token::Int(1),
                Token::Colon,
                Token::Int(2),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::Expr(pt::Expr::Ternary(
                    Box::new(pt::Expr::Subscript(Box::new(pt::Expr::Id("x")), 0)),
                    Box::new(pt::Expr::Int(1)),
                    Box::new(pt::Expr::Int(2)),
                ))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Expr(ast::Expr::Ternary(
                    Box::new(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(Identifier::from("x"))),
                        0,
                    )),
                    Box::new(ast::Expr::Constant(Value::I64(1))),
                    Box::new(ast::Expr::Constant(Value::I64(2))),
                ))],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    // ── SubscriptAssign statement tests ─────────────────────────────────────

    #[test]
    fn test_subscript_assign_simple() {
        let tc = ParserTestCase {
            input_str: r"x[0] = 1",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::Equals,
                Token::Int(1),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::SubscriptAssign("x", 0, pt::Expr::Int(1))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Assign(
                    AssignDest::Subscript(Identifier::from("x"), 0),
                    ast::Expr::Constant(Value::I64(1)),
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_nonzero_index() {
        let tc = ParserTestCase {
            input_str: r"tup[3] = 42",
            expected_tokens: vec![
                Token::Identifier("tup"),
                Token::OpenBracket,
                Token::Int(3),
                Token::CloseBracket,
                Token::Equals,
                Token::Int(42),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::SubscriptAssign("tup", 3, pt::Expr::Int(42))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Assign(
                    AssignDest::Subscript(Identifier::from("tup"), 3),
                    ast::Expr::Constant(Value::I64(42)),
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_complex_expr() {
        // x[1] = y + 1
        let tc = ParserTestCase {
            input_str: r"x[1] = y + 1",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(1),
                Token::CloseBracket,
                Token::Equals,
                Token::Identifier("y"),
                Token::Plus,
                Token::Int(1),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::SubscriptAssign(
                    "x",
                    1,
                    pt::Expr::Binary(
                        Box::new(pt::Expr::Id("y")),
                        pt::Operator::Plus,
                        Box::new(pt::Expr::Int(1)),
                    ),
                )],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Assign(
                    AssignDest::Subscript(Identifier::from("x"), 1),
                    ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Id(Identifier::from("y"))),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                    ),
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_tuple_value() {
        // x[0] = (1, 2) — assign a tuple to a subscript
        let tc = ParserTestCase {
            input_str: r"x[0] = (1, 2)",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::Equals,
                Token::OpenParen,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::CloseParen,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::SubscriptAssign(
                    "x",
                    0,
                    pt::Expr::Tuple(vec![pt::Expr::Int(1), pt::Expr::Int(2)]),
                )],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Assign(
                    AssignDest::Subscript(Identifier::from("x"), 0),
                    ast::Expr::Tuple(vec![
                        ast::Expr::Constant(Value::I64(1)),
                        ast::Expr::Constant(Value::I64(2)),
                    ]),
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_negative_index() {
        let tc = ParserTestCase {
            input_str: r"x[-1] = 99",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(-1),
                Token::CloseBracket,
                Token::Equals,
                Token::Int(99),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::SubscriptAssign("x", -1, pt::Expr::Int(99))],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Assign(
                    AssignDest::Subscript(Identifier::from("x"), -1),
                    ast::Expr::Constant(Value::I64(99)),
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_in_if_body() {
        let tc = ParserTestCase {
            input_str: r"if true { x[0] = 1 }",
            expected_tokens: vec![
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::Equals,
                Token::Int(1),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::If(
                    pt::Expr::Bool(true),
                    vec![pt::Statement::SubscriptAssign("x", 0, pt::Expr::Int(1))],
                )],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Conditional(
                    ast::Expr::Constant(Value::Bool(true)),
                    vec![ast::Statement::Assign(
                        AssignDest::Subscript(Identifier::from("x"), 0),
                        ast::Expr::Constant(Value::I64(1)),
                    )],
                    vec![],
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_with_subscript_expr_rhs() {
        // x[0] = y[1] — subscript on both sides
        let tc = ParserTestCase {
            input_str: r"x[0] = y[1]",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::Equals,
                Token::Identifier("y"),
                Token::OpenBracket,
                Token::Int(1),
                Token::CloseBracket,
            ],
            expected_parse_tree: pt::Module {
                statements: vec![pt::Statement::SubscriptAssign(
                    "x",
                    0,
                    pt::Expr::Subscript(Box::new(pt::Expr::Id("y")), 1),
                )],
            },
            expected_ast: ast::Module {
                body: vec![ast::Statement::Assign(
                    AssignDest::Subscript(Identifier::from("x"), 0),
                    ast::Expr::Subscript(Box::new(ast::Expr::Id(Identifier::from("y"))), 1),
                )],
                types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_multiline() {
        // Subscript assign alongside other statements
        let tc = ParserTestCase {
            input_str: r"x = (1, 2)
x[0] = 42",
            expected_tokens: vec![
                Token::Identifier("x"),
                Token::Equals,
                Token::OpenParen,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::CloseParen,
                Token::Newline,
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::Equals,
                Token::Int(42),
            ],
            expected_parse_tree: pt::Module {
                statements: vec![
                    pt::Statement::Assign(
                        "x",
                        pt::Expr::Tuple(vec![pt::Expr::Int(1), pt::Expr::Int(2)]),
                    ),
                    pt::Statement::SubscriptAssign("x", 0, pt::Expr::Int(42)),
                ],
            },
            expected_ast: ast::Module {
                body: vec![
                    ast::Statement::Assign(
                        AssignDest::Id(Identifier::from("x")),
                        ast::Expr::Tuple(vec![
                            ast::Expr::Constant(Value::I64(1)),
                            ast::Expr::Constant(Value::I64(2)),
                        ]),
                    ),
                    ast::Statement::Assign(
                        AssignDest::Subscript(Identifier::from("x"), 0),
                        ast::Expr::Constant(Value::I64(42)),
                    ),
                ],
                types: HashMap::new(),
            },
        };
        tc.run();
    }
}
