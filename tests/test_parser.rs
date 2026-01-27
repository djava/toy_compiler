use cs4999_compiler::{ast, parser::*};
use std::sync::Arc;

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
fn test_parser_simple_constant() {
    let tc = ParserTestCase {
        input_str: r"1",
        expected_tokens: vec![Token::Int(1)],
        expected_parse_tree: pt::Module {
            statements: vec![pt::Statement::Expr(pt::Expr::Int(1))],
        },
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::Constant(
            ast::Value::I64(1),
        ))]),
    };

    tc.run();
}

#[test]
fn test_parser_simple_neg_constant() {
    let tc = ParserTestCase {
        input_str: r"-100",
        expected_tokens: vec![Token::Int(-100)],
        expected_parse_tree: pt::Module {
            statements: vec![pt::Statement::Expr(pt::Expr::Int(-100))],
        },
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::Constant(
            ast::Value::I64(-100),
        ))]),
    };

    tc.run();
}

#[test]
fn test_parser_simple_binop() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::BinaryOp(
            Box::new(ast::Expr::Constant(ast::Value::I64(1))),
            ast::BinaryOperator::Add,
            Box::new(ast::Expr::Constant(ast::Value::I64(2))),
        ))]),
    };

    tc.run();
}

#[test]
fn test_parser_parens() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::BinaryOp(
            Box::new(ast::Expr::Constant(ast::Value::I64(1))),
            ast::BinaryOperator::Add,
            Box::new(ast::Expr::BinaryOp(
                Box::new(ast::Expr::Constant(ast::Value::I64(2))),
                ast::BinaryOperator::Add,
                Box::new(ast::Expr::Constant(ast::Value::I64(3))),
            )),
        ))]),
    };

    tc.run();
}

#[test]
fn test_parser_simple_unaryop() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::UnaryOp(
            ast::UnaryOperator::Minus,
            Box::new(ast::Expr::Constant(ast::Value::I64(1))),
        ))]),
    };

    tc.run();
}

#[test]
fn test_parser_simple_assign() {
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
            statements: (vec![pt::Statement::Assign(
                "x",
                pt::Expr::Binary(
                    Box::new(pt::Expr::Int(1000)),
                    pt::Operator::Plus,
                    Box::new(pt::Expr::Int(-21912983)),
                ),
            )]),
        },
        expected_ast: ast::Module::Body(vec![ast::Statement::Assign(
            ast::Identifier::Named(Arc::from("x")),
            ast::Expr::BinaryOp(
                Box::new(ast::Expr::Constant(ast::Value::I64(1000))),
                ast::BinaryOperator::Add,
                Box::new(ast::Expr::Constant(ast::Value::I64(-21912983))),
            ),
        )]),
    };
    tc.run();
}

#[test]
fn test_parser_call_simple_arg() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::Call(
            ast::Identifier::Named(Arc::from("print")),
            vec![ast::Expr::Id(ast::Identifier::Named(Arc::from("x")))],
        ))]),
    };
    tc.run();
}

#[test]
fn test_parser_call_no_arg() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::Call(
            ast::Identifier::Named(Arc::from("oogabooga")),
            vec![],
        ))]),
    };
    tc.run();
}

#[test]
fn test_parser_call_complex_arg() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::Call(
            ast::Identifier::Named(Arc::from("print")),
            vec![ast::Expr::BinaryOp(
                Box::new(ast::Expr::BinaryOp(
                    Box::new(ast::Expr::Constant(ast::Value::I64(1))),
                    ast::BinaryOperator::Add,
                    Box::new(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(ast::Value::I64(2))),
                        ast::BinaryOperator::Add,
                        Box::new(ast::Expr::Constant(ast::Value::I64(4))),
                    )),
                )),
                ast::BinaryOperator::Subtract,
                Box::new(ast::Expr::Call(
                    ast::Identifier::Named(Arc::from("read_int")),
                    vec![],
                )),
            )],
        ))]),
    };
    tc.run();
}

#[test]
fn test_parser_call_multi_arg() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::Call(
            ast::Identifier::Named(Arc::from("print")),
            vec![
                ast::Expr::Id(ast::Identifier::Named(Arc::from("x"))),
                ast::Expr::Id(ast::Identifier::Named(Arc::from("y"))),
                ast::Expr::Constant(ast::Value::I64(1)),
                ast::Expr::Constant(ast::Value::I64(3)),
                ast::Expr::Constant(ast::Value::I64(1000)),
            ],
        ))]),
    };
    tc.run();
}

#[test]
fn test_parser_assign_to_call() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Assign(
            ast::Identifier::Named(Arc::from("x")),
            ast::Expr::Call(
                ast::Identifier::Named(Arc::from("Fffoo")),
                vec![ast::Expr::Constant(ast::Value::I64(1))],
            ),
        )]),
    };

    tc.run();
}

#[test]
fn test_parser_multiline() {
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
        expected_ast: ast::Module::Body(vec![
            ast::Statement::Assign(
                ast::Identifier::Named(Arc::from("x")),
                ast::Expr::Constant(ast::Value::I64(100)),
            ),
            ast::Statement::Expr(ast::Expr::Call(
                ast::Identifier::Named(Arc::from("print")),
                vec![ast::Expr::Constant(ast::Value::I64(1000))],
            )),
            ast::Statement::Assign(
                ast::Identifier::Named(Arc::from("whatevn")),
                ast::Expr::Id(ast::Identifier::Named(Arc::from("x"))),
            ),
            ast::Statement::Expr(ast::Expr::BinaryOp(
                Box::new(ast::Expr::Constant(ast::Value::I64(101010))),
                ast::BinaryOperator::Add,
                Box::new(ast::Expr::Constant(ast::Value::I64(1001010))),
            )),
        ]),
    };

    tc.run()
}

#[test]
fn test_parser_space_delimited_unary() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::BinaryOp(
            Box::new(ast::Expr::Constant(ast::Value::I64(2))),
            ast::BinaryOperator::Add,
            Box::new(ast::Expr::UnaryOp(
                ast::UnaryOperator::Minus,
                Box::new(ast::Expr::Constant(ast::Value::I64(5))),
            )),
        ))]),
    };
    tc.run();
}

#[test]
fn test_parser_bool_op_simple() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::BinaryOp(
            Box::new(ast::Expr::Constant(ast::Value::Bool(false))),
            ast::BinaryOperator::And,
            Box::new(ast::Expr::Constant(ast::Value::Bool(true))),
        ))]),
    };
    tc.run();
}

#[test]
fn test_parser_bool_op_complex() {
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
        expected_ast: ast::Module::Body(vec![
            ast::Statement::Expr(ast::Expr::Call(
            ast::Identifier::Named(Arc::from("truefoofalse")),
            vec![ast::Expr::BinaryOp(
                Box::new(ast::Expr::BinaryOp(
                    Box::new(ast::Expr::UnaryOp(
                        ast::UnaryOperator::Not,
                        Box::new(ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Constant(ast::Value::Bool(false))),
                            ast::BinaryOperator::And,
                            Box::new(ast::Expr::Constant(ast::Value::Bool(true))),
                        )),
                    )),
                    ast::BinaryOperator::Or,
                    Box::new(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(ast::Value::Bool(false))),
                        ast::BinaryOperator::Equals,
                        Box::new(ast::Expr::Constant(ast::Value::Bool(true))),
                    )),
                )),
                ast::BinaryOperator::GreaterEquals,
                Box::new(ast::Expr::BinaryOp(
                    Box::new(ast::Expr::Constant(ast::Value::I64(1))),
                    ast::BinaryOperator::Subtract,
                    Box::new(ast::Expr::Constant(ast::Value::Bool(false))),
                )),
            )],
        ))]),
    };
    tc.run();
}

#[test]
fn test_parser_ternary_simple() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::Ternary(
            Box::new(ast::Expr::Constant(ast::Value::Bool(true))),
            Box::new(ast::Expr::Constant(ast::Value::I64(1))),
            Box::new(ast::Expr::Constant(ast::Value::I64(2))),
        ))]),
    };
    tc.run();
}

#[test]
fn test_parser_ternary_nested() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::Ternary(
            Box::new(ast::Expr::Constant(ast::Value::Bool(true))),
            Box::new(ast::Expr::Constant(ast::Value::I64(1))),
            Box::new(ast::Expr::Ternary(
                Box::new(ast::Expr::Constant(ast::Value::Bool(false))),
                Box::new(ast::Expr::Constant(ast::Value::I64(2))),
                Box::new(ast::Expr::Constant(ast::Value::I64(3))),
            )),
        ))]),
    };
    tc.run();
}

#[test]
fn test_parser_ternary_with_binop() {
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Expr(ast::Expr::Ternary(
            Box::new(ast::Expr::BinaryOp(
                Box::new(ast::Expr::Constant(ast::Value::I64(1))),
                ast::BinaryOperator::Add,
                Box::new(ast::Expr::Constant(ast::Value::I64(2))),
            )),
            Box::new(ast::Expr::Constant(ast::Value::I64(3))),
            Box::new(ast::Expr::Constant(ast::Value::I64(4))),
        ))]),
    };
    tc.run();
}

#[test]
fn test_parser_if_simple() {
    let tc = ParserTestCase {
        input_str: r"if true { 1 }",
        expected_tokens: vec![
            Token::If,
            Token::Bool(true),
            Token::OpenBracket,
            Token::Int(1),
            Token::CloseBracket,
        ],
        expected_parse_tree: pt::Module {
            statements: vec![pt::Statement::If(
                pt::Expr::Bool(true),
                vec![pt::Statement::Expr(pt::Expr::Int(1))],
            )],
        },
        expected_ast: ast::Module::Body(vec![ast::Statement::Conditional(
            ast::Expr::Constant(ast::Value::Bool(true)),
            vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(1)))],
            vec![],
        )]),
    };
    tc.run();
}

#[test]
fn test_parser_if_else() {
    let tc = ParserTestCase {
        input_str: r"if true { 1 }
else { 2 }",
        expected_tokens: vec![
            Token::If,
            Token::Bool(true),
            Token::OpenBracket,
            Token::Int(1),
            Token::CloseBracket,
            Token::Newline,
            Token::Else,
            Token::OpenBracket,
            Token::Int(2),
            Token::CloseBracket,
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Conditional(
            ast::Expr::Constant(ast::Value::Bool(true)),
            vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(1)))],
            vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(2)))],
        )]),
    };
    tc.run();
}

#[test]
fn test_parser_if_else_if_else() {
    let tc = ParserTestCase {
        input_str: r"if true { 1 }
else if false { 2 }
else { 3 }",
        expected_tokens: vec![
            Token::If,
            Token::Bool(true),
            Token::OpenBracket,
            Token::Int(1),
            Token::CloseBracket,
            Token::Newline,
            Token::Else,
            Token::If,
            Token::Bool(false),
            Token::OpenBracket,
            Token::Int(2),
            Token::CloseBracket,
            Token::Newline,
            Token::Else,
            Token::OpenBracket,
            Token::Int(3),
            Token::CloseBracket,
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Conditional(
            ast::Expr::Constant(ast::Value::Bool(true)),
            vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(1)))],
            vec![ast::Statement::Conditional(
                ast::Expr::Constant(ast::Value::Bool(false)),
                vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(2)))],
                vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(3)))],
            )],
        )]),
    };
    tc.run();
}

#[test]
fn test_parser_if_with_complex_condition() {
    let tc = ParserTestCase {
        input_str: r"if x == 1 { print(x) }",
        expected_tokens: vec![
            Token::If,
            Token::Identifier("x"),
            Token::DoubleEquals,
            Token::Int(1),
            Token::OpenBracket,
            Token::Identifier("print"),
            Token::OpenParen,
            Token::Identifier("x"),
            Token::CloseParen,
            Token::CloseBracket,
        ],
        expected_parse_tree: pt::Module {
            statements: vec![pt::Statement::If(
                pt::Expr::Binary(
                    Box::new(pt::Expr::Id("x")),
                    pt::Operator::Equals,
                    Box::new(pt::Expr::Int(1)),
                ),
                vec![pt::Statement::Expr(pt::Expr::Call("print", vec![pt::Expr::Id("x")]))],
            )],
        },
        expected_ast: ast::Module::Body(vec![ast::Statement::Conditional(
            ast::Expr::BinaryOp(
                Box::new(ast::Expr::Id(ast::Identifier::Named(Arc::from("x")))),
                ast::BinaryOperator::Equals,
                Box::new(ast::Expr::Constant(ast::Value::I64(1))),
            ),
            vec![ast::Statement::Expr(ast::Expr::Call(
                ast::Identifier::Named(Arc::from("print")),
                vec![ast::Expr::Id(ast::Identifier::Named(Arc::from("x")))],
            ))],
            vec![],
        )]),
    };
    tc.run();
}

#[test]
fn test_parser_if_multiline_body() {
    let tc = ParserTestCase {
        input_str: r"if true { x = 1


    print(x) }",
        expected_tokens: vec![
            Token::If,
            Token::Bool(true),
            Token::OpenBracket,
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
            Token::CloseBracket,
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Conditional(
            ast::Expr::Constant(ast::Value::Bool(true)),
            vec![
                ast::Statement::Assign(
                    ast::Identifier::Named(Arc::from("x")),
                    ast::Expr::Constant(ast::Value::I64(1)),
                ),
                ast::Statement::Expr(ast::Expr::Call(
                    ast::Identifier::Named(Arc::from("print")),
                    vec![ast::Expr::Id(ast::Identifier::Named(Arc::from("x")))],
                )),
            ],
            vec![],
        )]),
    };
    tc.run();
}

#[test]
fn test_parser_if_empty_body() {
    let tc = ParserTestCase {
        input_str: r"if true { }",
        expected_tokens: vec![
            Token::If,
            Token::Bool(true),
            Token::OpenBracket,
            Token::CloseBracket,
        ],
        expected_parse_tree: pt::Module {
            statements: vec![pt::Statement::If(pt::Expr::Bool(true), vec![])],
        },
        expected_ast: ast::Module::Body(vec![ast::Statement::Conditional(
            ast::Expr::Constant(ast::Value::Bool(true)),
            vec![],
            vec![],
        )]),
    };
    tc.run();
}

#[test]
fn test_parser_if_chained_else_ifs() {
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
            Token::OpenBracket,
            Token::Int(1),
            Token::CloseBracket,
            Token::Newline,
            Token::Else,
            Token::If,
            Token::Identifier("x"),
            Token::DoubleEquals,
            Token::Int(2),
            Token::OpenBracket,
            Token::Int(2),
            Token::CloseBracket,
            Token::Newline,
            Token::Else,
            Token::If,
            Token::Identifier("x"),
            Token::DoubleEquals,
            Token::Int(3),
            Token::OpenBracket,
            Token::Int(3),
            Token::CloseBracket,
            Token::Newline,
            Token::Else,
            Token::If,
            Token::Identifier("x"),
            Token::DoubleEquals,
            Token::Int(4),
            Token::OpenBracket,
            Token::Int(4),
            Token::CloseBracket,
            Token::Newline,
            Token::Else,
            Token::OpenBracket,
            Token::Int(5),
            Token::CloseBracket,
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Conditional(
            ast::Expr::BinaryOp(
                Box::new(ast::Expr::Id(ast::Identifier::Named(Arc::from("x")))),
                ast::BinaryOperator::Equals,
                Box::new(ast::Expr::Constant(ast::Value::I64(1))),
            ),
            vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(1)))],
            vec![ast::Statement::Conditional(
                ast::Expr::BinaryOp(
                    Box::new(ast::Expr::Id(ast::Identifier::Named(Arc::from("x")))),
                    ast::BinaryOperator::Equals,
                    Box::new(ast::Expr::Constant(ast::Value::I64(2))),
                ),
                vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(2)))],
                vec![ast::Statement::Conditional(
                    ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Id(ast::Identifier::Named(Arc::from("x")))),
                        ast::BinaryOperator::Equals,
                        Box::new(ast::Expr::Constant(ast::Value::I64(3))),
                    ),
                    vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(3)))],
                    vec![ast::Statement::Conditional(
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Id(ast::Identifier::Named(Arc::from("x")))),
                            ast::BinaryOperator::Equals,
                            Box::new(ast::Expr::Constant(ast::Value::I64(4))),
                        ),
                        vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(4)))],
                        vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(5)))],
                    )],
                )],
            )],
        )]),
    };
    tc.run();
}

#[test]
fn test_parser_if_else_single_line() {
    // No newline required between } and else
    let tc = ParserTestCase {
        input_str: r"if true { 1 } else { 2 }",
        expected_tokens: vec![
            Token::If,
            Token::Bool(true),
            Token::OpenBracket,
            Token::Int(1),
            Token::CloseBracket,
            Token::Else,
            Token::OpenBracket,
            Token::Int(2),
            Token::CloseBracket,
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Conditional(
            ast::Expr::Constant(ast::Value::Bool(true)),
            vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(1)))],
            vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(2)))],
        )]),
    };
    tc.run();
}

#[test]
fn test_parser_if_else_if_else_single_line() {
    let tc = ParserTestCase {
        input_str: r"if true { 1 } else if false { 2 } else { 3 }",
        expected_tokens: vec![
            Token::If,
            Token::Bool(true),
            Token::OpenBracket,
            Token::Int(1),
            Token::CloseBracket,
            Token::Else,
            Token::If,
            Token::Bool(false),
            Token::OpenBracket,
            Token::Int(2),
            Token::CloseBracket,
            Token::Else,
            Token::OpenBracket,
            Token::Int(3),
            Token::CloseBracket,
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
        expected_ast: ast::Module::Body(vec![ast::Statement::Conditional(
            ast::Expr::Constant(ast::Value::Bool(true)),
            vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(1)))],
            vec![ast::Statement::Conditional(
                ast::Expr::Constant(ast::Value::Bool(false)),
                vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(2)))],
                vec![ast::Statement::Expr(ast::Expr::Constant(ast::Value::I64(3)))],
            )],
        )]),
    };
    tc.run();
}
