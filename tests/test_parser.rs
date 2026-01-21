use cs4999_compiler::{ast, parser::*};

use parse_tree as pt;

pub struct ParserTestCase<'a> {
    pub input_str: &'a str,
    pub expected_tokens: Vec<Token<'a>>,
    pub expected_parse_tree: parse_tree::Module<'a>,
    pub expected_ast: ast::Module<'a>,
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
            ast::Identifier::Named("x"),
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
            ast::Identifier::Named("print"),
            vec![ast::Expr::Id(ast::Identifier::Named("x"))],
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
            ast::Identifier::Named("oogabooga"),
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
            ast::Identifier::Named("print"),
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
                Box::new(ast::Expr::Call(ast::Identifier::Named("read_int"), vec![])),
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
            ast::Identifier::Named("print"),
            vec![
                ast::Expr::Id(ast::Identifier::Named("x")),
                ast::Expr::Id(ast::Identifier::Named("y")),
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
            ast::Identifier::Named("x"),
            ast::Expr::Call(
                ast::Identifier::Named("Fffoo"),
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
            Token::Newline
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
                ast::Identifier::Named("x"),
                ast::Expr::Constant(ast::Value::I64(100)),
            ),
            ast::Statement::Expr(ast::Expr::Call(
                ast::Identifier::Named("print"),
                vec![ast::Expr::Constant(ast::Value::I64(1000))],
            )),
            ast::Statement::Assign(
                ast::Identifier::Named("whatevn"),
                ast::Expr::Id(ast::Identifier::Named("x")),
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
