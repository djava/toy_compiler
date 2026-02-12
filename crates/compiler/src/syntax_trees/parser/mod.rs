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

pub fn parse<'a>(input: &'a str) -> Result<ast::Program, ParserError<'a>> {
    let tokens = tokenizer::tokenize(input)?;
    let parse_tree = parse_tree::parse_tokens(&tokens)?;
    let ast = to_ast::to_ast(parse_tree);

    Ok(ast)
}

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;
    use std::collections::HashMap;

    use crate::utils::t_id;
    use test_support::compiler::{
        constants::LABEL_MAIN,
        syntax_trees::{ast, parser::*, shared::*},
    };

    use parse_tree as pt;

    pub struct ParserTestCase<'a> {
        pub input_str: &'a str,
        pub expected_tokens: Vec<Token<'a>>,
        pub expected_parse_tree: parse_tree::Module<'a>,
        pub expected_ast: ast::Program,
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
            input_str: r"fn main() -> int { 1 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Int(1),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Int(1))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(1)))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_neg_constant() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { -100 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Int(-100),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Int(-100))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(-100)))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_binop() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { 1 + 2 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Int(1),
                Token::Plus,
                Token::Int(2),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                        Box::new(pt::Expr::Int(1)),
                        pt::Operator::Plus,
                        Box::new(pt::Expr::Int(2)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::Constant(Value::I64(2))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_parens() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { 1 + (2 + 3) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Int(1),
                Token::Plus,
                Token::OpenParen,
                Token::Int(2),
                Token::Plus,
                Token::Int(3),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                        Box::new(pt::Expr::Int(1)),
                        pt::Operator::Plus,
                        Box::new(pt::Expr::Parens(Box::new(pt::Expr::Binary(
                            Box::new(pt::Expr::Int(2)),
                            pt::Operator::Plus,
                            Box::new(pt::Expr::Int(3)),
                        )))),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
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
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_unaryop() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { -(1) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Minus,
                Token::OpenParen,
                Token::Int(1),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Unary(
                        pt::Operator::Minus,
                        Box::new(pt::Expr::Parens(Box::new(pt::Expr::Int(1)))),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::UnaryOp(
                        UnaryOperator::Minus,
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_assign() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x = 1000 + -21912983 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::Equals,
                Token::Int(1000),
                Token::Plus,
                Token::Int(-21912983),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Assign(
                        "x",
                        pt::Expr::Binary(
                            Box::new(pt::Expr::Int(1000)),
                            pt::Operator::Plus,
                            Box::new(pt::Expr::Int(-21912983)),
                        ),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Id(t_id!("x")),
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Constant(Value::I64(1000))),
                            BinaryOperator::Add,
                            Box::new(ast::Expr::Constant(Value::I64(-21912983))),
                        ),
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_simple_arg() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { print(x) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("print"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Call(
                        "print",
                        vec![pt::Expr::Id("x")],
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(t_id!("print"))),
                        vec![ast::Expr::Id(t_id!("x"))],
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_no_arg() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { oogabooga() }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("oogabooga"),
                Token::OpenParen,
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Call("oogabooga", vec![]))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(t_id!("oogabooga"))),
                        vec![],
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_complex_arg() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { print((1 + (2 + 4)) - read_int()) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
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
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(t_id!("print"))),
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
                            Box::new(ast::Expr::Call(Box::new(ast::Expr::Id(t_id!("read_int"))), vec![])),
                        )],
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_multi_arg() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { print(x, y, 1, 3, 1000) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
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
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(t_id!("print"))),
                        vec![
                            ast::Expr::Id(t_id!("x")),
                            ast::Expr::Id(t_id!("y")),
                            ast::Expr::Constant(Value::I64(1)),
                            ast::Expr::Constant(Value::I64(3)),
                            ast::Expr::Constant(Value::I64(1000)),
                        ],
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_assign_to_call() {
        let tc = ParserTestCase {
            input_str: "fn main() -> int { x = Fffoo(1) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::Equals,
                Token::Identifier("Fffoo"),
                Token::OpenParen,
                Token::Int(1),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Assign(
                        "x",
                        pt::Expr::Call("Fffoo", vec![pt::Expr::Int(1)]),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Id(t_id!("x")),
                        ast::Expr::Call(Box::new(ast::Expr::Id(t_id!("Fffoo"))), vec![ast::Expr::Constant(Value::I64(1))]),
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_multiline() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x = 100
print(1000)
whatevn = x
101010 + 1001010
}",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
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
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        ast::Statement::Assign(
                            AssignDest::Id(t_id!("x")),
                            ast::Expr::Constant(Value::I64(100)),
                        ),
                        ast::Statement::Expr(ast::Expr::Call(
                            Box::new(ast::Expr::Id(t_id!("print"))),
                            vec![ast::Expr::Constant(Value::I64(1000))],
                        )),
                        ast::Statement::Assign(
                            AssignDest::Id(t_id!("whatevn")),
                            ast::Expr::Id(t_id!("x")),
                        ),
                        ast::Statement::Expr(ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Constant(Value::I64(101010))),
                            BinaryOperator::Add,
                            Box::new(ast::Expr::Constant(Value::I64(1001010))),
                        )),
                    ],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };

        tc.run()
    }

    #[test]
    fn test_space_delimited_unary() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { 2 + - 5 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Int(2),
                Token::Plus,
                Token::Minus,
                Token::Int(5),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                        Box::new(pt::Expr::Int(2)),
                        pt::Operator::Plus,
                        Box::new(pt::Expr::Unary(
                            pt::Operator::Minus,
                            Box::new(pt::Expr::Int(5)),
                        )),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(Value::I64(2))),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::UnaryOp(
                            UnaryOperator::Minus,
                            Box::new(ast::Expr::Constant(Value::I64(5))),
                        )),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_bool_op_simple() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { false && true }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Bool(false),
                Token::And,
                Token::Bool(true),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                        Box::new(pt::Expr::Bool(false)),
                        pt::Operator::And,
                        Box::new(pt::Expr::Bool(true)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(Value::Bool(false))),
                        BinaryOperator::And,
                        Box::new(ast::Expr::Constant(Value::Bool(true))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_bool_op_complex() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { truefoofalse(!(false && true) || (false == true) >= (1 - false)) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
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
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(t_id!("truefoofalse"))),
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
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_ternary_simple() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { true ? 1 : 2 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Bool(true),
                Token::QuestionMark,
                Token::Int(1),
                Token::Colon,
                Token::Int(2),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Ternary(
                        Box::new(pt::Expr::Bool(true)),
                        Box::new(pt::Expr::Int(1)),
                        Box::new(pt::Expr::Int(2)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Ternary(
                        Box::new(ast::Expr::Constant(Value::Bool(true))),
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                        Box::new(ast::Expr::Constant(Value::I64(2))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_ternary_nested() {
        // Right-associative: a ? b : c ? d : e  =>  a ? b : (c ? d : e)
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { true ? 1 : false ? 2 : 3 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Bool(true),
                Token::QuestionMark,
                Token::Int(1),
                Token::Colon,
                Token::Bool(false),
                Token::QuestionMark,
                Token::Int(2),
                Token::Colon,
                Token::Int(3),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Ternary(
                        Box::new(pt::Expr::Bool(true)),
                        Box::new(pt::Expr::Int(1)),
                        Box::new(pt::Expr::Ternary(
                            Box::new(pt::Expr::Bool(false)),
                            Box::new(pt::Expr::Int(2)),
                            Box::new(pt::Expr::Int(3)),
                        )),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
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
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_ternary_with_binop() {
        // Ternary has lower precedence than binary ops
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { 1 + 2 ? 3 : 4 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Int(1),
                Token::Plus,
                Token::Int(2),
                Token::QuestionMark,
                Token::Int(3),
                Token::Colon,
                Token::Int(4),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Ternary(
                        Box::new(pt::Expr::Binary(
                            Box::new(pt::Expr::Int(1)),
                            pt::Operator::Plus,
                            Box::new(pt::Expr::Int(2)),
                        )),
                        Box::new(pt::Expr::Int(3)),
                        Box::new(pt::Expr::Int(4)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
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
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_simple() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if true { 1 } }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::Int(1),
                Token::CloseCurly,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::If(
                        pt::Expr::Bool(true),
                        vec![pt::Statement::Expr(pt::Expr::Int(1))],
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(1)))],
                        vec![],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_else() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int {
if true { 1 }
else { 2 }
}",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Newline,
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
                Token::Newline,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![
                        pt::Statement::If(
                            pt::Expr::Bool(true),
                            vec![pt::Statement::Expr(pt::Expr::Int(1))],
                        ),
                        pt::Statement::Else(vec![pt::Statement::Expr(pt::Expr::Int(2))]),
                    ],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(1)))],
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(2)))],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_else_if_else() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int {
if true { 1 }
else if false { 2 }
else { 3 }
}",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Newline,
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
                Token::Newline,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
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
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(1)))],
                        vec![ast::Statement::Conditional(
                            ast::Expr::Constant(Value::Bool(false)),
                            vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(2)))],
                            vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(3)))],
                        )],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_with_complex_condition() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if x == 1 { print(x) } }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
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
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Id(t_id!("x"))),
                            BinaryOperator::Equals,
                            Box::new(ast::Expr::Constant(Value::I64(1))),
                        ),
                        vec![ast::Statement::Expr(ast::Expr::Call(
                            Box::new(ast::Expr::Id(t_id!("print"))),
                            vec![ast::Expr::Id(t_id!("x"))],
                        ))],
                        vec![],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_multiline_body() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int {
if true { x = 1


    print(x) }
}",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Newline,
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
                Token::Newline,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::If(
                        pt::Expr::Bool(true),
                        vec![
                            pt::Statement::Assign("x", pt::Expr::Int(1)),
                            pt::Statement::Expr(pt::Expr::Call("print", vec![pt::Expr::Id("x")])),
                        ],
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![
                            ast::Statement::Assign(
                                AssignDest::Id(t_id!("x")),
                                ast::Expr::Constant(Value::I64(1)),
                            ),
                            ast::Statement::Expr(ast::Expr::Call(
                                Box::new(ast::Expr::Id(t_id!("print"))),
                                vec![ast::Expr::Id(t_id!("x"))],
                            )),
                        ],
                        vec![],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_empty_body() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if true { } }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::CloseCurly,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::If(pt::Expr::Bool(true), vec![])],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![],
                        vec![],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_chained_else_ifs() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int {
if x == 1 { 1 }
else if x == 2 { 2 }
else if x == 3 { 3 }
else if x == 4 { 4 }
else { 5 }
}",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Newline,
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
                Token::Newline,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
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
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Id(t_id!("x"))),
                            BinaryOperator::Equals,
                            Box::new(ast::Expr::Constant(Value::I64(1))),
                        ),
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(1)))],
                        vec![ast::Statement::Conditional(
                            ast::Expr::BinaryOp(
                                Box::new(ast::Expr::Id(t_id!("x"))),
                                BinaryOperator::Equals,
                                Box::new(ast::Expr::Constant(Value::I64(2))),
                            ),
                            vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(2)))],
                            vec![ast::Statement::Conditional(
                                ast::Expr::BinaryOp(
                                    Box::new(ast::Expr::Id(t_id!("x"))),
                                    BinaryOperator::Equals,
                                    Box::new(ast::Expr::Constant(Value::I64(3))),
                                ),
                                vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(3)))],
                                vec![ast::Statement::Conditional(
                                    ast::Expr::BinaryOp(
                                        Box::new(ast::Expr::Id(t_id!("x"))),
                                        BinaryOperator::Equals,
                                        Box::new(ast::Expr::Constant(Value::I64(4))),
                                    ),
                                    vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(4)))],
                                    vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(5)))],
                                )],
                            )],
                        )],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_else_single_line() {
        // No newline required between } and else
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if true { 1 } else { 2 } }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::If,
                Token::Bool(true),
                Token::OpenCurly,
                Token::Int(1),
                Token::CloseCurly,
                Token::Else,
                Token::OpenCurly,
                Token::Int(2),
                Token::CloseCurly,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![
                        pt::Statement::If(
                            pt::Expr::Bool(true),
                            vec![pt::Statement::Expr(pt::Expr::Int(1))],
                        ),
                        pt::Statement::Else(vec![pt::Statement::Expr(pt::Expr::Int(2))]),
                    ],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(1)))],
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(2)))],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_else_if_else_single_line() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if true { 1 } else if false { 2 } else { 3 } }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
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
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(1)))],
                        vec![ast::Statement::Conditional(
                            ast::Expr::Constant(Value::Bool(false)),
                            vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(2)))],
                            vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(3)))],
                        )],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_pair() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { (1, 2) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::OpenParen,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Tuple(vec![
                        pt::Expr::Int(1),
                        pt::Expr::Int(2),
                    ]))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                        ast::Expr::Constant(Value::I64(1)),
                        ast::Expr::Constant(Value::I64(2)),
                    ]))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_single_trailing_comma() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { (42,) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::OpenParen,
                Token::Int(42),
                Token::Comma,
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Tuple(vec![pt::Expr::Int(
                        42,
                    )]))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                        ast::Expr::Constant(Value::I64(42)),
                    ]))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_assign() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x = (1 + 2, read_int(), true) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
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
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Id(t_id!("x")),
                        ast::Expr::Tuple(vec![
                            ast::Expr::BinaryOp(
                                Box::new(ast::Expr::Constant(Value::I64(1))),
                                BinaryOperator::Add,
                                Box::new(ast::Expr::Constant(Value::I64(2))),
                            ),
                            ast::Expr::Call(Box::new(ast::Expr::Id(t_id!("read_int"))), vec![]),
                            ast::Expr::Constant(Value::Bool(true)),
                        ]),
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_nested() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { ((1, 2), (3, 4)) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Tuple(vec![
                        pt::Expr::Tuple(vec![pt::Expr::Int(1), pt::Expr::Int(2)]),
                        pt::Expr::Tuple(vec![pt::Expr::Int(3), pt::Expr::Int(4)]),
                    ]))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
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
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_trailing_comma_multi() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { (1, 2,) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::OpenParen,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::Comma,
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Tuple(vec![
                        pt::Expr::Int(1),
                        pt::Expr::Int(2),
                    ]))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                        ast::Expr::Constant(Value::I64(1)),
                        ast::Expr::Constant(Value::I64(2)),
                    ]))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_in_call() {
        // print((1, 2)) is one tuple argument, not two int arguments
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { print((1, 2)) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("print"),
                Token::OpenParen,
                Token::OpenParen,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::CloseParen,
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Call(
                        "print",
                        vec![pt::Expr::Tuple(vec![pt::Expr::Int(1), pt::Expr::Int(2)])],
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(t_id!("print"))),
                        vec![ast::Expr::Tuple(vec![
                            ast::Expr::Constant(Value::I64(1)),
                            ast::Expr::Constant(Value::I64(2)),
                        ])],
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    // ── Subscript expression tests ──────────────────────────────────────────

    #[test]
    fn test_subscript_simple() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[0] }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Id("x")),
                        0,
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(t_id!("x"))),
                        0,
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_nonzero_index() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { myvar[2] }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("myvar"),
                Token::OpenBracket,
                Token::Int(2),
                Token::CloseBracket,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Id("myvar")),
                        2,
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(t_id!("myvar"))),
                        2,
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_negative_index() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[-1] }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(-1),
                Token::CloseBracket,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Id("x")),
                        -1,
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(t_id!("x"))),
                        -1,
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_chained() {
        // x[0][1] should be left-associative: Subscript(Subscript(x, 0), 1)
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[0][1] }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::OpenBracket,
                Token::Int(1),
                Token::CloseBracket,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Subscript(Box::new(pt::Expr::Id("x")), 0)),
                        1,
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Subscript(Box::new(ast::Expr::Id(t_id!("x"))), 0)),
                        1,
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_on_call() {
        // foo()[0] — subscript on a function call result
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { foo()[0] }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("foo"),
                Token::OpenParen,
                Token::CloseParen,
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Call("foo", vec![])),
                        0,
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Call(Box::new(ast::Expr::Id(t_id!("foo"))), vec![])),
                        0,
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_in_binop() {
        // x[0] + 1 — subscript has higher precedence than binary ops
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[0] + 1 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::Plus,
                Token::Int(1),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                        Box::new(pt::Expr::Subscript(Box::new(pt::Expr::Id("x")), 0)),
                        pt::Operator::Plus,
                        Box::new(pt::Expr::Int(1)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Subscript(Box::new(ast::Expr::Id(t_id!("x"))), 0)),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_on_parens_tuple() {
        // ((1, 2))[0] — subscript on a parenthesized tuple
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { ((1, 2))[0] }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Parens(Box::new(pt::Expr::Tuple(vec![
                            pt::Expr::Int(1),
                            pt::Expr::Int(2),
                        ])))),
                        0,
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Tuple(vec![
                            ast::Expr::Constant(Value::I64(1)),
                            ast::Expr::Constant(Value::I64(2)),
                        ])),
                        0,
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_in_ternary_condition() {
        // x[0] ? 1 : 2 — subscript in a ternary condition
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[0] ? 1 : 2 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::QuestionMark,
                Token::Int(1),
                Token::Colon,
                Token::Int(2),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Ternary(
                        Box::new(pt::Expr::Subscript(Box::new(pt::Expr::Id("x")), 0)),
                        Box::new(pt::Expr::Int(1)),
                        Box::new(pt::Expr::Int(2)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Ternary(
                        Box::new(ast::Expr::Subscript(Box::new(ast::Expr::Id(t_id!("x"))), 0)),
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                        Box::new(ast::Expr::Constant(Value::I64(2))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    // ── SubscriptAssign statement tests ─────────────────────────────────────

    #[test]
    fn test_subscript_assign_simple() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[0] = 1 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
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
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::SubscriptAssign("x", 0, pt::Expr::Int(1))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(t_id!("x"), 0),
                        ast::Expr::Constant(Value::I64(1)),
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_nonzero_index() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { tup[3] = 42 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("tup"),
                Token::OpenBracket,
                Token::Int(3),
                Token::CloseBracket,
                Token::Equals,
                Token::Int(42),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::SubscriptAssign("tup", 3, pt::Expr::Int(42))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(t_id!("tup"), 3),
                        ast::Expr::Constant(Value::I64(42)),
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_complex_expr() {
        // x[1] = y + 1
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[1] = y + 1 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(1),
                Token::CloseBracket,
                Token::Equals,
                Token::Identifier("y"),
                Token::Plus,
                Token::Int(1),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::SubscriptAssign(
                        "x",
                        1,
                        pt::Expr::Binary(
                            Box::new(pt::Expr::Id("y")),
                            pt::Operator::Plus,
                            Box::new(pt::Expr::Int(1)),
                        ),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(t_id!("x"), 1),
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Id(t_id!("y"))),
                            BinaryOperator::Add,
                            Box::new(ast::Expr::Constant(Value::I64(1))),
                        ),
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_tuple_value() {
        // x[0] = (1, 2) — assign a tuple to a subscript
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[0] = (1, 2) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::SubscriptAssign(
                        "x",
                        0,
                        pt::Expr::Tuple(vec![pt::Expr::Int(1), pt::Expr::Int(2)]),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(t_id!("x"), 0),
                        ast::Expr::Tuple(vec![
                            ast::Expr::Constant(Value::I64(1)),
                            ast::Expr::Constant(Value::I64(2)),
                        ]),
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_negative_index() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[-1] = 99 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(-1),
                Token::CloseBracket,
                Token::Equals,
                Token::Int(99),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::SubscriptAssign("x", -1, pt::Expr::Int(99))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(t_id!("x"), -1),
                        ast::Expr::Constant(Value::I64(99)),
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_in_if_body() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if true { x[0] = 1 } }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
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
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::If(
                        pt::Expr::Bool(true),
                        vec![pt::Statement::SubscriptAssign("x", 0, pt::Expr::Int(1))],
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![ast::Statement::Assign(
                            AssignDest::Subscript(t_id!("x"), 0),
                            ast::Expr::Constant(Value::I64(1)),
                        )],
                        vec![],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_with_subscript_expr_rhs() {
        // x[0] = y[1] — subscript on both sides
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[0] = y[1] }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::Equals,
                Token::Identifier("y"),
                Token::OpenBracket,
                Token::Int(1),
                Token::CloseBracket,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::SubscriptAssign(
                        "x",
                        0,
                        pt::Expr::Subscript(Box::new(pt::Expr::Id("y")), 1),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(t_id!("x"), 0),
                        ast::Expr::Subscript(Box::new(ast::Expr::Id(t_id!("y"))), 1),
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_multiline() {
        // Subscript assign alongside other statements
        let tc = ParserTestCase {
            input_str: r"fn main() -> int {
x = (1, 2)
x[0] = 42
}",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Newline,
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
                Token::Newline,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![
                        pt::Statement::Assign(
                            "x",
                            pt::Expr::Tuple(vec![pt::Expr::Int(1), pt::Expr::Int(2)]),
                        ),
                        pt::Statement::SubscriptAssign("x", 0, pt::Expr::Int(42)),
                    ],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!(LABEL_MAIN),
                    body: vec![
                        ast::Statement::Assign(
                            AssignDest::Id(t_id!("x")),
                            ast::Expr::Tuple(vec![
                                ast::Expr::Constant(Value::I64(1)),
                                ast::Expr::Constant(Value::I64(2)),
                            ]),
                        ),
                        ast::Statement::Assign(
                            AssignDest::Subscript(t_id!("x"), 0),
                            ast::Expr::Constant(Value::I64(42)),
                        ),
                    ],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    // ── Function definition tests ───────────────────────────────────

    #[test]
    fn test_fn_with_params() {
        let tc = ParserTestCase {
            input_str: r"fn add(x: int, y: int) -> int { x + y }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("add"),
                Token::OpenParen,
                Token::Identifier("x"), Token::Colon, Token::IntType, Token::Comma,
                Token::Identifier("y"), Token::Colon, Token::IntType,
                Token::CloseParen,
                Token::RightArrow, Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"), Token::Plus, Token::Identifier("y"),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "add",
                    params: vec![("x", ValueType::IntType), ("y", ValueType::IntType)],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                        Box::new(pt::Expr::Id("x")),
                        pt::Operator::Plus,
                        Box::new(pt::Expr::Id("y")),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("add"),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Id(t_id!("x"))),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::Id(t_id!("y"))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([
                        (t_id!("x"), ValueType::IntType),
                        (t_id!("y"), ValueType::IntType),
                    ]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_no_return_type() {
        let tc = ParserTestCase {
            input_str: r"fn greet() { print_int(42) }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("greet"),
                Token::OpenParen, Token::CloseParen,
                Token::OpenCurly,
                Token::Identifier("print_int"), Token::OpenParen, Token::Int(42), Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "greet",
                    params: vec![],
                    return_type: ValueType::NoneType,
                    statements: vec![pt::Statement::Expr(
                        pt::Expr::Call("print_int", vec![pt::Expr::Int(42)]),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("greet"),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(t_id!("print_int"))),
                        vec![ast::Expr::Constant(Value::I64(42))],
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::NoneType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_single_param() {
        let tc = ParserTestCase {
            input_str: r"fn negate(x: int) -> int { 0 - x }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("negate"),
                Token::OpenParen,
                Token::Identifier("x"), Token::Colon, Token::IntType,
                Token::CloseParen,
                Token::RightArrow, Token::IntType,
                Token::OpenCurly,
                Token::Int(0), Token::Minus, Token::Identifier("x"),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "negate",
                    params: vec![("x", ValueType::IntType)],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                        Box::new(pt::Expr::Int(0)),
                        pt::Operator::Minus,
                        Box::new(pt::Expr::Id("x")),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("negate"),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(Value::I64(0))),
                        BinaryOperator::Subtract,
                        Box::new(ast::Expr::Id(t_id!("x"))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_id!("x"), ValueType::IntType)]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_bool_param() {
        let tc = ParserTestCase {
            input_str: r"fn check(flag: bool) -> int { 1 }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("check"),
                Token::OpenParen,
                Token::Identifier("flag"), Token::Colon, Token::BoolType,
                Token::CloseParen,
                Token::RightArrow, Token::IntType,
                Token::OpenCurly, Token::Int(1), Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "check",
                    params: vec![("flag", ValueType::BoolType)],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Int(1))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("check"),
                    body: vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(1)))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_id!("flag"), ValueType::BoolType)]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_return_with_value() {
        let tc = ParserTestCase {
            input_str: r"fn id(x: int) -> int { return x }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("id"),
                Token::OpenParen,
                Token::Identifier("x"), Token::Colon, Token::IntType,
                Token::CloseParen,
                Token::RightArrow, Token::IntType,
                Token::OpenCurly,
                Token::Return, Token::Identifier("x"),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "id",
                    params: vec![("x", ValueType::IntType)],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Return(Some(pt::Expr::Id("x")))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("id"),
                    body: vec![ast::Statement::Return(ast::Expr::Id(t_id!("x")))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_id!("x"), ValueType::IntType)]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_return_no_value() {
        let tc = ParserTestCase {
            input_str: r"fn donothing() { return }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("donothing"),
                Token::OpenParen, Token::CloseParen,
                Token::OpenCurly,
                Token::Return,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "donothing",
                    params: vec![],
                    return_type: ValueType::NoneType,
                    statements: vec![pt::Statement::Return(None)],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("donothing"),
                    body: vec![ast::Statement::Return(ast::Expr::Constant(Value::None))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::NoneType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_multiple_functions() {
        let tc = ParserTestCase {
            input_str: "fn double(x: int) -> int { x + x }\nfn main() -> int { print_int(double(21)) }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("double"),
                Token::OpenParen,
                Token::Identifier("x"), Token::Colon, Token::IntType,
                Token::CloseParen,
                Token::RightArrow, Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"), Token::Plus, Token::Identifier("x"),
                Token::CloseCurly,
                Token::Newline,
                Token::Fn, Token::Identifier("main"),
                Token::OpenParen, Token::CloseParen,
                Token::RightArrow, Token::IntType,
                Token::OpenCurly,
                Token::Identifier("print_int"), Token::OpenParen,
                Token::Identifier("double"), Token::OpenParen, Token::Int(21), Token::CloseParen,
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![
                    pt::Function {
                        name: "double",
                        params: vec![("x", ValueType::IntType)],
                        return_type: ValueType::IntType,
                        statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                            Box::new(pt::Expr::Id("x")),
                            pt::Operator::Plus,
                            Box::new(pt::Expr::Id("x")),
                        ))],
                    },
                    pt::Function {
                        name: "main",
                        params: vec![],
                        return_type: ValueType::IntType,
                        statements: vec![pt::Statement::Expr(pt::Expr::Call(
                            "print_int",
                            vec![pt::Expr::Call("double", vec![pt::Expr::Int(21)])],
                        ))],
                    },
                ],
            },
            expected_ast: ast::Program {
                functions: vec![
                    ast::Function {
                        name: t_id!("double"),
                        body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Id(t_id!("x"))),
                            BinaryOperator::Add,
                            Box::new(ast::Expr::Id(t_id!("x"))),
                        ))],
                        types: HashMap::new(),
                        params: IndexMap::from([(t_id!("x"), ValueType::IntType)]),
                        return_type: ValueType::IntType,
                    },
                    ast::Function {
                        name: t_id!("main"),
                        body: vec![ast::Statement::Expr(ast::Expr::Call(
                            Box::new(ast::Expr::Id(t_id!("print_int"))),
                            vec![ast::Expr::Call(
                                Box::new(ast::Expr::Id(t_id!("double"))),
                                vec![ast::Expr::Constant(Value::I64(21))],
                            )],
                        ))],
                        types: HashMap::new(),
                        params: IndexMap::new(),
                        return_type: ValueType::IntType,
                    },
                ],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_return_bool_type() {
        let tc = ParserTestCase {
            input_str: r"fn is_zero(x: int) -> bool { x == 0 }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("is_zero"),
                Token::OpenParen,
                Token::Identifier("x"), Token::Colon, Token::IntType,
                Token::CloseParen,
                Token::RightArrow, Token::BoolType,
                Token::OpenCurly,
                Token::Identifier("x"), Token::DoubleEquals, Token::Int(0),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "is_zero",
                    params: vec![("x", ValueType::IntType)],
                    return_type: ValueType::BoolType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                        Box::new(pt::Expr::Id("x")),
                        pt::Operator::Equals,
                        Box::new(pt::Expr::Int(0)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("is_zero"),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Id(t_id!("x"))),
                        BinaryOperator::Equals,
                        Box::new(ast::Expr::Constant(Value::I64(0))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_id!("x"), ValueType::IntType)]),
                    return_type: ValueType::BoolType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_multiline_body() {
        let tc = ParserTestCase {
            input_str: "fn main() -> int { x = 10\nprint_int(x)\nreturn x }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("main"),
                Token::OpenParen, Token::CloseParen,
                Token::RightArrow, Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"), Token::Equals, Token::Int(10),
                Token::Newline,
                Token::Identifier("print_int"), Token::OpenParen, Token::Identifier("x"), Token::CloseParen,
                Token::Newline,
                Token::Return, Token::Identifier("x"),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![
                        pt::Statement::Assign("x", pt::Expr::Int(10)),
                        pt::Statement::Expr(pt::Expr::Call("print_int", vec![pt::Expr::Id("x")])),
                        pt::Statement::Return(Some(pt::Expr::Id("x"))),
                    ],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("main"),
                    body: vec![
                        ast::Statement::Assign(
                            AssignDest::Id(t_id!("x")),
                            ast::Expr::Constant(Value::I64(10)),
                        ),
                        ast::Statement::Expr(ast::Expr::Call(
                            Box::new(ast::Expr::Id(t_id!("print_int"))),
                            vec![ast::Expr::Id(t_id!("x"))],
                        )),
                        ast::Statement::Return(ast::Expr::Id(t_id!("x"))),
                    ],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_return_expr() {
        let tc = ParserTestCase {
            input_str: r"fn square(n: int) -> int { return n * n }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("square"),
                Token::OpenParen,
                Token::Identifier("n"), Token::Colon, Token::IntType,
                Token::CloseParen,
                Token::RightArrow, Token::IntType,
                Token::OpenCurly,
                Token::Return,
                Token::Identifier("n"), Token::Asterisk, Token::Identifier("n"),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "square",
                    params: vec![("n", ValueType::IntType)],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Return(Some(pt::Expr::Binary(
                        Box::new(pt::Expr::Id("n")),
                        pt::Operator::Asterisk,
                        Box::new(pt::Expr::Id("n")),
                    )))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("square"),
                    body: vec![ast::Statement::Return(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Id(t_id!("n"))),
                        BinaryOperator::Multiply,
                        Box::new(ast::Expr::Id(t_id!("n"))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_id!("n"), ValueType::IntType)]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_tuple_param() {
        let tc = ParserTestCase {
            input_str: r"fn first(p: tuple<int, int>) -> int { p[0] }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("first"),
                Token::OpenParen,
                Token::Identifier("p"), Token::Colon,
                Token::TupleType, Token::Less, Token::IntType, Token::Comma, Token::IntType, Token::Greater,
                Token::CloseParen,
                Token::RightArrow, Token::IntType,
                Token::OpenCurly,
                Token::Identifier("p"), Token::OpenBracket, Token::Int(0), Token::CloseBracket,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "first",
                    params: vec![("p", ValueType::TupleType(vec![ValueType::IntType, ValueType::IntType]))],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Id("p")),
                        0,
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("first"),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(t_id!("p"))),
                        0,
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(
                        t_id!("p"),
                        ValueType::TupleType(vec![ValueType::IntType, ValueType::IntType]),
                    )]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_tuple_return_type() {
        let tc = ParserTestCase {
            input_str: r"fn pair(a: int, b: int) -> tuple<int, int> { (a, b) }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("pair"),
                Token::OpenParen,
                Token::Identifier("a"), Token::Colon, Token::IntType, Token::Comma,
                Token::Identifier("b"), Token::Colon, Token::IntType,
                Token::CloseParen,
                Token::RightArrow,
                Token::TupleType, Token::Less, Token::IntType, Token::Comma, Token::IntType, Token::Greater,
                Token::OpenCurly,
                Token::OpenParen, Token::Identifier("a"), Token::Comma, Token::Identifier("b"), Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "pair",
                    params: vec![("a", ValueType::IntType), ("b", ValueType::IntType)],
                    return_type: ValueType::TupleType(vec![ValueType::IntType, ValueType::IntType]),
                    statements: vec![pt::Statement::Expr(pt::Expr::Tuple(vec![
                        pt::Expr::Id("a"),
                        pt::Expr::Id("b"),
                    ]))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("pair"),
                    body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                        ast::Expr::Id(t_id!("a")),
                        ast::Expr::Id(t_id!("b")),
                    ]))],
                    types: HashMap::new(),
                    params: IndexMap::from([
                        (t_id!("a"), ValueType::IntType),
                        (t_id!("b"), ValueType::IntType),
                    ]),
                    return_type: ValueType::TupleType(vec![ValueType::IntType, ValueType::IntType]),
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_mixed_tuple_and_primitive_params() {
        let tc = ParserTestCase {
            input_str: r"fn mix(x: int, p: tuple<int, bool>) -> bool { true }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("mix"),
                Token::OpenParen,
                Token::Identifier("x"), Token::Colon, Token::IntType, Token::Comma,
                Token::Identifier("p"), Token::Colon,
                Token::TupleType, Token::Less, Token::IntType, Token::Comma, Token::BoolType, Token::Greater,
                Token::CloseParen,
                Token::RightArrow, Token::BoolType,
                Token::OpenCurly, Token::Bool(true), Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "mix",
                    params: vec![
                        ("x", ValueType::IntType),
                        ("p", ValueType::TupleType(vec![ValueType::IntType, ValueType::BoolType])),
                    ],
                    return_type: ValueType::BoolType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Bool(true))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("mix"),
                    body: vec![ast::Statement::Expr(ast::Expr::Constant(Value::Bool(true)))],
                    types: HashMap::new(),
                    params: IndexMap::from([
                        (t_id!("x"), ValueType::IntType),
                        (t_id!("p"), ValueType::TupleType(vec![ValueType::IntType, ValueType::BoolType])),
                    ]),
                    return_type: ValueType::BoolType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_nested_tuple_param() {
        let tc = ParserTestCase {
            input_str: r"fn deep(t: tuple<tuple<int, int>, bool>) -> int { t[0][0] }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("deep"),
                Token::OpenParen,
                Token::Identifier("t"), Token::Colon,
                Token::TupleType, Token::Less,
                Token::TupleType, Token::Less, Token::IntType, Token::Comma, Token::IntType, Token::Greater,
                Token::Comma, Token::BoolType,
                Token::Greater,
                Token::CloseParen,
                Token::RightArrow, Token::IntType,
                Token::OpenCurly,
                Token::Identifier("t"),
                Token::OpenBracket, Token::Int(0), Token::CloseBracket,
                Token::OpenBracket, Token::Int(0), Token::CloseBracket,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "deep",
                    params: vec![(
                        "t",
                        ValueType::TupleType(vec![
                            ValueType::TupleType(vec![ValueType::IntType, ValueType::IntType]),
                            ValueType::BoolType,
                        ]),
                    )],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Subscript(Box::new(pt::Expr::Id("t")), 0)),
                        0,
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("deep"),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Subscript(
                            Box::new(ast::Expr::Id(t_id!("t"))),
                            0,
                        )),
                        0,
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(
                        t_id!("t"),
                        ValueType::TupleType(vec![
                            ValueType::TupleType(vec![ValueType::IntType, ValueType::IntType]),
                            ValueType::BoolType,
                        ]),
                    )]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_nested_tuple_return_type() {
        let tc = ParserTestCase {
            input_str: r"fn wrap(x: int) -> tuple<tuple<int, int>, bool> { ((x, x), true) }",
            expected_tokens: vec![
                Token::Fn, Token::Identifier("wrap"),
                Token::OpenParen,
                Token::Identifier("x"), Token::Colon, Token::IntType,
                Token::CloseParen,
                Token::RightArrow,
                Token::TupleType, Token::Less,
                Token::TupleType, Token::Less, Token::IntType, Token::Comma, Token::IntType, Token::Greater,
                Token::Comma, Token::BoolType,
                Token::Greater,
                Token::OpenCurly,
                Token::OpenParen,
                Token::OpenParen, Token::Identifier("x"), Token::Comma, Token::Identifier("x"), Token::CloseParen,
                Token::Comma,
                Token::Bool(true),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "wrap",
                    params: vec![("x", ValueType::IntType)],
                    return_type: ValueType::TupleType(vec![
                        ValueType::TupleType(vec![ValueType::IntType, ValueType::IntType]),
                        ValueType::BoolType,
                    ]),
                    statements: vec![pt::Statement::Expr(pt::Expr::Tuple(vec![
                        pt::Expr::Tuple(vec![pt::Expr::Id("x"), pt::Expr::Id("x")]),
                        pt::Expr::Bool(true),
                    ]))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_id!("wrap"),
                    body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                        ast::Expr::Tuple(vec![
                            ast::Expr::Id(t_id!("x")),
                            ast::Expr::Id(t_id!("x")),
                        ]),
                        ast::Expr::Constant(Value::Bool(true)),
                    ]))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_id!("x"), ValueType::IntType)]),
                    return_type: ValueType::TupleType(vec![
                        ValueType::TupleType(vec![ValueType::IntType, ValueType::IntType]),
                        ValueType::BoolType,
                    ]),
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }
}
