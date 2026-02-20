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

    use crate::utils::{t_global, t_local};
    use test_support::compiler::{
        constants::LABEL_MAIN,
        syntax_trees::{ast, parser::*, shared::*},
    };

    macro_rules! main_local {
        ($name:expr) => {
            t_local!($name, t_global!(LABEL_MAIN))
        };
    }

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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                        None,
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Id(main_local!("x")),
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Constant(Value::I64(1000))),
                            BinaryOperator::Add,
                            Box::new(ast::Expr::Constant(Value::I64(-21912983))),
                        ),
                        None,
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
                        Box::new(pt::Expr::Id("print")),
                        vec![pt::Expr::Id("x")],
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(main_local!("print"))),
                        vec![ast::Expr::Id(main_local!("x"))],
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

                    statements: vec![pt::Statement::Expr(pt::Expr::Call(
                        Box::new(pt::Expr::Id("oogabooga")),
                        vec![],
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(main_local!("oogabooga"))),
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
                        Box::new(pt::Expr::Id("print")),
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
                            Box::new(pt::Expr::Call(Box::new(pt::Expr::Id("read_int")), vec![])),
                        )],
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(main_local!("print"))),
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
                            Box::new(ast::Expr::Call(
                                Box::new(ast::Expr::Id(main_local!("read_int"))),
                                vec![],
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
                        Box::new(pt::Expr::Id("print")),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(main_local!("print"))),
                        vec![
                            ast::Expr::Id(main_local!("x")),
                            ast::Expr::Id(main_local!("y")),
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
                        pt::Expr::Call(Box::new(pt::Expr::Id("Fffoo")), vec![pt::Expr::Int(1)]),
                        None,
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Id(main_local!("x")),
                        ast::Expr::Call(
                            Box::new(ast::Expr::Id(main_local!("Fffoo"))),
                            vec![ast::Expr::Constant(Value::I64(1))],
                        ),
                        None,
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
                        pt::Statement::Assign("x", pt::Expr::Int(100), None),
                        pt::Statement::Expr(pt::Expr::Call(
                            Box::new(pt::Expr::Id("print")),
                            vec![pt::Expr::Int(1000)],
                        )),
                        pt::Statement::Assign("whatevn", pt::Expr::Id("x"), None),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![
                        ast::Statement::Assign(
                            AssignDest::Id(main_local!("x")),
                            ast::Expr::Constant(Value::I64(100)),
                            None,
                        ),
                        ast::Statement::Expr(ast::Expr::Call(
                            Box::new(ast::Expr::Id(main_local!("print"))),
                            vec![ast::Expr::Constant(Value::I64(1000))],
                        )),
                        ast::Statement::Assign(
                            AssignDest::Id(main_local!("whatevn")),
                            ast::Expr::Id(main_local!("x")),
                            None,
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                        Box::new(pt::Expr::Id("truefoofalse")),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(main_local!("truefoofalse"))),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                            Box::new(pt::Expr::Id("print")),
                            vec![pt::Expr::Id("x")],
                        ))],
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Id(main_local!("x"))),
                            BinaryOperator::Equals,
                            Box::new(ast::Expr::Constant(Value::I64(1))),
                        ),
                        vec![ast::Statement::Expr(ast::Expr::Call(
                            Box::new(ast::Expr::Id(main_local!("print"))),
                            vec![ast::Expr::Id(main_local!("x"))],
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
                            pt::Statement::Assign("x", pt::Expr::Int(1), None),
                            pt::Statement::Expr(pt::Expr::Call(
                                Box::new(pt::Expr::Id("print")),
                                vec![pt::Expr::Id("x")],
                            )),
                        ],
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![
                            ast::Statement::Assign(
                                AssignDest::Id(main_local!("x")),
                                ast::Expr::Constant(Value::I64(1)),
                                None,
                            ),
                            ast::Statement::Expr(ast::Expr::Call(
                                Box::new(ast::Expr::Id(main_local!("print"))),
                                vec![ast::Expr::Id(main_local!("x"))],
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Id(main_local!("x"))),
                            BinaryOperator::Equals,
                            Box::new(ast::Expr::Constant(Value::I64(1))),
                        ),
                        vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(1)))],
                        vec![ast::Statement::Conditional(
                            ast::Expr::BinaryOp(
                                Box::new(ast::Expr::Id(main_local!("x"))),
                                BinaryOperator::Equals,
                                Box::new(ast::Expr::Constant(Value::I64(2))),
                            ),
                            vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(2)))],
                            vec![ast::Statement::Conditional(
                                ast::Expr::BinaryOp(
                                    Box::new(ast::Expr::Id(main_local!("x"))),
                                    BinaryOperator::Equals,
                                    Box::new(ast::Expr::Constant(Value::I64(3))),
                                ),
                                vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(3)))],
                                vec![ast::Statement::Conditional(
                                    ast::Expr::BinaryOp(
                                        Box::new(ast::Expr::Id(main_local!("x"))),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                            pt::Expr::Call(Box::new(pt::Expr::Id("read_int")), vec![]),
                            pt::Expr::Bool(true),
                        ]),
                        None,
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Id(main_local!("x")),
                        ast::Expr::Tuple(vec![
                            ast::Expr::BinaryOp(
                                Box::new(ast::Expr::Constant(Value::I64(1))),
                                BinaryOperator::Add,
                                Box::new(ast::Expr::Constant(Value::I64(2))),
                            ),
                            ast::Expr::Call(
                                Box::new(ast::Expr::Id(main_local!("read_int"))),
                                vec![],
                            ),
                            ast::Expr::Constant(Value::Bool(true)),
                        ]),
                        None,
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
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
                        Box::new(pt::Expr::Id("print")),
                        vec![pt::Expr::Tuple(vec![pt::Expr::Int(1), pt::Expr::Int(2)])],
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(main_local!("print"))),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(main_local!("x"))),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(main_local!("myvar"))),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(main_local!("x"))),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Subscript(
                            Box::new(ast::Expr::Id(main_local!("x"))),
                            0,
                        )),
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
                        Box::new(pt::Expr::Call(Box::new(pt::Expr::Id("foo")), vec![])),
                        0,
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Call(
                            Box::new(ast::Expr::Id(main_local!("foo"))),
                            vec![],
                        )),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Subscript(
                            Box::new(ast::Expr::Id(main_local!("x"))),
                            0,
                        )),
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
                    name: t_global!(LABEL_MAIN),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Ternary(
                        Box::new(ast::Expr::Subscript(
                            Box::new(ast::Expr::Id(main_local!("x"))),
                            0,
                        )),
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(main_local!("x"), 0),
                        ast::Expr::Constant(Value::I64(1)),
                        None,
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(main_local!("tup"), 3),
                        ast::Expr::Constant(Value::I64(42)),
                        None,
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(main_local!("x"), 1),
                        ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Id(main_local!("y"))),
                            BinaryOperator::Add,
                            Box::new(ast::Expr::Constant(Value::I64(1))),
                        ),
                        None,
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(main_local!("x"), 0),
                        ast::Expr::Tuple(vec![
                            ast::Expr::Constant(Value::I64(1)),
                            ast::Expr::Constant(Value::I64(2)),
                        ]),
                        None,
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(main_local!("x"), -1),
                        ast::Expr::Constant(Value::I64(99)),
                        None,
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![ast::Statement::Assign(
                            AssignDest::Subscript(main_local!("x"), 0),
                            ast::Expr::Constant(Value::I64(1)),
                            None,
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
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::Subscript(main_local!("x"), 0),
                        ast::Expr::Subscript(Box::new(ast::Expr::Id(main_local!("y"))), 1),
                        None,
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
                            None,
                        ),
                        pt::Statement::SubscriptAssign("x", 0, pt::Expr::Int(42)),
                    ],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![
                        ast::Statement::Assign(
                            AssignDest::Id(main_local!("x")),
                            ast::Expr::Tuple(vec![
                                ast::Expr::Constant(Value::I64(1)),
                                ast::Expr::Constant(Value::I64(2)),
                            ]),
                            None,
                        ),
                        ast::Statement::Assign(
                            AssignDest::Subscript(main_local!("x"), 0),
                            ast::Expr::Constant(Value::I64(42)),
                            None,
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
        let add_id = t_global!("add");

        let tc = ParserTestCase {
            input_str: r"fn add(x: int, y: int) -> int { x + y }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("add"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Colon,
                Token::IntType,
                Token::Comma,
                Token::Identifier("y"),
                Token::Colon,
                Token::IntType,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::Plus,
                Token::Identifier("y"),
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
                    name: add_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Id(t_local!("x", add_id.clone()))),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::Id(t_local!("y", add_id.clone()))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([
                        (t_local!("x", add_id.clone()), ValueType::IntType),
                        (t_local!("y", add_id.clone()), ValueType::IntType),
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
        let greet_id = t_global!("greet");

        let tc = ParserTestCase {
            input_str: r"fn greet() { print_int(42) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("greet"),
                Token::OpenParen,
                Token::CloseParen,
                Token::OpenCurly,
                Token::Identifier("print_int"),
                Token::OpenParen,
                Token::Int(42),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "greet",
                    params: vec![],
                    return_type: ValueType::NoneType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Call(
                        Box::new(pt::Expr::Id("print_int")),
                        vec![pt::Expr::Int(42)],
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!("greet"),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Id(t_local!("print_int", greet_id.clone()))),
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
        let neg_id = t_global!("negate");
        let tc = ParserTestCase {
            input_str: r"fn negate(x: int) -> int { 0 - x }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("negate"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Colon,
                Token::IntType,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Int(0),
                Token::Minus,
                Token::Identifier("x"),
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
                    name: neg_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Constant(Value::I64(0))),
                        BinaryOperator::Subtract,
                        Box::new(ast::Expr::Id(t_local!("x", neg_id.clone()))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_local!("x", neg_id.clone()), ValueType::IntType)]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_bool_param() {
        let check_id = t_global!("check");
        let tc = ParserTestCase {
            input_str: r"fn check(flag: bool) -> int { 1 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("check"),
                Token::OpenParen,
                Token::Identifier("flag"),
                Token::Colon,
                Token::BoolType,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Int(1),
                Token::CloseCurly,
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
                    name: check_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::Constant(Value::I64(1)))],
                    types: HashMap::new(),
                    params: IndexMap::from([(
                        t_local!("flag", check_id.clone()),
                        ValueType::BoolType,
                    )]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_return_with_value() {
        let id_id = t_global!("id");
        let tc = ParserTestCase {
            input_str: r"fn id(x: int) -> int { return x }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("id"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Colon,
                Token::IntType,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Return,
                Token::Identifier("x"),
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
                    name: id_id.clone(),
                    body: vec![ast::Statement::Return(ast::Expr::Id(t_local!(
                        "x",
                        id_id.clone()
                    )))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_local!("x", id_id.clone()), ValueType::IntType)]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_return_no_value() {
        let dn_id = t_global!("donothing");
        let tc = ParserTestCase {
            input_str: r"fn donothing() { return }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("donothing"),
                Token::OpenParen,
                Token::CloseParen,
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
                    name: dn_id.clone(),
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
        let d_id = t_global!("double");
        let tc = ParserTestCase {
            input_str: "fn double(x: int) -> int { x + x }\nfn main() -> int { print_int(double(21)) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("double"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Colon,
                Token::IntType,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::Plus,
                Token::Identifier("x"),
                Token::CloseCurly,
                Token::Newline,
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("print_int"),
                Token::OpenParen,
                Token::Identifier("double"),
                Token::OpenParen,
                Token::Int(21),
                Token::CloseParen,
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
                            Box::new(pt::Expr::Id("print_int")),
                            vec![pt::Expr::Call(
                                Box::new(pt::Expr::Id("double")),
                                vec![pt::Expr::Int(21)],
                            )],
                        ))],
                    },
                ],
            },
            expected_ast: ast::Program {
                functions: vec![
                    ast::Function {
                        name: d_id.clone(),
                        body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                            Box::new(ast::Expr::Id(t_local!("x", d_id.clone()))),
                            BinaryOperator::Add,
                            Box::new(ast::Expr::Id(t_local!("x", d_id.clone()))),
                        ))],
                        types: HashMap::new(),
                        params: IndexMap::from([(t_local!("x", d_id.clone()), ValueType::IntType)]),
                        return_type: ValueType::IntType,
                    },
                    ast::Function {
                        name: t_global!("main"),
                        body: vec![ast::Statement::Expr(ast::Expr::Call(
                            Box::new(ast::Expr::Id(main_local!("print_int"))),
                            vec![ast::Expr::Call(
                                Box::new(ast::Expr::Id(main_local!("double"))),
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
        let iz_id = t_global!("is_zero");
        let tc = ParserTestCase {
            input_str: r"fn is_zero(x: int) -> bool { x == 0 }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("is_zero"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Colon,
                Token::IntType,
                Token::CloseParen,
                Token::RightArrow,
                Token::BoolType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::DoubleEquals,
                Token::Int(0),
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
                    name: iz_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Id(t_local!("x", iz_id.clone()))),
                        BinaryOperator::Equals,
                        Box::new(ast::Expr::Constant(Value::I64(0))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_local!("x", iz_id.clone()), ValueType::IntType)]),
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
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("x"),
                Token::Equals,
                Token::Int(10),
                Token::Newline,
                Token::Identifier("print_int"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::CloseParen,
                Token::Newline,
                Token::Return,
                Token::Identifier("x"),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![
                        pt::Statement::Assign("x", pt::Expr::Int(10), None),
                        pt::Statement::Expr(pt::Expr::Call(
                            Box::new(pt::Expr::Id("print_int")),
                            vec![pt::Expr::Id("x")],
                        )),
                        pt::Statement::Return(Some(pt::Expr::Id("x"))),
                    ],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!("main"),
                    body: vec![
                        ast::Statement::Assign(
                            AssignDest::Id(main_local!("x")),
                            ast::Expr::Constant(Value::I64(10)),
                            None,
                        ),
                        ast::Statement::Expr(ast::Expr::Call(
                            Box::new(ast::Expr::Id(main_local!("print_int"))),
                            vec![ast::Expr::Id(main_local!("x"))],
                        )),
                        ast::Statement::Return(ast::Expr::Id(main_local!("x"))),
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
        let sq_id = t_global!("square");
        let tc = ParserTestCase {
            input_str: r"fn square(n: int) -> int { return n * n }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("square"),
                Token::OpenParen,
                Token::Identifier("n"),
                Token::Colon,
                Token::IntType,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Return,
                Token::Identifier("n"),
                Token::Asterisk,
                Token::Identifier("n"),
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
                    name: sq_id.clone(),
                    body: vec![ast::Statement::Return(ast::Expr::BinaryOp(
                        Box::new(ast::Expr::Id(t_local!("n", sq_id.clone()))),
                        BinaryOperator::Multiply,
                        Box::new(ast::Expr::Id(t_local!("n", sq_id.clone()))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_local!("n", sq_id.clone()), ValueType::IntType)]),
                    return_type: ValueType::IntType,
                }],
                function_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_tuple_param() {
        let first_id = t_global!("first");
        let tc = ParserTestCase {
            input_str: r"fn first(p: tuple<int, int>) -> int { p[0] }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("first"),
                Token::OpenParen,
                Token::Identifier("p"),
                Token::Colon,
                Token::TupleType,
                Token::Less,
                Token::IntType,
                Token::Comma,
                Token::IntType,
                Token::Greater,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("p"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "first",
                    params: vec![(
                        "p",
                        ValueType::TupleType(vec![ValueType::IntType, ValueType::IntType]),
                    )],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Id("p")),
                        0,
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: first_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(t_local!("p", first_id.clone()))),
                        0,
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(
                        t_local!("p", first_id.clone()),
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
        let pair_id = t_global!("pair");
        let tc = ParserTestCase {
            input_str: r"fn pair(a: int, b: int) -> tuple<int, int> { (a, b) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("pair"),
                Token::OpenParen,
                Token::Identifier("a"),
                Token::Colon,
                Token::IntType,
                Token::Comma,
                Token::Identifier("b"),
                Token::Colon,
                Token::IntType,
                Token::CloseParen,
                Token::RightArrow,
                Token::TupleType,
                Token::Less,
                Token::IntType,
                Token::Comma,
                Token::IntType,
                Token::Greater,
                Token::OpenCurly,
                Token::OpenParen,
                Token::Identifier("a"),
                Token::Comma,
                Token::Identifier("b"),
                Token::CloseParen,
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
                    name: pair_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                        ast::Expr::Id(t_local!("a", pair_id.clone())),
                        ast::Expr::Id(t_local!("b", pair_id.clone())),
                    ]))],
                    types: HashMap::new(),
                    params: IndexMap::from([
                        (t_local!("a", pair_id.clone()), ValueType::IntType),
                        (t_local!("b", pair_id.clone()), ValueType::IntType),
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
        let mix_id = t_global!("mix");
        let tc = ParserTestCase {
            input_str: r"fn mix(x: int, p: tuple<int, bool>) -> bool { true }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("mix"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Colon,
                Token::IntType,
                Token::Comma,
                Token::Identifier("p"),
                Token::Colon,
                Token::TupleType,
                Token::Less,
                Token::IntType,
                Token::Comma,
                Token::BoolType,
                Token::Greater,
                Token::CloseParen,
                Token::RightArrow,
                Token::BoolType,
                Token::OpenCurly,
                Token::Bool(true),
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "mix",
                    params: vec![
                        ("x", ValueType::IntType),
                        (
                            "p",
                            ValueType::TupleType(vec![ValueType::IntType, ValueType::BoolType]),
                        ),
                    ],
                    return_type: ValueType::BoolType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Bool(true))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: mix_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::Constant(Value::Bool(true)))],
                    types: HashMap::new(),
                    params: IndexMap::from([
                        (t_local!("x", mix_id.clone()), ValueType::IntType),
                        (
                            t_local!("p", mix_id.clone()),
                            ValueType::TupleType(vec![ValueType::IntType, ValueType::BoolType]),
                        ),
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
        let deep_id = t_global!("deep");
        let tc = ParserTestCase {
            input_str: r"fn deep(t: tuple<tuple<int, int>, bool>) -> int { t[0][0] }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("deep"),
                Token::OpenParen,
                Token::Identifier("t"),
                Token::Colon,
                Token::TupleType,
                Token::Less,
                Token::TupleType,
                Token::Less,
                Token::IntType,
                Token::Comma,
                Token::IntType,
                Token::Greater,
                Token::Comma,
                Token::BoolType,
                Token::Greater,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("t"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
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
                    name: deep_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Subscript(
                            Box::new(ast::Expr::Id(t_local!("t", deep_id.clone()))),
                            0,
                        )),
                        0,
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(
                        t_local!("t", deep_id.clone()),
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
    fn test_ternary_call() {
        // (i == 0 ? a : b)(10) — call result of ternary expression
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { (i == 0 ? a : b)(10) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::OpenParen,
                Token::Identifier("i"),
                Token::DoubleEquals,
                Token::Int(0),
                Token::QuestionMark,
                Token::Identifier("a"),
                Token::Colon,
                Token::Identifier("b"),
                Token::CloseParen,
                Token::OpenParen,
                Token::Int(10),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Call(
                        Box::new(pt::Expr::Parens(Box::new(pt::Expr::Ternary(
                            Box::new(pt::Expr::Binary(
                                Box::new(pt::Expr::Id("i")),
                                pt::Operator::Equals,
                                Box::new(pt::Expr::Int(0)),
                            )),
                            Box::new(pt::Expr::Id("a")),
                            Box::new(pt::Expr::Id("b")),
                        )))),
                        vec![pt::Expr::Int(10)],
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Ternary(
                            Box::new(ast::Expr::BinaryOp(
                                Box::new(ast::Expr::Id(main_local!("i"))),
                                BinaryOperator::Equals,
                                Box::new(ast::Expr::Constant(Value::I64(0))),
                            )),
                            Box::new(ast::Expr::Id(main_local!("a"))),
                            Box::new(ast::Expr::Id(main_local!("b"))),
                        )),
                        vec![ast::Expr::Constant(Value::I64(10))],
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
    fn test_subscript_call() {
        // t[0](10) — call function obtained by subscripting a tuple
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { t[0](10) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("t"),
                Token::OpenBracket,
                Token::Int(0),
                Token::CloseBracket,
                Token::OpenParen,
                Token::Int(10),
                Token::CloseParen,
                Token::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Call(
                        Box::new(pt::Expr::Subscript(Box::new(pt::Expr::Id("t")), 0)),
                        vec![pt::Expr::Int(10)],
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Call(
                        Box::new(ast::Expr::Subscript(
                            Box::new(ast::Expr::Id(main_local!("t"))),
                            0,
                        )),
                        vec![ast::Expr::Constant(Value::I64(10))],
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
    fn test_fn_nested_tuple_return_type() {
        let wrap_id = t_global!("wrap");
        let tc = ParserTestCase {
            input_str: r"fn wrap(x: int) -> tuple<tuple<int, int>, bool> { ((x, x), true) }",
            expected_tokens: vec![
                Token::Fn,
                Token::Identifier("wrap"),
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Colon,
                Token::IntType,
                Token::CloseParen,
                Token::RightArrow,
                Token::TupleType,
                Token::Less,
                Token::TupleType,
                Token::Less,
                Token::IntType,
                Token::Comma,
                Token::IntType,
                Token::Greater,
                Token::Comma,
                Token::BoolType,
                Token::Greater,
                Token::OpenCurly,
                Token::OpenParen,
                Token::OpenParen,
                Token::Identifier("x"),
                Token::Comma,
                Token::Identifier("x"),
                Token::CloseParen,
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
                    name: wrap_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::Tuple(vec![
                        ast::Expr::Tuple(vec![
                            ast::Expr::Id(t_local!("x", wrap_id.clone())),
                            ast::Expr::Id(t_local!("x", wrap_id.clone())),
                        ]),
                        ast::Expr::Constant(Value::Bool(true)),
                    ]))],
                    types: HashMap::new(),
                    params: IndexMap::from([(t_local!("x", wrap_id.clone()), ValueType::IntType)]),
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

    #[test]
    fn test_lambda_simple() {
        // lambda x: x + 1
        let input_str = r"fn main() -> int { lambda x: x + 1 }";

        let tokens = tokenize(input_str).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Lambda,
                Token::Identifier("x"),
                Token::Colon,
                Token::Identifier("x"),
                Token::Plus,
                Token::Int(1),
                Token::CloseCurly,
            ]
        );

        let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
        assert_eq!(
            parse_tree,
            pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Lambda(
                        vec!["x"],
                        vec![pt::Statement::Return(Some(pt::Expr::Binary(
                            Box::new(pt::Expr::Id("x")),
                            pt::Operator::Plus,
                            Box::new(pt::Expr::Int(1)),
                        )))],
                    ))],
                }],
            }
        );

        let ast = to_ast::to_ast(parse_tree);
        // Check the main function has one statement with a Lambda expr
        assert_eq!(ast.functions.len(), 1);
        let main_body = &ast.functions[0].body;
        assert_eq!(main_body.len(), 1);
        match &main_body[0] {
            ast::Statement::Expr(ast::Expr::Lambda(func)) => {
                assert_eq!(func.params.len(), 1);
                assert_eq!(func.return_type, ValueType::Indeterminate);
                assert_eq!(func.body.len(), 1);
            }
            other => panic!("expected Lambda, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_multi_args() {
        // lambda x, y: x + y
        let input_str = r"fn main() -> int { lambda x, y: x + y }";

        let tokens = tokenize(input_str).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Lambda,
                Token::Identifier("x"),
                Token::Comma,
                Token::Identifier("y"),
                Token::Colon,
                Token::Identifier("x"),
                Token::Plus,
                Token::Identifier("y"),
                Token::CloseCurly,
            ]
        );

        let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
        assert_eq!(
            parse_tree,
            pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Lambda(
                        vec!["x", "y"],
                        vec![pt::Statement::Return(Some(pt::Expr::Binary(
                            Box::new(pt::Expr::Id("x")),
                            pt::Operator::Plus,
                            Box::new(pt::Expr::Id("y")),
                        )))],
                    ))],
                }],
            }
        );

        let ast = to_ast::to_ast(parse_tree);
        assert_eq!(ast.functions.len(), 1);
        match &ast.functions[0].body[0] {
            ast::Statement::Expr(ast::Expr::Lambda(func)) => {
                assert_eq!(func.params.len(), 2);
                assert_eq!(func.return_type, ValueType::Indeterminate);
            }
            other => panic!("expected Lambda, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_no_args() {
        // lambda: 42
        let input_str = r"fn main() -> int { lambda: 42 }";

        let tokens = tokenize(input_str).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Lambda,
                Token::Colon,
                Token::Int(42),
                Token::CloseCurly,
            ]
        );

        let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
        assert_eq!(
            parse_tree,
            pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Lambda(
                        vec![],
                        vec![pt::Statement::Return(Some(pt::Expr::Int(42)))],
                    ))],
                }],
            }
        );

        let ast = to_ast::to_ast(parse_tree);
        match &ast.functions[0].body[0] {
            ast::Statement::Expr(ast::Expr::Lambda(func)) => {
                assert_eq!(func.params.len(), 0);
                assert_eq!(func.body.len(), 1);
            }
            other => panic!("expected Lambda, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_called() {
        // (lambda x: x)(5)
        let input_str = r"fn main() -> int { (lambda x: x)(5) }";

        let tokens = tokenize(input_str).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::OpenParen,
                Token::Lambda,
                Token::Identifier("x"),
                Token::Colon,
                Token::Identifier("x"),
                Token::CloseParen,
                Token::OpenParen,
                Token::Int(5),
                Token::CloseParen,
                Token::CloseCurly,
            ]
        );

        let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
        // Should parse as Call(Lambda(...), [Int(5)])
        match &parse_tree.functions[0].statements[0] {
            pt::Statement::Expr(pt::Expr::Call(callee, args)) => {
                match callee.as_ref() {
                    pt::Expr::Parens(inner) => match inner.as_ref() {
                        pt::Expr::Lambda(params, _body) => {
                            assert_eq!(*params, vec!["x"]);
                        }
                        other => panic!("expected Lambda inside Parens, got {:?}", other),
                    },
                    other => panic!("expected Parens as callee, got {:?}", other),
                }
                assert_eq!(args.len(), 1);
            }
            other => panic!("expected Call expression, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_as_function_arg() {
        // foo(lambda x: x + 1)
        let input_str = r"fn main() -> int { foo(lambda x: x + 1) }";

        let tokens = tokenize(input_str).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("foo"),
                Token::OpenParen,
                Token::Lambda,
                Token::Identifier("x"),
                Token::Colon,
                Token::Identifier("x"),
                Token::Plus,
                Token::Int(1),
                Token::CloseParen,
                Token::CloseCurly,
            ]
        );

        let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
        match &parse_tree.functions[0].statements[0] {
            pt::Statement::Expr(pt::Expr::Call(callee, args)) => {
                match callee.as_ref() {
                    pt::Expr::Id("foo") => {}
                    other => panic!("expected Id(\"foo\") as callee, got {:?}", other),
                }
                assert_eq!(args.len(), 1);
                match &args[0] {
                    pt::Expr::Lambda(params, body) => {
                        assert_eq!(*params, vec!["x"]);
                        assert_eq!(body.len(), 1);
                    }
                    other => panic!("expected Lambda arg, got {:?}", other),
                }
            }
            other => panic!("expected Call expression, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_assigned_to_var() {
        // f = lambda x: x
        let input_str = r"fn main() -> int { f = lambda x: x }";

        let tokens = tokenize(input_str).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("f"),
                Token::Equals,
                Token::Lambda,
                Token::Identifier("x"),
                Token::Colon,
                Token::Identifier("x"),
                Token::CloseCurly,
            ]
        );

        let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
        match &parse_tree.functions[0].statements[0] {
            pt::Statement::Assign("f", pt::Expr::Lambda(params, body), _) => {
                assert_eq!(*params, vec!["x"]);
                assert_eq!(body.len(), 1);
            }
            other => panic!("expected Assign with Lambda, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_in_tuple() {
        // (lambda x: x, lambda y: y)
        let input_str = r"fn main() -> int { (lambda x: x, lambda y: y) }";

        let tokens = tokenize(input_str).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::OpenParen,
                Token::Lambda,
                Token::Identifier("x"),
                Token::Colon,
                Token::Identifier("x"),
                Token::Comma,
                Token::Lambda,
                Token::Identifier("y"),
                Token::Colon,
                Token::Identifier("y"),
                Token::CloseParen,
                Token::CloseCurly,
            ]
        );

        let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
        match &parse_tree.functions[0].statements[0] {
            pt::Statement::Expr(pt::Expr::Tuple(elems)) => {
                assert_eq!(elems.len(), 2);
                match &elems[0] {
                    pt::Expr::Lambda(params, _) => assert_eq!(*params, vec!["x"]),
                    other => panic!("expected Lambda[0], got {:?}", other),
                }
                match &elems[1] {
                    pt::Expr::Lambda(params, _) => assert_eq!(*params, vec!["y"]),
                    other => panic!("expected Lambda[1], got {:?}", other),
                }
            }
            other => panic!("expected Tuple expression, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_in_ternary() {
        // true ? lambda x: x : lambda y: y
        let input_str = r"fn main() -> int { true ? lambda x: x : lambda y: y }";

        let tokens = tokenize(input_str).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Bool(true),
                Token::QuestionMark,
                Token::Lambda,
                Token::Identifier("x"),
                Token::Colon,
                Token::Identifier("x"),
                Token::Colon,
                Token::Lambda,
                Token::Identifier("y"),
                Token::Colon,
                Token::Identifier("y"),
                Token::CloseCurly,
            ]
        );

        let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
        match &parse_tree.functions[0].statements[0] {
            pt::Statement::Expr(pt::Expr::Ternary(cond, pos, neg)) => {
                match cond.as_ref() {
                    pt::Expr::Bool(true) => {}
                    other => panic!("expected Bool(true) cond, got {:?}", other),
                }
                match pos.as_ref() {
                    pt::Expr::Lambda(params, _) => assert_eq!(*params, vec!["x"]),
                    other => panic!("expected Lambda in pos branch, got {:?}", other),
                }
                match neg.as_ref() {
                    pt::Expr::Lambda(params, _) => assert_eq!(*params, vec!["y"]),
                    other => panic!("expected Lambda in neg branch, got {:?}", other),
                }
            }
            other => panic!("expected Ternary expression, got {:?}", other),
        }
    }

    #[test]
    fn test_lambda_with_multiple_args_passed_to_function() {
        // apply(lambda x, y: x + y, 1, 2)
        let input_str = r"fn main() -> int { apply(lambda x, y: x + y, 1, 2) }";

        let tokens = tokenize(input_str).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("main"),
                Token::OpenParen,
                Token::CloseParen,
                Token::RightArrow,
                Token::IntType,
                Token::OpenCurly,
                Token::Identifier("apply"),
                Token::OpenParen,
                Token::Lambda,
                Token::Identifier("x"),
                Token::Comma,
                Token::Identifier("y"),
                Token::Colon,
                Token::Identifier("x"),
                Token::Plus,
                Token::Identifier("y"),
                Token::Comma,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::CloseParen,
                Token::CloseCurly,
            ]
        );

        let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
        match &parse_tree.functions[0].statements[0] {
            pt::Statement::Expr(pt::Expr::Call(callee, args)) => {
                match callee.as_ref() {
                    pt::Expr::Id("apply") => {}
                    other => panic!("expected Id(\"apply\"), got {:?}", other),
                }
                assert_eq!(args.len(), 3, "expected 3 args: lambda, 1, 2");
                match &args[0] {
                    pt::Expr::Lambda(params, _) => {
                        assert_eq!(*params, vec!["x", "y"]);
                    }
                    other => panic!("expected Lambda as first arg, got {:?}", other),
                }
                assert_eq!(args[1], pt::Expr::Int(1));
                assert_eq!(args[2], pt::Expr::Int(2));
            }
            other => panic!("expected Call expression, got {:?}", other),
        }
    }
}
