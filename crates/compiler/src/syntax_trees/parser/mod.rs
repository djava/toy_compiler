use super::ast;
use nom_locate::LocatedSpan;
use peg::error::ParseError;

pub mod parse_tree;
pub mod to_ast;
mod tokenizer;

pub use tokenizer::{TokenValue, tokenize};

#[derive(Debug, Clone)]
pub enum ParserError<'a> {
    Tokenizer(nom::Err<nom::error::Error<LocatedSpan<&'a str>>>),
    ParseTree(ParseError<usize>, TokenValue<'a>),
}

pub fn parse<'a>(input: &'a str) -> Result<ast::Program, ParserError<'a>> {
    let tokens = tokenizer::tokenize(input).map_err(|e| ParserError::Tokenizer(e))?;
    let parse_tree = parse_tree::parse_tokens(&tokens.iter().map(|t| t.token).collect::<Vec<_>>())?;
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
        pub expected_tokens: Vec<TokenValue<'a>>,
        pub expected_parse_tree: parse_tree::Module<'a>,
        pub expected_ast: ast::Program,
    }

    impl ParserTestCase<'_> {
        pub fn run(self) {
            let tokens = tokenize(self.input_str)
                .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
                .unwrap();
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_neg_constant() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { -100 }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Int(-100),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_binop() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { 1 + 2 }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::Plus,
                TokenValue::Int(2),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_parens() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { 1 + (2 + 3) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::Plus,
                TokenValue::OpenParen,
                TokenValue::Int(2),
                TokenValue::Plus,
                TokenValue::Int(3),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_unaryop() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { -(1) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Minus,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };

        tc.run();
    }

    #[test]
    fn test_simple_assign() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x = 1000 + -21912983 }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::Equals,
                TokenValue::Int(1000),
                TokenValue::Plus,
                TokenValue::Int(-21912983),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_simple_arg() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { print(x) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("print"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_no_arg() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { oogabooga() }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("oogabooga"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_complex_arg() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { print((1 + (2 + 4)) - read_int()) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("print"),
                TokenValue::OpenParen,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::Plus,
                TokenValue::OpenParen,
                TokenValue::Int(2),
                TokenValue::Plus,
                TokenValue::Int(4),
                TokenValue::CloseParen,
                TokenValue::CloseParen,
                TokenValue::Minus,
                TokenValue::Identifier("read_int"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_call_multi_arg() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { print(x, y, 1, 3, 1000) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("print"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::Comma,
                TokenValue::Identifier("y"),
                TokenValue::Comma,
                TokenValue::Int(1),
                TokenValue::Comma,
                TokenValue::Int(3),
                TokenValue::Comma,
                TokenValue::Int(1000),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_assign_to_call() {
        let tc = ParserTestCase {
            input_str: "fn main() -> int { x = Fffoo(1) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::Equals,
                TokenValue::Identifier("Fffoo"),
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::Equals,
                TokenValue::Int(100),
                TokenValue::Newline,
                TokenValue::Identifier("print"),
                TokenValue::OpenParen,
                TokenValue::Int(1000),
                TokenValue::CloseParen,
                TokenValue::Newline,
                TokenValue::Identifier("whatevn"),
                TokenValue::Equals,
                TokenValue::Identifier("x"),
                TokenValue::Newline,
                TokenValue::Int(101010),
                TokenValue::Plus,
                TokenValue::Int(1001010),
                TokenValue::Newline,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };

        tc.run()
    }

    #[test]
    fn test_space_delimited_unary() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { 2 + - 5 }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Int(2),
                TokenValue::Plus,
                TokenValue::Minus,
                TokenValue::Int(5),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_bool_op_simple() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { false && true }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Bool(false),
                TokenValue::And,
                TokenValue::Bool(true),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_bool_op_complex() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { truefoofalse(!(false && true) || (false == true) >= (1 - false)) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("truefoofalse"),
                TokenValue::OpenParen,
                TokenValue::Not,
                TokenValue::OpenParen,
                TokenValue::Bool(false),
                TokenValue::And,
                TokenValue::Bool(true),
                TokenValue::CloseParen,
                TokenValue::Or,
                TokenValue::OpenParen,
                TokenValue::Bool(false),
                TokenValue::DoubleEquals,
                TokenValue::Bool(true),
                TokenValue::CloseParen,
                TokenValue::GreaterEquals,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::Minus,
                TokenValue::Bool(false),
                TokenValue::CloseParen,
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_ternary_simple() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { true ? 1 : 2 }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Bool(true),
                TokenValue::QuestionMark,
                TokenValue::Int(1),
                TokenValue::Colon,
                TokenValue::Int(2),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Bool(true),
                TokenValue::QuestionMark,
                TokenValue::Int(1),
                TokenValue::Colon,
                TokenValue::Bool(false),
                TokenValue::QuestionMark,
                TokenValue::Int(2),
                TokenValue::Colon,
                TokenValue::Int(3),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::Plus,
                TokenValue::Int(2),
                TokenValue::QuestionMark,
                TokenValue::Int(3),
                TokenValue::Colon,
                TokenValue::Int(4),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_simple() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if true { 1 } }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::If,
                TokenValue::Bool(true),
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Newline,
                TokenValue::If,
                TokenValue::Bool(true),
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::Else,
                TokenValue::OpenCurly,
                TokenValue::Int(2),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Newline,
                TokenValue::If,
                TokenValue::Bool(true),
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::Else,
                TokenValue::If,
                TokenValue::Bool(false),
                TokenValue::OpenCurly,
                TokenValue::Int(2),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::Else,
                TokenValue::OpenCurly,
                TokenValue::Int(3),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_with_complex_condition() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if x == 1 { print(x) } }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::If,
                TokenValue::Identifier("x"),
                TokenValue::DoubleEquals,
                TokenValue::Int(1),
                TokenValue::OpenCurly,
                TokenValue::Identifier("print"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Newline,
                TokenValue::If,
                TokenValue::Bool(true),
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::Equals,
                TokenValue::Int(1),
                TokenValue::Newline,
                TokenValue::Identifier("print"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_empty_body() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if true { } }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::If,
                TokenValue::Bool(true),
                TokenValue::OpenCurly,
                TokenValue::CloseCurly,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Newline,
                TokenValue::If,
                TokenValue::Identifier("x"),
                TokenValue::DoubleEquals,
                TokenValue::Int(1),
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::Else,
                TokenValue::If,
                TokenValue::Identifier("x"),
                TokenValue::DoubleEquals,
                TokenValue::Int(2),
                TokenValue::OpenCurly,
                TokenValue::Int(2),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::Else,
                TokenValue::If,
                TokenValue::Identifier("x"),
                TokenValue::DoubleEquals,
                TokenValue::Int(3),
                TokenValue::OpenCurly,
                TokenValue::Int(3),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::Else,
                TokenValue::If,
                TokenValue::Identifier("x"),
                TokenValue::DoubleEquals,
                TokenValue::Int(4),
                TokenValue::OpenCurly,
                TokenValue::Int(4),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::Else,
                TokenValue::OpenCurly,
                TokenValue::Int(5),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::If,
                TokenValue::Bool(true),
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
                TokenValue::Else,
                TokenValue::OpenCurly,
                TokenValue::Int(2),
                TokenValue::CloseCurly,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_if_else_if_else_single_line() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if true { 1 } else if false { 2 } else { 3 } }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::If,
                TokenValue::Bool(true),
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
                TokenValue::Else,
                TokenValue::If,
                TokenValue::Bool(false),
                TokenValue::OpenCurly,
                TokenValue::Int(2),
                TokenValue::CloseCurly,
                TokenValue::Else,
                TokenValue::OpenCurly,
                TokenValue::Int(3),
                TokenValue::CloseCurly,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_pair() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { (1, 2) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::Comma,
                TokenValue::Int(2),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_single_trailing_comma() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { (42,) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::OpenParen,
                TokenValue::Int(42),
                TokenValue::Comma,
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_assign() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x = (1 + 2, read_int(), true) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::Equals,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::Plus,
                TokenValue::Int(2),
                TokenValue::Comma,
                TokenValue::Identifier("read_int"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::Comma,
                TokenValue::Bool(true),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_nested() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { ((1, 2), (3, 4)) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::OpenParen,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::Comma,
                TokenValue::Int(2),
                TokenValue::CloseParen,
                TokenValue::Comma,
                TokenValue::OpenParen,
                TokenValue::Int(3),
                TokenValue::Comma,
                TokenValue::Int(4),
                TokenValue::CloseParen,
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_tuple_trailing_comma_multi() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { (1, 2,) }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::Comma,
                TokenValue::Int(2),
                TokenValue::Comma,
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("print"),
                TokenValue::OpenParen,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::Comma,
                TokenValue::Int(2),
                TokenValue::CloseParen,
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Id("x")),
                        Box::new(pt::Expr::Int(0)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(main_local!("x"))),
                        Box::new(ast::Expr::Constant(Value::I64(0))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_nonzero_index() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { myvar[2] }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("myvar"),
                TokenValue::OpenBracket,
                TokenValue::Int(2),
                TokenValue::CloseBracket,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Id("myvar")),
                        Box::new(pt::Expr::Int(2)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(main_local!("myvar"))),
                        Box::new(ast::Expr::Constant(Value::I64(2))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_negative_index() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[-1] }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(-1),
                TokenValue::CloseBracket,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Id("x")),
                        Box::new(pt::Expr::Int(-1)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(main_local!("x"))),
                        Box::new(ast::Expr::Constant(Value::I64(-1))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::OpenBracket,
                TokenValue::Int(1),
                TokenValue::CloseBracket,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Subscript(
                            Box::new(pt::Expr::Id("x")),
                            Box::new(pt::Expr::Int(0)),
                        )),
                        Box::new(pt::Expr::Int(1)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Subscript(
                            Box::new(ast::Expr::Id(main_local!("x"))),
                            Box::new(ast::Expr::Constant(Value::I64(0))),
                        )),
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("foo"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Subscript(
                        Box::new(pt::Expr::Call(Box::new(pt::Expr::Id("foo")), vec![])),
                        Box::new(pt::Expr::Int(0)),
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
                        Box::new(ast::Expr::Constant(Value::I64(0))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::Plus,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Binary(
                        Box::new(pt::Expr::Subscript(
                            Box::new(pt::Expr::Id("x")),
                            Box::new(pt::Expr::Int(0)),
                        )),
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
                            Box::new(ast::Expr::Constant(Value::I64(0))),
                        )),
                        BinaryOperator::Add,
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::OpenParen,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::Comma,
                TokenValue::Int(2),
                TokenValue::CloseParen,
                TokenValue::CloseParen,
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::CloseCurly,
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
                        Box::new(pt::Expr::Int(0)),
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
                        Box::new(ast::Expr::Constant(Value::I64(0))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::QuestionMark,
                TokenValue::Int(1),
                TokenValue::Colon,
                TokenValue::Int(2),
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Ternary(
                        Box::new(pt::Expr::Subscript(
                            Box::new(pt::Expr::Id("x")),
                            Box::new(pt::Expr::Int(0)),
                        )),
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
                            Box::new(ast::Expr::Constant(Value::I64(0))),
                        )),
                        Box::new(ast::Expr::Constant(Value::I64(1))),
                        Box::new(ast::Expr::Constant(Value::I64(2))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::Equals,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::SubscriptAssign(
                        pt::Expr::Id("x"),
                        pt::Expr::Int(0),
                        pt::Expr::Int(1),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::ComplexSubscript(ComplexSubscript {
                            container: ast::Expr::Id(main_local!("x")),
                            index: ast::Expr::Constant(Value::I64(0)),
                        }),
                        ast::Expr::Constant(Value::I64(1)),
                        None,
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_nonzero_index() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { tup[3] = 42 }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("tup"),
                TokenValue::OpenBracket,
                TokenValue::Int(3),
                TokenValue::CloseBracket,
                TokenValue::Equals,
                TokenValue::Int(42),
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::SubscriptAssign(
                        pt::Expr::Id("tup"),
                        pt::Expr::Int(3),
                        pt::Expr::Int(42),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::ComplexSubscript(ComplexSubscript {
                            container: ast::Expr::Id(main_local!("tup")),
                            index: ast::Expr::Constant(Value::I64(3)),
                        }),
                        ast::Expr::Constant(Value::I64(42)),
                        None,
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(1),
                TokenValue::CloseBracket,
                TokenValue::Equals,
                TokenValue::Identifier("y"),
                TokenValue::Plus,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::SubscriptAssign(
                        pt::Expr::Id("x"),
                        pt::Expr::Int(1),
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
                        AssignDest::ComplexSubscript(ComplexSubscript {
                            container: ast::Expr::Id(main_local!("x")),
                            index: ast::Expr::Constant(Value::I64(1)),
                        }),
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::Equals,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::Comma,
                TokenValue::Int(2),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::SubscriptAssign(
                        pt::Expr::Id("x"),
                        pt::Expr::Int(0),
                        pt::Expr::Tuple(vec![pt::Expr::Int(1), pt::Expr::Int(2)]),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::ComplexSubscript(ComplexSubscript {
                            container: ast::Expr::Id(main_local!("x")),
                            index: ast::Expr::Constant(Value::I64(0)),
                        }),
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_negative_index() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { x[-1] = 99 }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(-1),
                TokenValue::CloseBracket,
                TokenValue::Equals,
                TokenValue::Int(99),
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::SubscriptAssign(
                        pt::Expr::Id("x"),
                        pt::Expr::Int(-1),
                        pt::Expr::Int(99),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::ComplexSubscript(ComplexSubscript {
                            container: ast::Expr::Id(main_local!("x")),
                            index: ast::Expr::Constant(Value::I64(-1)),
                        }),
                        ast::Expr::Constant(Value::I64(99)),
                        None,
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_subscript_assign_in_if_body() {
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { if true { x[0] = 1 } }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::If,
                TokenValue::Bool(true),
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::Equals,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::If(
                        pt::Expr::Bool(true),
                        vec![pt::Statement::SubscriptAssign(
                            pt::Expr::Id("x"),
                            pt::Expr::Int(0),
                            pt::Expr::Int(1),
                        )],
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Conditional(
                        ast::Expr::Constant(Value::Bool(true)),
                        vec![ast::Statement::Assign(
                            AssignDest::ComplexSubscript(ComplexSubscript {
                                container: ast::Expr::Id(main_local!("x")),
                                index: ast::Expr::Constant(Value::I64(0)),
                            }),
                            ast::Expr::Constant(Value::I64(1)),
                            None,
                        )],
                        vec![],
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::Equals,
                TokenValue::Identifier("y"),
                TokenValue::OpenBracket,
                TokenValue::Int(1),
                TokenValue::CloseBracket,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::SubscriptAssign(
                        pt::Expr::Id("x"),
                        pt::Expr::Int(0),
                        pt::Expr::Subscript(
                            Box::new(pt::Expr::Id("y")),
                            Box::new(pt::Expr::Int(1)),
                        ),
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![ast::Statement::Assign(
                        AssignDest::ComplexSubscript(ComplexSubscript {
                            container: ast::Expr::Id(main_local!("x")),
                            index: ast::Expr::Constant(Value::I64(0)),
                        }),
                        ast::Expr::Subscript(
                            Box::new(ast::Expr::Id(main_local!("y"))),
                            Box::new(ast::Expr::Constant(Value::I64(1))),
                        ),
                        None,
                    )],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Newline,
                TokenValue::Identifier("x"),
                TokenValue::Equals,
                TokenValue::OpenParen,
                TokenValue::Int(1),
                TokenValue::Comma,
                TokenValue::Int(2),
                TokenValue::CloseParen,
                TokenValue::Newline,
                TokenValue::Identifier("x"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::Equals,
                TokenValue::Int(42),
                TokenValue::Newline,
                TokenValue::CloseCurly,
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
                        pt::Statement::SubscriptAssign(
                            pt::Expr::Id("x"),
                            pt::Expr::Int(0),
                            pt::Expr::Int(42),
                        ),
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
                            AssignDest::ComplexSubscript(ComplexSubscript {
                                container: ast::Expr::Id(main_local!("x")),
                                index: ast::Expr::Constant(Value::I64(0)),
                            }),
                            ast::Expr::Constant(Value::I64(42)),
                            None,
                        ),
                    ],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("add"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::Comma,
                TokenValue::Identifier("y"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::Plus,
                TokenValue::Identifier("y"),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("greet"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::OpenCurly,
                TokenValue::Identifier("print_int"),
                TokenValue::OpenParen,
                TokenValue::Int(42),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("negate"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Int(0),
                TokenValue::Minus,
                TokenValue::Identifier("x"),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("check"),
                TokenValue::OpenParen,
                TokenValue::Identifier("flag"),
                TokenValue::Colon,
                TokenValue::BoolType,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("id"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Return,
                TokenValue::Identifier("x"),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("donothing"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::OpenCurly,
                TokenValue::Return,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("double"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::Plus,
                TokenValue::Identifier("x"),
                TokenValue::CloseCurly,
                TokenValue::Newline,
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("print_int"),
                TokenValue::OpenParen,
                TokenValue::Identifier("double"),
                TokenValue::OpenParen,
                TokenValue::Int(21),
                TokenValue::CloseParen,
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("is_zero"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::BoolType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::DoubleEquals,
                TokenValue::Int(0),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_fn_multiline_body() {
        let tc = ParserTestCase {
            input_str: "fn main() -> int { x = 10\nprint_int(x)\nreturn x }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("x"),
                TokenValue::Equals,
                TokenValue::Int(10),
                TokenValue::Newline,
                TokenValue::Identifier("print_int"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::CloseParen,
                TokenValue::Newline,
                TokenValue::Return,
                TokenValue::Identifier("x"),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("square"),
                TokenValue::OpenParen,
                TokenValue::Identifier("n"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Return,
                TokenValue::Identifier("n"),
                TokenValue::Asterisk,
                TokenValue::Identifier("n"),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("first"),
                TokenValue::OpenParen,
                TokenValue::Identifier("p"),
                TokenValue::Colon,
                TokenValue::TupleType,
                TokenValue::Less,
                TokenValue::IntType,
                TokenValue::Comma,
                TokenValue::IntType,
                TokenValue::Greater,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("p"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::CloseCurly,
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
                        Box::new(pt::Expr::Int(0)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: first_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Id(t_local!("p", first_id.clone()))),
                        Box::new(ast::Expr::Constant(Value::I64(0))),
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::from([(
                        t_local!("p", first_id.clone()),
                        ValueType::TupleType(vec![ValueType::IntType, ValueType::IntType]),
                    )]),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("pair"),
                TokenValue::OpenParen,
                TokenValue::Identifier("a"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::Comma,
                TokenValue::Identifier("b"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::TupleType,
                TokenValue::Less,
                TokenValue::IntType,
                TokenValue::Comma,
                TokenValue::IntType,
                TokenValue::Greater,
                TokenValue::OpenCurly,
                TokenValue::OpenParen,
                TokenValue::Identifier("a"),
                TokenValue::Comma,
                TokenValue::Identifier("b"),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("mix"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::Comma,
                TokenValue::Identifier("p"),
                TokenValue::Colon,
                TokenValue::TupleType,
                TokenValue::Less,
                TokenValue::IntType,
                TokenValue::Comma,
                TokenValue::BoolType,
                TokenValue::Greater,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::BoolType,
                TokenValue::OpenCurly,
                TokenValue::Bool(true),
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("deep"),
                TokenValue::OpenParen,
                TokenValue::Identifier("t"),
                TokenValue::Colon,
                TokenValue::TupleType,
                TokenValue::Less,
                TokenValue::TupleType,
                TokenValue::Less,
                TokenValue::IntType,
                TokenValue::Comma,
                TokenValue::IntType,
                TokenValue::Greater,
                TokenValue::Comma,
                TokenValue::BoolType,
                TokenValue::Greater,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("t"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::CloseCurly,
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
                        Box::new(pt::Expr::Subscript(
                            Box::new(pt::Expr::Id("t")),
                            Box::new(pt::Expr::Int(0)),
                        )),
                        Box::new(pt::Expr::Int(0)),
                    ))],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: deep_id.clone(),
                    body: vec![ast::Statement::Expr(ast::Expr::Subscript(
                        Box::new(ast::Expr::Subscript(
                            Box::new(ast::Expr::Id(t_local!("t", deep_id.clone()))),
                            Box::new(ast::Expr::Constant(Value::I64(0))),
                        )),
                        Box::new(ast::Expr::Constant(Value::I64(0))),
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::OpenParen,
                TokenValue::Identifier("i"),
                TokenValue::DoubleEquals,
                TokenValue::Int(0),
                TokenValue::QuestionMark,
                TokenValue::Identifier("a"),
                TokenValue::Colon,
                TokenValue::Identifier("b"),
                TokenValue::CloseParen,
                TokenValue::OpenParen,
                TokenValue::Int(10),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("t"),
                TokenValue::OpenBracket,
                TokenValue::Int(0),
                TokenValue::CloseBracket,
                TokenValue::OpenParen,
                TokenValue::Int(10),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,

                    statements: vec![pt::Statement::Expr(pt::Expr::Call(
                        Box::new(pt::Expr::Subscript(
                            Box::new(pt::Expr::Id("t")),
                            Box::new(pt::Expr::Int(0)),
                        )),
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
                            Box::new(ast::Expr::Constant(Value::I64(0))),
                        )),
                        vec![ast::Expr::Constant(Value::I64(10))],
                    ))],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
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
                TokenValue::Fn,
                TokenValue::Identifier("wrap"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::IntType,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::TupleType,
                TokenValue::Less,
                TokenValue::TupleType,
                TokenValue::Less,
                TokenValue::IntType,
                TokenValue::Comma,
                TokenValue::IntType,
                TokenValue::Greater,
                TokenValue::Comma,
                TokenValue::BoolType,
                TokenValue::Greater,
                TokenValue::OpenCurly,
                TokenValue::OpenParen,
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::Comma,
                TokenValue::Identifier("x"),
                TokenValue::CloseParen,
                TokenValue::Comma,
                TokenValue::Bool(true),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_lambda_simple() {
        // lambda x: x + 1
        let input_str = r"fn main() -> int { lambda x: x + 1 }";

        let tokens = tokenize(input_str)
            .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Lambda,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::Identifier("x"),
                TokenValue::Plus,
                TokenValue::Int(1),
                TokenValue::CloseCurly,
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

        let tokens = tokenize(input_str)
            .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Lambda,
                TokenValue::Identifier("x"),
                TokenValue::Comma,
                TokenValue::Identifier("y"),
                TokenValue::Colon,
                TokenValue::Identifier("x"),
                TokenValue::Plus,
                TokenValue::Identifier("y"),
                TokenValue::CloseCurly,
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

        let tokens = tokenize(input_str)
            .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Lambda,
                TokenValue::Colon,
                TokenValue::Int(42),
                TokenValue::CloseCurly,
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

        let tokens = tokenize(input_str)
            .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::OpenParen,
                TokenValue::Lambda,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::Identifier("x"),
                TokenValue::CloseParen,
                TokenValue::OpenParen,
                TokenValue::Int(5),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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

        let tokens = tokenize(input_str)
            .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("foo"),
                TokenValue::OpenParen,
                TokenValue::Lambda,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::Identifier("x"),
                TokenValue::Plus,
                TokenValue::Int(1),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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

        let tokens = tokenize(input_str)
            .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("f"),
                TokenValue::Equals,
                TokenValue::Lambda,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::Identifier("x"),
                TokenValue::CloseCurly,
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

        let tokens = tokenize(input_str)
            .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::OpenParen,
                TokenValue::Lambda,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::Identifier("x"),
                TokenValue::Comma,
                TokenValue::Lambda,
                TokenValue::Identifier("y"),
                TokenValue::Colon,
                TokenValue::Identifier("y"),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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

        let tokens = tokenize(input_str)
            .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Bool(true),
                TokenValue::QuestionMark,
                TokenValue::Lambda,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::Identifier("x"),
                TokenValue::Colon,
                TokenValue::Lambda,
                TokenValue::Identifier("y"),
                TokenValue::Colon,
                TokenValue::Identifier("y"),
                TokenValue::CloseCurly,
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

        let tokens = tokenize(input_str)
            .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::Identifier("apply"),
                TokenValue::OpenParen,
                TokenValue::Lambda,
                TokenValue::Identifier("x"),
                TokenValue::Comma,
                TokenValue::Identifier("y"),
                TokenValue::Colon,
                TokenValue::Identifier("x"),
                TokenValue::Plus,
                TokenValue::Identifier("y"),
                TokenValue::Comma,
                TokenValue::Int(1),
                TokenValue::Comma,
                TokenValue::Int(2),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
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
                assert_eq!(args.get(1), Some(&pt::Expr::Int(1)));
                assert_eq!(args.get(2), Some(&pt::Expr::Int(2)));
            }
            other => panic!("expected Call expression, got {:?}", other),
        }
    }

    // ── For loop ─────────────────────────────────────────────────────────────

    #[test]
    fn test_for_simple_desugar() {
        // for (i = 0; i < 5; i = i + 1) { print_int(i) }
        // desugars to: i = 0; while i < 5 { print_int(i); i = i + 1 }
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { for (i = 0; i < 5; i = i + 1) { print_int(i) } }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::For,
                TokenValue::OpenParen,
                TokenValue::Identifier("i"),
                TokenValue::Equals,
                TokenValue::Int(0),
                TokenValue::Semicolon,
                TokenValue::Identifier("i"),
                TokenValue::Less,
                TokenValue::Int(5),
                TokenValue::Semicolon,
                TokenValue::Identifier("i"),
                TokenValue::Equals,
                TokenValue::Identifier("i"),
                TokenValue::Plus,
                TokenValue::Int(1),
                TokenValue::CloseParen,
                TokenValue::OpenCurly,
                TokenValue::Identifier("print_int"),
                TokenValue::OpenParen,
                TokenValue::Identifier("i"),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::For(
                        Box::new(pt::Statement::Assign("i", pt::Expr::Int(0), None)),
                        pt::Expr::Binary(
                            Box::new(pt::Expr::Id("i")),
                            pt::Operator::Less,
                            Box::new(pt::Expr::Int(5)),
                        ),
                        Box::new(pt::Statement::Assign(
                            "i",
                            pt::Expr::Binary(
                                Box::new(pt::Expr::Id("i")),
                                pt::Operator::Plus,
                                Box::new(pt::Expr::Int(1)),
                            ),
                            None,
                        )),
                        vec![pt::Statement::Expr(pt::Expr::Call(
                            Box::new(pt::Expr::Id("print_int")),
                            vec![pt::Expr::Id("i")],
                        ))],
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![
                        ast::Statement::Assign(
                            AssignDest::Id(main_local!("i")),
                            ast::Expr::Constant(Value::I64(0)),
                            None,
                        ),
                        ast::Statement::WhileLoop(
                            ast::Expr::BinaryOp(
                                Box::new(ast::Expr::Id(main_local!("i"))),
                                BinaryOperator::Less,
                                Box::new(ast::Expr::Constant(Value::I64(5))),
                            ),
                            vec![
                                ast::Statement::Expr(ast::Expr::Call(
                                    Box::new(ast::Expr::Id(main_local!("print_int"))),
                                    vec![ast::Expr::Id(main_local!("i"))],
                                )),
                                ast::Statement::Assign(
                                    AssignDest::Id(main_local!("i")),
                                    ast::Expr::BinaryOp(
                                        Box::new(ast::Expr::Id(main_local!("i"))),
                                        BinaryOperator::Add,
                                        Box::new(ast::Expr::Constant(Value::I64(1))),
                                    ),
                                    None,
                                ),
                            ],
                        ),
                    ],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_for_decrement() {
        // for (x = 3; x > 0; x = x - 1) { print_int(x) }
        let tc = ParserTestCase {
            input_str: r"fn main() -> int { for (x = 3; x > 0; x = x - 1) { print_int(x) } }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::RightArrow,
                TokenValue::IntType,
                TokenValue::OpenCurly,
                TokenValue::For,
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::Equals,
                TokenValue::Int(3),
                TokenValue::Semicolon,
                TokenValue::Identifier("x"),
                TokenValue::Greater,
                TokenValue::Int(0),
                TokenValue::Semicolon,
                TokenValue::Identifier("x"),
                TokenValue::Equals,
                TokenValue::Identifier("x"),
                TokenValue::Minus,
                TokenValue::Int(1),
                TokenValue::CloseParen,
                TokenValue::OpenCurly,
                TokenValue::Identifier("print_int"),
                TokenValue::OpenParen,
                TokenValue::Identifier("x"),
                TokenValue::CloseParen,
                TokenValue::CloseCurly,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::IntType,
                    statements: vec![pt::Statement::For(
                        Box::new(pt::Statement::Assign("x", pt::Expr::Int(3), None)),
                        pt::Expr::Binary(
                            Box::new(pt::Expr::Id("x")),
                            pt::Operator::Greater,
                            Box::new(pt::Expr::Int(0)),
                        ),
                        Box::new(pt::Statement::Assign(
                            "x",
                            pt::Expr::Binary(
                                Box::new(pt::Expr::Id("x")),
                                pt::Operator::Minus,
                                Box::new(pt::Expr::Int(1)),
                            ),
                            None,
                        )),
                        vec![pt::Statement::Expr(pt::Expr::Call(
                            Box::new(pt::Expr::Id("print_int")),
                            vec![pt::Expr::Id("x")],
                        ))],
                    )],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![
                        ast::Statement::Assign(
                            AssignDest::Id(main_local!("x")),
                            ast::Expr::Constant(Value::I64(3)),
                            None,
                        ),
                        ast::Statement::WhileLoop(
                            ast::Expr::BinaryOp(
                                Box::new(ast::Expr::Id(main_local!("x"))),
                                BinaryOperator::Greater,
                                Box::new(ast::Expr::Constant(Value::I64(0))),
                            ),
                            vec![
                                ast::Statement::Expr(ast::Expr::Call(
                                    Box::new(ast::Expr::Id(main_local!("print_int"))),
                                    vec![ast::Expr::Id(main_local!("x"))],
                                )),
                                ast::Statement::Assign(
                                    AssignDest::Id(main_local!("x")),
                                    ast::Expr::BinaryOp(
                                        Box::new(ast::Expr::Id(main_local!("x"))),
                                        BinaryOperator::Subtract,
                                        Box::new(ast::Expr::Constant(Value::I64(1))),
                                    ),
                                    None,
                                ),
                            ],
                        ),
                    ],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::IntType,
                }],
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }

    #[test]
    fn test_for_nested() {
        // for (i = 0; i < 2; i = i + 1) { for (j = 0; j < 2; j = j + 1) { print_int(j) } }
        // Just verify the parse tree structure — two nested For nodes
        let input_str = r"fn main() -> int { for (i = 0; i < 2; i = i + 1) { for (j = 0; j < 2; j = j + 1) { print_int(j) } } }";
        let tokens = tokenize(input_str)
            .map(|v| v.iter().map(|t| t.token).collect::<Vec<_>>())
            .unwrap();
        let parse_tree = parse_tree::parse_tokens(&tokens).unwrap();
        match &parse_tree.functions[0].statements[0] {
            pt::Statement::For(init, _cond, _incr, body) => {
                assert_eq!(**init, pt::Statement::Assign("i", pt::Expr::Int(0), None));
                assert_eq!(body.len(), 1);
                assert!(matches!(&body[0], pt::Statement::For(..)));
            }
            other => panic!("expected outer For, got {:?}", other),
        }
    }

    #[test]
    fn test_string() {
        let tc = ParserTestCase {
            input_str: "fn main() {
                x = \"abc\"
                y = \"bcdf\"
            }",
            expected_tokens: vec![
                TokenValue::Fn,
                TokenValue::Identifier("main"),
                TokenValue::OpenParen,
                TokenValue::CloseParen,
                TokenValue::OpenCurly,
                TokenValue::Newline,
                TokenValue::Identifier("x"),
                TokenValue::Equals,
                TokenValue::StringLiteral("abc"),
                TokenValue::Newline,
                TokenValue::Identifier("y"),
                TokenValue::Equals,
                TokenValue::StringLiteral("bcdf"),
                TokenValue::Newline,
                TokenValue::CloseCurly,
            ],
            expected_parse_tree: pt::Module {
                functions: vec![pt::Function {
                    name: "main",
                    params: vec![],
                    return_type: ValueType::NoneType,
                    statements: vec![
                        pt::Statement::Assign("x", pt::Expr::StringLiteral("abc"), None),
                        pt::Statement::Assign("y", pt::Expr::StringLiteral("bcdf"), None),
                    ],
                }],
            },
            expected_ast: ast::Program {
                functions: vec![ast::Function {
                    name: t_global!(LABEL_MAIN),
                    body: vec![
                        ast::Statement::Assign(
                            AssignDest::Id(main_local!("x")),
                            ast::Expr::Array(vec![
                                ast::Expr::Constant(Value::Char('a')),
                                ast::Expr::Constant(Value::Char('b')),
                                ast::Expr::Constant(Value::Char('c')),
                                ast::Expr::Constant(Value::Char('\0')),
                            ]),
                            None,
                        ),
                        ast::Statement::Assign(
                            AssignDest::Id(main_local!("y")),
                            ast::Expr::Array(vec![
                                ast::Expr::Constant(Value::Char('b')),
                                ast::Expr::Constant(Value::Char('c')),
                                ast::Expr::Constant(Value::Char('d')),
                                ast::Expr::Constant(Value::Char('f')),
                                ast::Expr::Constant(Value::Char('\0')),
                            ]),
                            None,
                        ),
                    ],
                    types: HashMap::new(),
                    params: IndexMap::new(),
                    return_type: ValueType::NoneType,
                }],
                global_types: HashMap::new(),
            },
        };
        tc.run();
    }
}
