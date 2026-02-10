pub use std::collections::VecDeque;

use test_support::{
    compiler::{syntax_trees::parser, pipeline::Pipeline},
    x86_interpreter::interpret_x86,
};

#[derive(Debug, Clone)]
pub struct TestCase<'a> {
    pub input: &'a str,
    pub inputs: VecDeque<i64>,
    pub expected_outputs: VecDeque<i64>,
}

pub fn execute_test_case(mut tc: TestCase) {
    println!("{}\n-------", tc.input);

    let ast = match parser::parse(tc.input) {
        Ok(ast) => ast,
        Err(e) => panic!("Parse error: {e:#?}"),
    };

    let no_opt_pipeline = Pipeline::make_no_opt();
    let no_opt_x86 = no_opt_pipeline.run(ast.clone());
    println!("-- No Opt: -- \n{}\n", no_opt_x86);

    let mut no_opt_tc = tc.clone();
    let mut no_opt_outputs = VecDeque::<i64>::new();
    interpret_x86(&no_opt_x86, &mut no_opt_tc.inputs, &mut no_opt_outputs);
    assert_eq!(no_opt_outputs, no_opt_tc.expected_outputs);

    let opt_pipeline = Pipeline::make_opt();
    let opt_x86 = opt_pipeline.run(ast);
    println!("-- With Opt: -- \n{}\n", opt_x86);

    let mut opt_outputs = VecDeque::<i64>::new();
    interpret_x86(&opt_x86, &mut tc.inputs, &mut opt_outputs);
    assert_eq!(opt_outputs, tc.expected_outputs, "Failed in optimized only");
}